library(sf)
library(readxl)
library(tidyverse)
library(mapview)
library(BIOMASS)
library(stringr)

# Read
df <- read.csv("Data/BD_Campo_New.csv") |>
  filter(Year != 2014 & Muerto.vivo == "V" & !Id.Sitio %in% c("Lidar 9", "Lidar 26")) |>
  mutate(id = row_number()) |>
  select(Id.Sitio, id, Parc.dim, Especie, Altura.directa, Densidad.Madera..Mg.m3., PAP1, PAP2, PAP3, PAP4, PAP5, PAP6, PAP7, PAP8, PAP9) |>
  # Quitar especies desconocidas porque luego fuerza los nombres
  mutate(across(Especie, ~ case_when(
    .x == "Cruzetillo" ~ NA,
    .x == "Hoja alterna" ~ NA,
    .x == "Corcho" ~ NA,
    .x == "Ciche, sp. 2 desconocido" ~ NA, 
    .default = .x))) |>
  mutate(Genero = str_extract(Especie, "^[A-z]+"),
         Especie_un = str_extract(Especie, "(?<=\\s)[A-z]+")) 

# Get taxonomic data and WD
taxo <- correctTaxo(genus = df$Genero, 
                    species = df$Especie_un, 
                    useCache = FALSE, 
                    verbose = FALSE)

df2 <- df |>
  mutate(genuscorr = taxo$genusCorrected,
         speciescorr = taxo$speciesCorrected) 

APG <- getTaxonomy(df2$genuscorr, findOrder = TRUE)

df2 <- df2 |>
  mutate(family = APG$family)

# Get average wood density for all the sampled individuals
# This value will be the one set as wood density for all the
# sampled trees in 2019
dataWD <- getWoodDensity(
  genus = df2$genuscorr,
  species = df2$speciescorr,
  family = df2$family,
  region = "World",
  stand = df2$parcela
)

df2 <- df2 |>
  mutate(meanWD = dataWD$meanWD,
         sdWD = dataWD$sdWD,
         levelWD = dataWD$levelWD) 

# Ignore NA warnings
df_agb <- df2 |>
  # Pasarlo a dap en cm
  mutate(across(matches("PAP[0-9]+$"), ~ .x / pi)) |>
  mutate(across(matches("PAP[0-9]+$"), ~ computeAGB(D = .x,
                                                    WD = meanWD,
                                                    H = Altura.directa))) |>
  select(id, matches("PAP[0-9]+$"))



df_agb_long <- df2 |>
  mutate(across(matches("PAP[0-9]+$"), ~ .x / pi)) |>
  pivot_longer(cols = starts_with("PAP"),
               names_to = "Var",
               names_prefix = "PAP",
               values_to = "Val") |>
  filter(Val != 0)

set.seed(10)
err_df <- map(unique(df_agb_long$Id.Sitio), function(x){
  df3 <- df_agb_long |>
    filter(Id.Sitio == x)

  # HDmodel <- modelHD(D = df3$Val,
  #                    H = df3$Altura.directa,
  #                    method = "weibull",
  #                    plot = FALSE)
  
  
  df_agb2 <- AGBmonteCarlo(D = df3$Val,
                           WD = df3$meanWD,
                           errWD = df3$sdWD,
                           H = df3$Altura.directa,
                           errH = 0,
                           Dpropag = "chave2004"
                           )
  tibble(meanAGB = df_agb2$meanAGB,
         sdAGB = df_agb2$sdAGB,
         cred25 = df_agb2$credibilityAGB[1],
         cred975 = df_agb2$credibilityAGB[2])
}) |>
  setNames(unique(df_agb_long$Id.Sitio)) |>
  bind_rows(.id = "Id.Sitio")

# # Salen warnings por NA en Diámetros
# colnames(df_agb) <- c("id", paste0(("AGB"), seq(1,9)))
# 
# df2 <- df2 |>
#   left_join(df_agb, "id")
# 
# # AGB
# AGB <- df2 |>
#   select(id, Id.Sitio, starts_with("AGB")) |>
#   pivot_longer(cols = -c(id, Id.Sitio), 
#                names_to = c("AGB")) |>
#   drop_na(value) |>
#   group_by(Id.Sitio) |>
#   summarise(AGB_sum = sum(value))

# # Listado por Id.Sitio
# df_exp <- df |>
#   select(Id.Sitio, Parc.dim) |>
#   group_by(Id.Sitio, Parc.dim) |>
#   slice_head(n=1) |>
#   left_join(AGB, by = "Id.Sitio") |>
#   select(Id.Sitio, Parc.dim, AGB_sum) |>
#   drop_na(AGB_sum) |>
#   arrange(AGB_sum) |>
#   mutate(across(AGB_sum, ~.x*10000/`Parc.dim`)) |>
#   arrange(AGB_sum)

# Con errores
df_exp <- df |>
  select(Id.Sitio, Parc.dim) |>
  group_by(Id.Sitio, Parc.dim) |>
  slice_head(n=1) |>
  ungroup() |>
  left_join(err_df, by = "Id.Sitio") |>
  left_join(df2 |> 
              filter(levelWD == "dataset") |>
              group_by(Id.Sitio) |>
              count() |>
              ungroup(), by = "Id.Sitio") |>
  rename("Unknown.species.inds" = "n") |>
  mutate(across(c(meanAGB, sdAGB, cred25, cred975), ~.x*10000/`Parc.dim`)) |>
  mutate(across(Id.Sitio, ~as.numeric(str_extract(.x, "[0-9]+")))) |>
  arrange(Id.Sitio) 

df_exp |>
  write.csv(paste0("Results/Cometa_AGB_errs_2024.csv"), row.names = F)
