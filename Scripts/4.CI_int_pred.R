library(tidyverse)
library(tidymodels)


# Gerar intervalos de confianza
df <- read.csv(paste0("/Results/df_AGB.csv"))
df_compl <- read.csv(paste0("/Data/BD_Joni_lidR_24.csv"))
model_rf <- readRDS(paste0("/Results/Lista_bestmodel_rf_24.rds"))

# Bootstrap split----
set.seed(27)
boots <- bootstraps(df, 
                    times = 1000, 
                    apparent = TRUE)

boot_models <- model_rf$model|>
  fit_resamples(resamples = boots,
                control = control_resamples(save_pred = TRUE, 
                                            save_workflow = TRUE))

sits <- df |>
  left_join(df_compl |>
            select(Sitio, SumaAGB_ha),
            by = c("AGB" = "SumaAGB_ha")) |>
  select(Sitio)

# Por observación
icpred <- boot_models %>% 
  select(.predictions) |>
  unnest(.predictions) |>
  group_by(.row) |>
  summarise(mean = mean(.pred),
            sd =sd(.pred),
            .groups = "drop") |>
  bind_cols(sits) |>
  select(Sitio, mean, sd)


write.csv(icpred,
          paste0("Results/IC_bootstrap.csv"))

icpred |>
  mutate(ymin = mean - 1.96 * sd,
         ymax = mean + 1.96 * sd) |>
  ggplot(aes(x = mean, 
             y = mean,
             ymin = ymin,
             ymax = ymax)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "firebrick2") +
  geom_ribbon(alpha = 0.3) + 
  cowplot::theme_cowplot()

# De todo el modelos
percentile_intervals <- int_pctl(boot_models)
percentile_intervals