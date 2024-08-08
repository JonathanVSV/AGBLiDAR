library(tidyverse)
library(tidymodels)


# Gerar intervalos de confianza
df <- read.csv(paste0("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLiDARrevML","/Results/df_AGB.csv"))
df_compl <- read.csv(paste0("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLiDARrevML","/Data/BD_Joni_lidR_24.csv"))
model_rf <- readRDS(paste0("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLiDARrevML","/Results/Lista_bestmodel_rf_24.rds"))

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


# # Conformal split ----
# # Por el número de datos no alcanza a tener 90 % de CI, pero podemos sacar unos con 0.7
# 
# # Intervalo de confianza constante
# 
# df_con <- int_conformal_split(resuls[[1]]$AGB_model, df_test)
# 
# test_split_res <- predict(df_con, df_test, level = 0.7) %>% 
#   bind_cols(df_test)
# 
# # Intervalo de confianza variables
# 
# quant_int <- int_conformal_quantile(
#   resuls[[1]]$AGB_model, 
#   train_data = df_training,
#   cal_data = df_test, 
#   level = 0.70,
#   ntree = 2000)
# 
# test_quant_res <- predict(quant_int, df_test) |>
#   rowwise() |>
#   mutate(cvper = max(c(abs(.pred - .pred_lower), 
#                   abs(.pred - .pred_upper))) / .pred)
