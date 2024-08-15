library(tidyverse)
library(tidymodels)
library(randomForest)
library(yardstick)
library(glue)
library(vip)
library(furrr)

tidymodels_prefer()

plan(multisession(workers = 11))

# Preprocesamiento----
# Leer datos y arreglar nombres de columnas
df <- read.csv("Data/BD_Joni_lidR_24.csv")
colnames(df)
colnames(df)[1:16] <- c("Plot","H", "SD_H", "H10perc", "H10top", "Cob", "AB", "AGB", "sdAGB", "IC2.5AGB", "IC97.5AGB","Unk.N","D", "Rich", "Simp", "Shan")

# Variables dependientes; Seleccionar solo AGB por el momento
dep_var <- "AGB"
dep_vars <- c("H", "SD_H", "H10perc", "H10top", "Cob", "AB", "AGB", "sdAGB", "IC2.5AGB", "IC97.5AGB","Unk.N", "D", "Rich", "Simp", "Shan")
rm_vars <- dep_vars[!dep_vars %in% dep_var]

# Quitar columnas extras
df <- df |>
  select(-all_of(c("Plot", rm_vars, "area","n"))) #|>
  # Transfomración logarítima
  # mutate(across(dep_var, ~ log10(.x)))

# Es un primer approach para no hacer todas las combinatorias y quitar cosas muy correlacionadas desde un principio
# Quizás se puede mejorar
indep_vars <- colnames(df)[-1]
quitar = indep_vars
removers = df
aux <- 1
sub_df<-df
# 
# while(nrow(removers)>=1){
#   
#   removers <- sub_df |>
#     select(-all_of(quitar[1:aux])) |>
#     corrr::correlate() |>
#     pivot_longer(cols = -term) |>
#     filter(value >= 0.8) 
#   
#   quitar <- removers |> 
#     distinct(term) |>
#     pull(term)
#   
#   aux <- aux+1
# }
# 
# df <- df |>
#   select(-all_of(quitar))

# write.csv(df ,
#           "Results/df_AGB.csv",
#           row.names = FALSE)

df <- read.csv("Results/df_AGB.csv")

indep_vars <- colnames(df)[-1]

# Combination of predictive variables
# Generate all combinations of variables (excluding the empty set)
var_combinations <- combn(indep_vars, 
                          3, 
                          simplify = FALSE) 

# CV training----
# Función para evaluar qué modelo es mejor solo con cv

fit_rf <- function(df_training, df_cv, vars){
  
  # Specify algorithm to be used, engine and mode (classification or regression)
  rf_model <- rand_forest() |>
    set_engine("randomForest") |>
    set_mode("regression")
  
  formula_1 <- as.formula(paste(dep_var," ~ ", paste(vars, collapse = " + "))) 
  
  df_recipe <- recipe(formula_1, 
                      data = df_training) # |>
    # step_corr(all_numeric_predictors(),
    #           threshold = 0.8) |>
    # step_dummy(all_nominal(), -all_outcomes()) 
  
  fit_rf <- workflow() |> 
    add_model(rf_model) |>
    add_recipe(df_recipe) |>
    fit_resamples(resamples = df_cv,
                  control = control_resamples(save_pred = TRUE, 
                                              save_workflow = TRUE))
  
  # resuls_cv <- fit_rf |>
  #   collect_predictions() |>
  #   rename("actual" = "AGB",
  #          "predicted" = ".pred")
  
  # Esto hace lo mismo que
  resul_metrics <- fit_rf |>
    collect_metrics()
  
  # Esto
  # fit_rf |>
  #   select(id, id2, .metrics) |>
  #   unnest(.metrics) |>
  #   group_by(.metric) |>
  #   summarise(mean = mean(.estimate))
  # 
  # fit_rf <- fit_rf |>
  #   fit_best()
  
  # 
  # return list
  temp <- list(vars, 
               resul_metrics)
  
  # names(temp) <- paste0(rep(glue("{dep_var}"), 11), c("_model", "_importance","_resul_cv", "_resul_train", "_resul_test", "_rmse_cv", "_rmse_train", "_rmse_test", "_r2_cv", "_r2_train", "_r2_test"))
  names(temp) <- c("pred_vars", "resul_metrics")
  temp
}

# Fit three types of models
set.seed(10)
sample_split <- initial_split(df, 
                              prop = 0.80)

# Create training and test set
df_training <- sample_split |>
  training()

df_test <- sample_split |>
  testing()

# Vfold cv
df_cv <- vfold_cv(df_training, 
                  v = 6,
                  repeats = 2)

# Parallel training----
# Define the number of nodes to be used for the training procedure. I recommend using 1 less than the total number of cores available in your computer
set.seed(5)
resuls <- furrr::future_map(var_combinations, 
                            .f = function(x) {
                              fit_rf(df_training, df_cv, x)
                            },
                            .options = furrr_options(packages = c("tidymodels", 
                                                                  "vip", 
                                                                  "yardstick", 
                                                                  "randomForest", 
                                                                  "tibble",
                                                                  "dplyr",
                                                                  "glue"),
                                                     seed = TRUE),
                            .progress = TRUE)

# Best model----
# Ya teniendo el mejor modelo, entrenarlo para sacar sus métricas de evaluación

# saveRDS(resuls,
#         paste0("Results/rf_models.rds"))

resuls <- readRDS(paste0("Results/rf_models.rds"))

eval_cv <- map(resuls, ~.x |>
  pluck("resul_metrics")) |>
  bind_rows(.id = "id")

minrmse <- eval_cv |>
  filter(.metric == "rmse") |>
  slice_min(mean, n = 1)

maxrsq <- eval_cv |>
  filter(.metric == "rsq") |>
  slice_max(mean, n = 1)

best_rf <- eval_cv |>
  filter((.metric == "rmse" & mean == minrmse$mean))

id_rf <- best_rf  |>
  pull(id)

# Entrenar

# Sacar vars
vars <- resuls[[as.numeric(id_rf)]]$pred_vars

# Specify algorithm to be used, engine and mode (classification or regression)
set.seed(1)
rf_model <- rand_forest() |> 
  set_mode("regression") |>
  set_engine("randomForest")

formula_1 <- as.formula(paste(dep_var," ~ ", paste(vars, collapse = " + "))) 


df_recipe <- recipe(formula_1,
                    data = df_training)

# Entrenar sobre training y evaluar sobre test
best_rf_pv <- workflow() |> 
  add_model(rf_model) |>
  add_recipe(df_recipe) 

# Hyperparams tuning----
# set.seed(15)
# rf_grid <- grid_latin_hypercube(
#   mtry(range = c(1,2),
#        trans = NULL),
#   trees(range = c(500,2000),
#         trans = NULL),
#   size = 80)
# 
# rf_model <- rand_forest(
#   mtry = tune(),
#   trees = tune()
# ) |>
#   set_mode("regression") |>
#   set_engine("randomForest",
#              num.threads = 7)
# 
# rf_tune <- workflow() |>
#   add_model(rf_model) |>
#   add_recipe(df_recipe)
# 
# doParallel::registerDoParallel(cores = 7)
# 
# set.seed(123)
# tune_res <-
#   rf_tune %>%
#   tune_grid(
#     resamples = df_cv,
#     grid = rf_grid,
#     metrics = metric_set(rmse)
#   )
# 
# # parallel::stopCluster()
# 
# # finalize model set up
# final_rf <- rf_tune %>%
#   finalize_workflow(
#     show_best(x = tune_res, metric = "rmse", n = 1)
#   )
# 
# fit_rf <- final_rf |>
#   fit(data = df_training)

# Evaluación final----
# Evaluar

fit_rf <- best_rf_pv |>
  fit(data = df_training)

# Get actual and predicted
resuls_test <- tibble(actual = df_test |>
                        pull(dep_var),
                      predicted = predict(fit_rf, df_test) |>
                        pull(.pred))
resuls_train <- tibble(actual = df_training |>
                         pull(dep_var),
                       predicted = predict(fit_rf, df_training) |>
                         pull(.pred))

# Calculate RMSE
# Train
rmse_train <- resuls_train |>
  rmse(actual, predicted)

r2_train <- resuls_train |>
  rsq_trad(actual, predicted)

relrmse_train <- resuls_train |>
  summarise(rRMSE = ehaGoF::gofRRMSE(actual, predicted))

# Test
rmse_test <- resuls_test |>
  rmse(actual, predicted)

r2_test <- resuls_test |>
  rsq_trad(actual, predicted)

relrmse_test <- resuls_test |>
  summarise(rRMSE = ehaGoF::gofRRMSE(actual, predicted))

# Variable of importance as table
df_varimp <- vip::vi(fit_rf|>
                       # Extract model from parsnip
                       extract_fit_parsnip(),
                     scale = TRUE,
                     decreasing = TRUE)

exp_df_rf <- list(model = fit_rf,
                  rmse_train = rmse_train,
                  r2_train = r2_train,
                  relrmse_train = relrmse_train,
                  rmse_test = rmse_test,
                  r2_test = r2_test,
                  relrmse_test = relrmse_test,
                  df_varimp = df_varimp,
                  cv_resul = best_rf,
                  df_test = resuls_test,
                  df_train = resuls_train)

saveRDS(exp_df_rf, paste0("Results/Lista_bestmodel_rf_24.rds"))
