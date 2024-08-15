library(tidyverse)
library(tidymodels)
library(yardstick)
library(vip)
library(glue)
library(xgboost)
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

df <- df |>
  select(-all_of(quitar))

df <- read.csv("Results/df_AGB.csv")

indep_vars <- colnames(df)[-1]

# Combination of predictive variables
# Generate all combinations of variables (excluding the empty set)
var_combinations <- combn(indep_vars, 
                          3, 
                          simplify = FALSE) 

# CV training----
fit_xgb <- function(df_training, df_cv, vars){
  
  # Specify algorithm to be used, engine and mode (classification or regression)
  xgb_model <- boost_tree() |> 
    set_mode("regression") |>
    set_engine("xgboost")
    
  
  formula_1 <- as.formula(paste(dep_var," ~ ", paste(vars, collapse = " + "))) 
  
  df_recipe <- recipe(formula_1, 
                      data = df_training) # |>
  # step_corr(all_numeric_predictors(),
  #           threshold = 0.8) |>
  # step_dummy(all_nominal(), -all_outcomes()) 
  
  fit_xgb <- workflow() |> 
    add_model(xgb_model) |>
    add_recipe(df_recipe) |>
    fit_resamples(resamples = df_cv,
                  control = control_resamples(save_pred = TRUE))
  
  # resuls_cv <- fit_xgb |>
  #   collect_predictions() |>
  #   rename("actual" = "AGB",
  #          "predicted" = ".pred")
  
  # Esto hace lo mismo que
  resul_metrics <- fit_xgb |>
    collect_metrics()
  
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
# Define the number of nodes to be used for the training procedure. I recommend using 1 less than the total number of cor
set.seed(5)
resuls <- furrr::future_map(var_combinations, 
                            .f = function(x) {
                              fit_xgb(df_training, df_cv, x)
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
#         paste0("Results/xgb_models.rds"))

resuls <- readRDS(paste0("Results/xgb_models.rds"))

eval_cv <- map(resuls, ~.x |>
                 pluck("resul_metrics")) |>
  bind_rows(.id = "id")

minrmse <- eval_cv |>
  filter(.metric == "rmse") |>
  slice_min(mean, n = 1)

maxrsq <- eval_cv |>
  filter(.metric == "rsq") |>
  slice_max(mean, n = 1)

best_xgb <- eval_cv |>
  filter((.metric == "rmse" & mean == minrmse$mean))

id_xgb <- best_xgb  |>
  pull(id)

# Entrenar

# Sacar vars
vars <- resuls[[as.numeric(id_xgb)]]$pred_vars

# Specify algorithm to be used, engine and mode (classification or regression)
set.seed(1)
xgb_model <- boost_tree() |> 
  set_mode("regression") |>
  set_engine("xgboost")

formula_1 <- as.formula(paste(dep_var," ~ ", paste(vars, collapse = " + "))) 

df_recipe <- recipe(formula_1, 
                    data = df_training) 

# Entrenar sobre training y evaluar sobre test
best_xgb_fv <- workflow() |> 
  add_model(xgb_model) |>
  add_recipe(df_recipe) 

# Hyperparams tuning----
# set.seed(15)
# xgb_grid <- grid_latin_hypercube(
#   tree_depth(range = c(2,5)),
#   mtry(range = c(1, 2)),
#   trees(range = c(100,500)),
#   min_n(range = c(3,5)),
#   learn_rate(range = c(-5,-4)),
#   size = 80
# )
# 
# xgb_model <- boost_tree(
#   tree_depth = tune(),
#   # loss_reduction = tune(),                    ## first three: model complexity
#   mtry = tune(),  
#   trees = tune(),
#         ## randomness
#   min_n = tune(),
#   learn_rate = tune()
# ) |>
#   set_mode("regression") |>
#   set_engine("xgboost")
# 
# xgb_tune <- workflow() |>
#   add_model(xgb_model) |>
#   add_recipe(df_recipe)
# 
# doParallel::registerDoParallel(cores = 7)
# 
# set.seed(123)
# tune_res <-
#   xgb_tune %>%
#   tune_grid(
#     resamples = df_cv,
#     grid = xgb_grid,
#     metrics = metric_set(rmse)
#   )
# 
# # parallel::stopCluster()
# 
# # finalize model set up
# final_xgb <- xgb_tune %>%
#   finalize_workflow(
#     show_best(x = tune_res,
#               metric = "rmse",
#               n = 1)
#   )
# 
# fit_xgb <- final_xgb |>
#   fit(data = df_training)

# Evaluación final----
# Evaluar

fit_xgb <- best_xgb_fv |>
  fit(data = df_training)

# Get actual and predicted
resuls_test <- tibble(actual = df_test |>
                        pull(dep_var),
                      predicted = predict(fit_xgb, df_test) |>
                        pull(.pred))
resuls_train <- tibble(actual = df_training |>
                         pull(dep_var),
                       predicted = predict(fit_xgb, df_training) |>
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
df_varimp <- vip::vi(fit_xgb|>
                       # Extract model from parsnip
                       extract_fit_parsnip(),
                     scale = TRUE,
                     decreasing = TRUE)

exp_df_xgb <- list(model = fit_xgb,
                  rmse_train = rmse_train,
                  r2_train = r2_train,
                  relrmse_train = relrmse_train,
                  rmse_test = rmse_test,
                  r2_test = r2_test,
                  relrmse_test = relrmse_test,
                  df_varimp = df_varimp,
                  cv_resul = best_xgb,
                  df_test = resuls_test,
                  df_train = resuls_train)

saveRDS(exp_df_xgb, paste0("Results/Lista_bestmodel_xgb_24.rds"))
