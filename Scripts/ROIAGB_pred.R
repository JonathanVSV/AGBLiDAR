library(tidymodels)
library(tidyverse)
library(raster)

ims <- list.files("Raster/",
           ".tif$", full.names = TRUE) |>
  map(stack)

names(ims) <- NULL
ims$fun <- "mean"
ims$na.rm <- TRUE
im <- do.call(mosaic,
              ims)
names(im) <- c("zq35", "zmean", "p4th")

orig_im <- im
rf_model <- readRDS(paste0("Results/Lista_bestmodel_rf_24.rds"))

# Use model to make raster
# Function for applying model to rasters and obtain a raster as results
fun<-function(...){
  p<-predict(...)
  return(as.matrix(as.numeric(p[, 1, drop=T]))) 
}

# 1 sola pred----
# Make prediction
pred_im <- raster::predict(im, 
                           model = rf_model$model, 
                           fun=fun)
# Write raster
writeRaster(pred_im,
            paste0("AGB_pred/AGB_pred_rf_allroi.tif"),
            format = "GTiff",
            overwrite = T)

# Bootstrap split----

# Gerar intervalos de confianza
df <- read.csv(paste0("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLiDARrevML","/Results/df_AGB.csv"))
df_compl <- read.csv(paste0("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLiDARrevML","/Data/BD_Joni_lidR_24.csv"))
model_rf <- readRDS(paste0("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLiDARrevML","/Results/Lista_bestmodel_rf_24.rds"))

set.seed(27)
boots <- bootstraps(df, 
                    times = 100, 
                    apparent = TRUE)

df_boots <- map(1:nrow(boots), function(i){
  df <- boots$splits[[i]]$data
  in_id <- boots$splits[[i]]$in_id
  out_id <- which(!1:25 %in% boots$splits[[i]]$in_id)

  boot_models <- model_rf$model |>
    fit(data = df)
  
  return(boot_models)
})

# Use model to make raster
# Function for applying model to rasters and obtain a raster as results
fun<-function(...){
  p<-predict(...)
  return(as.matrix(as.numeric(p[, 1, drop=T]))) 
}

# 100 pred----
# Make prediction
walk(1:length(df_boots), function(i){
  pred_im <- raster::predict(im, 
                             model = df_boots[[i]], 
                             fun=fun)
  # Write raster
  writeRaster(pred_im,
              paste0("AGB_pred/AGB_pred_rf_allroi_",i,".tif"),
              format = "GTiff",
              overwrite = T)
})

# CV----
im_cv <- list.files(paste0("AGB_pred/"),
           ".tif$",
           full.names = TRUE) |>
  map(rast) 

im_resul <- rast(im_cv)

mean_resul <- app(im_resul, fun = mean)
sd_resul <- app(im_resul, fun = sd)
cv_resul <- 100 * sd_resul / mean_resul
plot(mean_resul)
plot(sd_resul)
plot(cv_resul)

# Enmascarar arbolado
mascara <- orig_im$zmean[orig_im$zmean>=1.5, drop = FALSE]
mascara <- rast(mascara)
mascara <- project(mascara, mean_resul)
mean_resul <- mask(mean_resul, mascara)
sd_resul <- mask(sd_resul, mascara)
cv_resul <- mask(cv_resul, mascara)

sum(values(mean_resul), na.rm = TRUE) / (10000/(25*25))
length(values(mean_resul)[!is.na(values(mean_resul)),])

writeRaster(mean_resul,
            "AGB_boots/mean_resul.tif",
            overwrite = TRUE)
writeRaster(sd_resul,
            "AGB_boots/sd_resul.tif",
            overwrite = TRUE)
writeRaster(cv_resul,
            "AGB_boots/cv_resul.tif",
            overwrite = TRUE)

