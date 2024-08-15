#library(rLiDAR)
library(lidR)
library(terra)
library(sf)
# library(rgdal)
library (dplyr)
# library(gdalUtils)
library(rgl)

#Load files
archivos<-list.files("CometaLidar/",pattern="*.las",full.names=T)
set_lidr_threads(11) 
archivos <- archivos
# data.table::setDTthreads(3) # for cran only

resuls <- purrr::walk(archivos, function(x){
  arch <- gsub(".las","",basename(x))
  lidares <- readLAS(x)
  st_crs(lidares)<-as.character("EPSG:4488")
  Veg <- filter_poi(lidares, Classification == 1)
  Suelo <- filter_poi(lidares,Classification == 2)
  DTM <- rasterize_terrain(Suelo,
                           res=25,
                           # USar tin en lugar de kriging para hacerlo más rápido
                           algorithm= tin(),
                           keep_lowest = F)
  Veg_Norm <- normalize_height(Veg,
                               DTM)
  Veg_Z <- pixel_metrics(Veg_Norm, 
                         ~list(zq35 = stats::quantile(Z, 0.35),
                               zmean = mean(Z),
                               p4th = as.numeric(stdmetrics_rn(ReturnNumber, class = NULL)[4])),
                         res = 25)
  writeRaster(Veg_Z,
              paste0("Raster/", arch,"_25m_rast.tif"),
              overwrite = TRUE)
}, .progress = TRUE)
