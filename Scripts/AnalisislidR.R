#library(rLiDAR)
library(lidR)
library(terra)
library(sf)
# library(rgdal)
library (dplyr)
# library(gdalUtils)
library(rgl)

#Load files
archivos<-list.files("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLidar/Cometa",pattern="*.las",full.names=T)
lidares<-readLAScatalog(archivos)
# plot(lidares)

# Check files
las_check(lidares)

#Define projection (view metadata)
# epsg<-make_EPSG()
# crsLas<-epsg %>%
#   filter(code == 4488)

st_crs(lidares)<-as.character("EPSG:4488")

#Load shapefiles
ptos<-st_read(paste0("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLidar/Shape/27_Parcelas_rectangulos.shp"))
ptos<-subset(ptos,parcela!=26)
ptos<-subset(ptos,parcela!=9)

parcelas<-ptos$parcela

#Reproject to same crs, vector
ptos<-st_transform(ptos,
                   st_crs(lidares))

buff<-st_read(paste0("D:/Drive/Jonathan_trabaggio/Doctorado/R/CometaLidar/Shape/Buff.shp"))

buff<-subset(buff,parcela!=26)
buff<-subset(buff,parcela!=9)

buff<-st_transform(buff,
                   st_crs(lidares))

#Classify points
#opt_output_files(lidares)<-"{ID}_ground"

#lasground(lidares,csf())
#Esto es sólo si hay duplicados
#lidares<-lasfilterduplicates(lidares)

#Clip
clipeados <- clip_roi(lidares, ptos)
clipeados2 <- clip_roi(lidares,buff)
#clipeados3<-lasclip(lidares,buff100)
#plot(clipeados[[1]])
#str(clipeados[[1]])

# Guardar snapshot
plot(clipeados[[1]], 
     # color = "ScanAngleRank", 
     bg = "white", 
     axis = TRUE, 
     legend = TRUE)
rgl.postscript("Plots/3dplot.pdf",
               fmt="pdf")

#Como los puntos ya están clasificados nada más utilizamos esa info

#Vegetación
Veg<-lapply(1:length(clipeados),
            function(i) filter_poi(clipeados[[i]], Classification == 1))
#plot(Veg[[1]])
parcelas<-sapply(1:length(clipeados),function(i) as.character(ptos$parcela[[i]]))

#Suelo
Suelo<-lapply(1:length(clipeados2),
              function(i) filter_poi(clipeados2[[i]],Classification == 2))

#Esto NO se hace pa Cometa
#Hay que filtrar los valores porque hay unos muy altos y bajos
#Suelo<-lapply(1:length(Suelo),
#              function(i) filter_poi(Suelo[[i]],Z >=90 & Z <=250))
#plot(Suelo[[60]])
DTM<-lapply(1:length(Suelo),
            function(i) rasterize_terrain(Suelo[[i]],
                                     res=0.5,
                                     algorithm=kriging(k=20),
                                     keep_lowest = F))
#plot(DTM[[60]])
plot(is.na(DTM[[5]]))

#Write rastersDTM
sapply(1:length(Suelo),
       function(i) writeRaster(DTM[[i]],
                                paste0("DTM_krig20_05m/DTM_",
                                       parcelas[i], ".grd"),
                               overwrite = TRUE))

archivos2<-lapply(list.files("DTM_krig20_05m",
                      pattern="*.grd$",
                      full.names=T),
                  rast)

mosaico <- do.call(mosaic, 
                   archivos2)

writeRaster(mosaico,
            "DEM_krig20_05m_lidR.tif",
            overwrite =TRUE)
crs(mosaico)

#Normalizar point cloud
Veg_Norm<-lapply(1:length(Veg),
                 function(i) normalize_height(Veg[[i]],
                                              DTM[[i]]))

# Canopy height model rasterized
# thr <- c(0,2,5,10,15)
# edg <- c(0, 1.5)
# CHM<-lapply(1:25,
#               function(i) grid_canopy(Veg_Norm[[i]],0.5, pitfree(thr, edg)))
# 
# sapply(1:25,
#        function(i) writeRaster(CHM[[i]],paste0("CHM_05m_pitfree/Rast_Parc",parcelas[i],".tif"),format = "GTiff",overwrite=T))
# 
# archivos2<-list.files("CHM_05m_pitfree",pattern="*.tif",full.names=T)
# mosaico<-mosaic(raster(archivos2[1]),
#                 raster(archivos2[2]),
#                 raster(archivos2[3]),
#                 raster(archivos2[4]),
#                 raster(archivos2[5]),
#                 raster(archivos2[6]),
#                 raster(archivos2[7]),
#                 raster(archivos2[8]),
#                 raster(archivos2[9]),
#                 raster(archivos2[10]),
#                 raster(archivos2[11]),
#                 raster(archivos2[12]),
#                 raster(archivos2[13]),
#                 raster(archivos2[14]),
#                 raster(archivos2[15]),
#                 raster(archivos2[16]),
#                 raster(archivos2[17]),
#                 raster(archivos2[18]),
#                 raster(archivos2[19]),
#                 raster(archivos2[20]),
#                 raster(archivos2[21]),
#                 raster(archivos2[22]),
#                 raster(archivos2[23]),
#                 raster(archivos2[24]),
#                 raster(archivos2[25]),
#                 fun=mean,
#                 na.rm=T)
# 
# writeRaster(mosaico,"CHM_05m_pitfree_lidR.tif",
#             format="GTiff",overwrite=T)

#Extract Metrics from Z
Veg_Z<-lapply(1:length(Veg_Norm),
              function(i) cloud_metrics(Veg_Norm[[i]], .stdmetrics))
resuls_Z<- Veg_Z |>
  setNames(ptos$parcela) |>
  bind_rows(.id = "Sitio") |>
  select(Sitio, everything()) |>
  mutate(across(Sitio, ~as.numeric(.x))) |>
  arrange(Sitio)

write.csv(resuls_Z,
          "MetricsZ.csv",
          row.names = F)

#Extract Metrics from I
Veg_I<-lapply(1:length(Veg),
              function(i) cloud_metrics(Veg_Norm[[i]], .stdmetrics_i))
resuls_I<- Veg_I |>
  setNames(ptos$parcela) |>
  bind_rows(.id = "Sitio") |>
  select(Sitio, everything()) |>
  mutate(across(Sitio, ~as.numeric(.x))) |>
  arrange(Sitio)

write.csv(resuls_I,
          "MetricsI.csv",
          row.names = F)
