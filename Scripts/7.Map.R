library(cowplot)
library(gridExtra)
library(tmap)
library(basemaps)
library(ceramic)
library(sf)
library(terra)
library(ggpubr)

# Map1----
splots <- st_read("Shape/27_Parcelas_rectangulos.shp") |>
  dplyr::filter(!parcela %in% c(9,26)) |>
  st_centroid()
cometa <- st_read("Shape/Cometa.gpkg") |>
  st_transform(3857) |>
  mutate(name = "Cometa\nLagoon") 
World <- st_read("WorldWithoutMX.shp") |>
  st_transform(3857)
Mx <- st_read("LimitesIntl/MX_inegi.shp")|>
  st_transform(3857)

roi <- splots |>
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf() |>
  st_buffer(1000) |>
  st_transform(3857)

splots <- splots |>
  st_transform(3857)

splots2 <- splots |>
  st_buffer(500)

# Download base map
# base_im <- basemap_geotif(roi,
#                map_service = "esri",
#                map_type = "world_imagery",
#                map_dir = "Basemap")

im1 <- rast("Basemap/basemap_20240723170715.357565.tif")
im2 <- rast("Basemap/basemap_20240723172752.482039.tif")

main_map <- tm_shape(im2,
         bbox = splots2 |>
           st_bbox()) +
  tm_rgb() +
  tm_shape(splots) + 
  tm_dots(col = "#FFFFFF",
          alpha = 0.95,
          shape = 21,
          border.col = "#000000",
          size = 0.15) +
  tm_shape(cometa) + 
  tm_dots(col = "#FFFFFF00",
          alpha = 0,
          shape = 21,
          border.col = "#FFFFFF00",
          border.alpha = 0,
          size = 0.15) +
  tm_text("name", 
          size = 0.8,
          alpha = 1,
          col = "skyblue",
          ymod = 0.1) +
  tm_graticules(n.x = 3,
                n.y = 3,
                projection = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(90,0),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.only = F,
            legend.outside = T,
            attr.outside = F,
            legend.outside.position = "right",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontface = "bold",
            fontfamily = "sans")  +
  tm_scale_bar(breaks = seq(0,1,0.5),
               position = c(0.45, -0.05),
               text.size = 0.8,
               text.color = "white",
               color.dark = "gray10",
               color.light = "white",
               just = "right",
               bg.color = "gray90",
               bg.alpha = 0)
main_map <- tmap_grob(main_map)

inset_map <- tm_shape(World,
                     bbox = Mx |>
                       st_bbox())+
  tm_polygons(col = "gray90") +
  tm_shape(Mx |>
             mutate(country = "Mexico")) +
  tm_polygons(col = "gray75") +
  tm_text("country", size = 0.6) +
  tm_shape(roi |>
             st_centroid()) +
  tm_dots(col = "firebrick2",
          size = 0.25) +
  tm_graticules(n.x = 3,
                n.y = 2,
                projection = "EPSG:4326",
                labels.show = T,
                labels.size = 0.6,
                labels.col = "#FFFFFF",
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(90,0),
                labels.cardinal = T,
                ticks = T,
                lines = F)

inset_map <- tmap_grob(inset_map)

# Create empty plot as base
p1 <- ggplot() +
  geom_blank() +
  theme_void()

texto <- text_grob(label = "WGS 84",
                   size = 8) 

legend_im <- tm_shape(splots |>
                        mutate(type = "Sampling plots")) +
  tm_dots(col = "type",
          title = "Legend",
          alpha = 0.95,
          size = 0.15,
          shape = 21,
          border.col = "#000000",
          palette = "#FFFFFF",
          legend.show = TRUE) +
  tm_layout(title = "Legend",
            legend.only = T,
            legend.outside = T,
            legend.text.size = 0.8,
            legend.title.color = "#FFFFFF",
            legend.text.color = "#FFFFFF",
            legend.outside.size = 0.5,
            attr.outside = F,
            legend.outside.position = "right",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1))  

legend_im <- tmap_grob(legend_im)

exp_plot <- ggdraw(p1) +
  draw_plot(main_map,
            x = -0.38,
            y = -0.25,
            hjust = 0,
            vjust = 0,
            width = 1.6,
            height = 1.30) +
  draw_plot(inset_map,
            x = 0.195,
            y = 0.68,
            hjust = 0,
            vjust = 0,
            width = 0.26,
            height = 0.26) +
  draw_plot(legend_im,
            x = 0.46,
            y = -0.37,
            hjust = 0,
            vjust = 0,
            width = 0.5,
            height = 0.8) +
  draw_plot(texto,
            x = 0.65,
            y = -0.01,
            hjust = 0,
            vjust = 0,
            width = 0.2,
            height = 0.2)

exp_plot

save_plot(exp_plot,
          # asp = 1.5,
          base_width = 20,
          base_height = 15,
          units = "cm",
          dpi = 300,
          filename = "Map/Map1.jpeg")

# Map2----
library(cowplot)
library(gridExtra)
library(tmap)
library(basemaps)
library(ceramic)
library(sf)
library(terra)
library(ggpubr)

mean_resul <- rast("AGB_boots/mean_resul.tif")
sd_resul <- rast("AGB_boots/sd_resul.tif")
cv_resul <- rast("AGB_boots/cv_resul.tif")

main_map <- tm_shape(mean_resul,
                     xlim = c(555000,558000),
                     ylim = c(2039500,2043000)) +
  tm_raster(style = 'cont',
            palette = scales::pal_brewer(palette = "Greens")(10),
            title = "mean AGB (Mg/ha)",
            legend.reverse = TRUE) +
  tm_graticules(n.x = 3,
                n.y = 3,
                projection = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(90,0),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.only = F,
            legend.outside = T,
            attr.outside = F,
            legend.outside.position = "right",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontface = "bold",
            fontfamily = "sans")  +
  tm_scale_bar(breaks = seq(0,1,0.5),
               position = c(0.42, -0.01),
               text.size = 0.5,
               text.color = "#000000",
               color.dark = "gray10",
               color.light = "white",
               just = "right",
               bg.color = "gray90",
               bg.alpha = 0)

main_map <- tmap_grob(main_map)

sec_map <- tm_shape(cv_resul,
                     xlim = c(555000,558000),
                     ylim = c(2039500,2043000)) +
  tm_raster(style = 'cont',
            palette = viridis::viridis_pal()(10),
            title = "CV AGB (%)",
            legend.reverse = TRUE) +
  tm_graticules(n.x = 3,
                n.y = 3,
                projection = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(90,0),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.only = F,
            legend.outside = T,
            attr.outside = F,
            legend.outside.position = "right",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontface = "bold",
            fontfamily = "sans")  +
  tm_scale_bar(breaks = seq(0,1,0.5),
               position = c(0.42, -0.01),
               text.size = 0.5,
               text.color = "#000000",
               color.dark = "gray10",
               color.light = "white",
               just = "right",
               bg.color = "gray90",
               bg.alpha = 0)
sec_map <- tmap_grob(sec_map)

# Create empty plot as base
p1 <- ggplot() +
  geom_blank() +
  theme_void()

texto <- text_grob(label = "WGS 84",
                   size = 8) 

exp_plot <- ggdraw(p1) +
  draw_plot(main_map,
            x = 0.05,
            y = -0.2,
            hjust = 0,
            vjust = 0,
            width = 0.48,
            height = 1.30) +
  draw_plot(sec_map,
            x = 0.52,
            y = -0.2,
            hjust = 0,
            vjust = 0,
            width = 0.48,
            height = 1.3) +
  draw_plot(texto,
            x = 0.8,
            y = 0.2,
            hjust = 0,
            vjust = 0,
            width = 0.2,
            height = 0.2)

exp_plot

save_plot(exp_plot,
          # asp = 1.5,
          base_width = 20,
          base_height = 15,
          units = "cm",
          dpi = 300,
          filename = "Map/Map2.jpeg")
