library(magick)
library(cowplot)
library(gridExtra)
library(tmap)
library(basemaps)
library(ceramic)
library(sf)
library(terra)
library(ggpubr)

plib <- Sys.getenv("PROJ_LIB")
prj <- system.file("proj", package = "terra")[1]
Sys.setenv("PROJ_LIB" = prj)

# and perhaps set it back when done so that you can use Postgres
# Sys.setenv("PROJ_LIB" = plib)

# Map1----
splots <- st_read("Shape/27_Parcelas_rectangulos.shp") |>
  dplyr::filter(!parcela %in% c(9,26)) |>
  st_centroid() |>
  st_transform(32615)
cometa <- st_read("Shape/Cometa.gpkg") |>
  st_transform(32615) |>
  mutate(name = "Cometa\nLagoon") 
World <- st_read("LimitesIntl/WorldWithoutMX.shp") |>
  st_transform(6372)
Mx <- st_read("LimitesIntl/MX_inegi.shp")|>
  st_transform(6372)
roi2 <- st_read("Shp/roi_inset2.gpkg")|>
  # st_buffer(2500) |>
  st_transform(6372)
roilidar <- st_read("Shp/roi_lidar.gpkg")|>
  st_transform(32615)
LULC <- st_read("Shp/polys_2.shp") |>
  st_transform(32615)
golfo <- st_read("Shp/golforios.gpkg") |>
  st_transform(32615)
oceanos <- st_read("Shp/oceanos.gpkg") |>
  st_transform(32615) |>
  mutate(across(nombre, ~ifelse(.x == "Gulf of Mexico", "Gulf of\nMexico", .x)))
secchannel <- st_read("Shp/secchannel.gpkg") |>
  st_transform(32615)
river <- st_sf(st_sfc(
  sf::st_linestring(
    matrix(c(-10286740,
             -10292460, 
             2101449,
             2112993.68), 2)),
  crs = 3857))|>
  mutate(nombre = "San Pedro - San Pablo River") |>
  st_transform(32615)

library(raster)

ims <- list.files("Raster2/",
                  ".tif$", full.names = TRUE) |>
  purrr::map(stack)

names(ims) <- NULL
ims$fun <- "mean"
ims$na.rm <- TRUE
im <- do.call(mosaic,
              ims) |>
  raster::projectRaster(res = 10,
                        crs = "EPSG:32615")
names(im) <- c("zmax")

plot(im[[1]])

roi <- splots |>
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf() |>
  st_buffer(1000) |>
  st_transform(32615)

splots <- splots |>
  st_transform(32615)

splots2 <- splots |>
  st_buffer(500)

# Bajar mapa base
# base_im <- basemap_geotif(roi2,
#                map_service = "esri",
#                map_type = "world_imagery",
#                map_dir = "Basemap")

im1 <- rast("Basemap/basemap_20240723170715.357565.tif")
im2 <- rast("Basemap/basemap_20240723172752.482039.tif") |>
  project("EPSG:32615")
im3 <- rast("Basemap//basemap_20240812212358.276884.tif")

main_map <- tm_shape(im2,
         bbox = splots2 |>
           st_bbox()) +
  tm_rgb(stretch.palette = TRUE) +
  # tm_rgb(stretch.palette = TRUE) +
  tm_shape(im) +
  tm_raster(col = "zmax",
            palette = rev( RColorBrewer::brewer.pal(9,"Greys")),
            col.scale = tm_scale_continuous(),
            col.legend = tm_legend_hide()) +
  tm_legend_hide() +
  tm_shape(splots) + 
  tm_dots(col = "firebrick2",
          alpha = 0.95,
          shape = 21,
          border.col = "#000000",
          size = 0.5) +
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
          col = "#0ebcf4",
          ymod = 0.1,
          fontface = "bold",
          fontfamily = "sans",
          shadow = TRUE
          ) +
  tm_shape(secchannel) + 
  tm_lines(col = "#185cee",
           col.alpha = 0.9,
           border.col = "#185cee",
           border.alpha = 0.9,
           lwd = 2) +
  # tm_text("name", 
  #         size = 0.6,
  #         options = opt_tm_text(along_lines = TRUE),
  #         just = c("center","top"),
  #         alpha = 1,
  #         col = "#185cee",
  #         ymod = 0.1,
  #         fontfamily = "sans"
  # ) +
  tm_graticules(n.x = 3,
                n.y = 3,
                crs = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(0,90),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_scalebar(breaks = seq(0,1,0.5),
               position = c(-0.34, -0.20),
               text.size = 0.65,
               lwd = 1,
               text.color = "white",
               color.dark = "gray10",
               color.light = "white",
               # just = "right",
               bg.color = "gray90",
               bg.alpha = 0) +
  tm_compass(type = "arrow", 
             text.size = 1,
             size = 2,
             text.color = "white",
             position = c(-0.32, 1.35)) +
  tm_layout(legend.only = F,
            legend.show = F,
            legend.outside = F,
            attr.outside = F,
            legend.outside.position = "right",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontface = "bold",
            fontfamily = "sans",
            asp = 1,
            frame.lwd = 3)  
main_map <- tmap_grob(main_map)

inset_map <- tm_shape(World,
                     bbox = Mx |>
                       st_bbox())+
  tm_polygons(col = "gray90") +
  tm_shape(Mx |>
             mutate(country = "Mexico")) +
  tm_polygons(col = "gray75") +
  tm_text("country", 
          size = 0.6,
          fontfamily = "sans",
          col = "#000000") +
  tm_shape(roi |>
             st_centroid()) +
  tm_dots(col = "firebrick2",
          border.col = "firebrick2",
          size = 0.30) +
  tm_shape(oceanos) + 
  tm_dots(col = "#FFFFFF00",
          alpha = 0,
          shape = 21,
          border.col = "#FFFFFF00",
          border.alpha = 0,
          size = 0.15) +
  tm_text("nombre", 
          size = 0.6,
          alpha = 1,
          col = "#000000",
          ymod = 0.1,
          fontfamily = "sans"
  ) +
  tm_graticules(n.x = 3,
                n.y = 2,
                crs = "EPSG:4326",
                labels.show = T,
                labels.size = 0.6,
                labels.col = "#000000",
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(0,90),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontface = "bold",
            fontfamily = "sans",
            frame.lwd = 2) 

inset_map <- tmap_grob(inset_map)

inset_map2 <- tm_shape(LULC,
                       bbox = roi2 |>
                         st_buffer(-500) |>
                         st_bbox()) +
  tm_polygons(fill = "GRIDCODE",
              palette = c("#4ab21a", "#d3d604","#0ebcf4"),
          border.col = "#FFFFFF00",
          lwd = 0,
          style ="fixed", 
              breaks = c(-0.5,0.5,1.5,2),
          labels = c("Forested wetland", "Herbaceous wetland", "Water"),
          legend.show = FALSE) +
  # tm_rgb(alpha = 1,
  #        saturation = 1,
  #        stretch.palette = TRUE,
  #        title = "Mean height\n(m)",
  #        legend.reverse = TRUE) +
  tm_shape(roilidar) + 
  tm_borders(col = "firebrick2",
          lwd = 2) +
  tm_shape(golfo) + 
  tm_dots(col = "#FFFFFF00",
          alpha = 0,
          shape = 21,
          border.col = "#FFFFFF00",
          border.alpha = 0,
          size = 0.15) +
  tm_text("nombre", 
          size = 0.6,
          just = c("center","top"),
          alpha = 1,
          col = "#000000",
          ymod = 0.1,
          fontfamily = "sans"
  ) +
  tm_shape(river) + 
  tm_lines(col = "#FFFFFF00",
          alpha = 0,
          shape = 21,
          border.col = "#FFFFFF00",
          border.alpha = 0,
          size = 0.15) +
  tm_text("nombre", 
          size = 0.6,
          options = opt_tm_text(along_lines = TRUE),
          just = c("center","top"),
          alpha = 1,
          col = "#000000",
          ymod = 0.1,
          fontfamily = "sans"
  ) +
  tm_graticules(n.x = 3,
                n.y = 2,
                crs = "EPSG:4326",
                labels.show = T,
                labels.size = 0.6,
                labels.col = "#000000",
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(0,90),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.only = F,
            legend.outside = T,
            attr.outside = F,
            legend.outside.position = "none",
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontface = "bold",
            fontfamily = "sans",
            frame.lwd = 2)

inset_map2 <- tmap_grob(inset_map2)

# Create empty plot as base
p1 <- ggplot() +
  geom_blank() +
  theme_void()

texto <- text_grob(label = "WGS 84",
                   size = 8,
                   family = "sans") 

legend_im <- 
  tm_shape(splots |>
                        dplyr::mutate(Legend = "Sampling plots") |>
                        # dplyr::select(Legend) |>
                        dplyr::slice(1) |>
                        dplyr::ungroup()) +
  tm_dots(col = "Legend",
          fill_alpha = 0.95,
          title = "B",
          size = 0.5,
          shape = 21,
          border.col = "#000000",
          textNA= "",
          palette = c("firebrick2"),
          # fill.scale = tm_scale_discrete(values = "Sampling plots",
          #                                value.na = NA,
          #                                value.null = NA),
          fill.legend = tm_legend("Legend",
                                  orientation = "Landscape",
                                  show = TRUE,
                                  width = 20,
                                  na.show = F)) + # 
  tm_shape(secchannel |>
             mutate(`Water bodies` = "Secondary channel") |>
             dplyr::slice(1)) + 
  tm_lines(col = "Water bodies",
           # col.scale = "#185cee",
           col.scale = tm_scale_categorical(values = "#185cee",
                            label.na = ""),
           lwd = 2,
           col.legend = tm_legend(title = "", 
                                   # title.fontface = "normal",
                                   text.size = 0.8,
                                  # orientation = "Landscape",
                                  width = 20,
                                  na.show = FALSE,
                                  item.width = 0.95)
           # fill.legend = tm_legend(na.show = FALSE)
           ) +
  tm_layout(legend.only = T,
            legend.outside = T,
            legend.text.size = 0.8,
            legend.title.color = "#000000",
            legend.text.color = "#000000",
            legend.outside.size = 0.5,
            legend.frame = "#FFFFFF00",
            attr.outside = F,
            legend.outside.position = "right",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            text.fontfamily = "sans",
            legend.title.fontface = "bold",
            component.autoscale = FALSE)

legend_im <- tmap_grob(legend_im)

legend_im2 <- tm_shape(im) +
  tm_raster(col = "zmax",
            palette = rev( RColorBrewer::brewer.pal(9,"Greys")),
            title = "DSM (m a.m.s.l.)",
            style = "cont",
            breaks = seq(0,30,5),
            legend.reverse = FALSE,
            legend.show = TRUE,
            legend.is.portrait = FALSE)+
  tm_layout(legend.width = 15,
            legend.only = T,
            legend.outside = T,
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.title.color = "#000000",
            legend.text.color = "#000000",
            legend.frame = "#FFFFFF00",
            legend.outside.size = 0.6,
            attr.outside = F,
            legend.outside.position = "bottom",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontfamily = "sans",
            legend.title.fontface = "bold")
legend_im2 <- tmap_grob(legend_im2)

legend_im3 <- tm_shape(LULC,
                       bbox = roi2 |>
                         st_buffer(-500) |>
                         st_bbox()) +
  tm_fill(col = "GRIDCODE",
          title = "Land cover",
          palette = c("#4ab21a", "#d3d604","#0ebcf4"),
          style ="fixed", 
          breaks = c(-0.5,0.5,1.5,2),
          textNA= "",
          labels = c("Forested wetland", "Herbaceous wetland", "Water"),
          legend.show = TRUE,
          legend.is.portrait = T) +
  # tm_rgb(alpha = 1,
  #        saturation = 1,
  #        stretch.palette = TRUE,
  #        title = "Mean height\n(m)",
  #        legend.reverse = TRUE) +
  tm_layout(legend.only = T,
            legend.outside = T,
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.title.color = "#000000",
            legend.text.color = "#000000",
            legend.frame = "#FFFFFF00",
            legend.outside.size = 0.6,
            attr.outside = F,
            legend.outside.position = "bottom",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontfamily = "sans",
            legend.title.fontface = "bold")

legend_im3 <- tmap_grob(legend_im3)

legend_im4 <- tm_shape(roilidar |>
                         mutate(roi = "Study area")) + 
  tm_borders(col = "roi",
             col.scale = tm_scale_categorical(values = "firebrick2",
                                              label.na = ""),
             lwd = 2,
            col.legend = tm_legend(title = "",
                                   orientation = "portrait",
                                   text.size = 0.8,
                                   # orientation = "Landscape",
                                   width = 20,
                                   na.show = FALSE,
                                   item.width = 0.95)
             ) +
  # tm_rgb(alpha = 1,
  #        saturation = 1,
  #        stretch.palette = TRUE,
  #        title = "Mean height\n(m)",
  #        legend.reverse = TRUE) +
  tm_layout(legend.only = T,
            legend.outside = T,
            legend.title.size = 0.8,
            legend.text.size = 0.6,
            legend.title.color = "#000000",
            legend.text.color = "#000000",
            legend.frame = "#FFFFFF00",
            legend.outside.size = 0.6,
            attr.outside = F,
            legend.outside.position = "bottom",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontfamily = "sans",
            legend.title.fontface = "bold")

legend_im4 <- tmap_grob(legend_im4)

exp_plot <- ggdraw(p1) +
  draw_plot(main_map,
            x = -0.1,
            y = 0.0,
            hjust = 0,
            vjust = 0,
            width = 0.9,
            height = 1.1) +
  draw_plot(inset_map,
            x = 0.69,
            y = 0.72,
            hjust = 0,
            vjust = 0,
            width = 0.33,
            height = 0.31) +
  draw_plot(inset_map2,
            x = 0.45,
            y = 0.065,
            hjust = 0,
            vjust = 0,
            width = 0.80,
            height = 0.75) +
  # SAmpling plots
  draw_plot(legend_im,
            x = 0.02,
            y = -0.5,
            hjust = 0,
            vjust = 0,
            width = 0.5,
            height = 0.8) +
  # Height
  draw_plot(legend_im2,
            x = 0.30,
            y = -0.34,
            hjust = 0,
            vjust = 0,
            width = 0.1,
            height = 0.8) +
  # LULC
  draw_plot(legend_im3,
            x = 0.50,
            y = -0.55,
            hjust = 0,
            vjust = 0,
            width = 0.5,
            height = 0.9) +
  draw_plot(legend_im4,
            x = 0.72,
            y = -0.59,
            hjust = 0,
            vjust = 0,
            width = 0.5,
            height = 0.9) +
  draw_plot(texto,
            x = 0.815,
            y = -0.026,
            hjust = 0,
            vjust = 0,
            width = 0.2,
            height = 0.2)

# exp_plot

save_plot(exp_plot,
          # asp = 1.5,
          base_width = 20,
          base_height = 15,
          units = "cm",
          dpi = 300,
          filename = "Map/Map1.jpeg")

# Trim empty spaces
map1 <- image_read("Map/Map1.jpeg")
map1 <- image_trim(map1)
map1 <- image_border(map1, "#FFFFFF", "20x20")
image_write(map1, 
            path = "Map/Map1.jpeg", 
            format = "jpeg")

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

# main map 1, mean AGB
main_map <- tm_shape(mean_resul,
                     xlim = c(555000,558000),
                     ylim = c(2039500,2043000)) +
  tm_raster(style = 'cont',
            palette = scales::pal_brewer(palette = "Greens")(8),
            title = "mean AGB\n(Mg/ha)",
            legend.reverse = TRUE) +
  tm_graticules(n.x = 2,
                n.y = 3,
                crs = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(0,90),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.only = F,
            legend.outside = T,
            attr.outside = F,
            legend.outside.position = "right",
            legend.frame = "#FFFFFF",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontface = "bold",
            fontfamily = "sans",
            frame.lwd = 3)  +
  tm_scalebar(breaks = seq(0,1,0.5),
              position = c(-0.05, 0.03),
               text.size = 0.5,
               text.color = "#000000",
               color.dark = "gray10",
               color.light = "white",
               # just = "right",
               bg.color = "gray90",
               bg.alpha = 0) +
  tm_compass(type = "arrow", 
             text.size = 1,
             size = 1.5,
             position = c(-0.05, 1.05))

main_map <- tmap_grob(main_map)

sec_map <- tm_shape(cv_resul,
                     xlim = c(555000,558000),
                     ylim = c(2039500,2043000)) +
  tm_raster(style = 'cont',
            palette = scales::pal_brewer(palette = "Reds")(8),
            title = "CoV AGB\n(%)",
            legend.reverse = TRUE) +
  tm_graticules(n.x = 2,
                n.y = 3,
                crs = "EPSG:4326",
                labels.show = T,
                labels.size = 0.7,
                # labels.format = list(fun = function(x){
                #   degs <- floor(x)
                #   decs <- (x %% 1)
                #   mins <- floor(decs*60)
                #   paste0(degs, "°", mins, "\' ")}),
                labels.rot = c(0,90),
                labels.cardinal = T,
                ticks = T,
                lines = F) +
  tm_layout(legend.only = F,
            legend.outside = T,
            attr.outside = F,
            legend.outside.position = "right",
            legend.frame = "#FFFFFF",
            # legend.position = c(0.1,0.7),
            # attr.position = c(1.2, -0.05),
            between.margin = c(0),
            outer.margins = c(0.1),
            inner.margins = c(0.1),
            fontface = "bold",
            fontfamily = "sans",
            frame.lwd = 3)  +
  tm_scalebar(breaks = seq(0,1,0.5),
              position = c(-0.05, 0.03),
               text.size = 0.5,
               text.color = "#000000",
               color.dark = "gray10",
               color.light = "white",
               # just = "right",
               bg.color = "gray90",
               bg.alpha = 0) +
  tm_compass(type = "arrow", 
             text.size = 1,
             size = 1.5,
             position = c(-0.05, 1.05))
sec_map <- tmap_grob(sec_map)

# Create empty plot as base
p1 <- ggplot() +
  geom_blank() +
  theme_void()

texto <- text_grob(label = "WGS 84",
                   size = 8) 

exp_plot <- ggdraw(p1) +
  draw_plot(main_map,
            x = -0.015,
            y = -0.2,
            hjust = 0,
            vjust = 0,
            width = 0.55,
            height = 1.30) +
  draw_plot(sec_map,
            x = 0.46,
            y = -0.2,
            hjust = 0,
            vjust = 0,
            width = 0.55,
            height = 1.3) +
  draw_plot(texto,
            x = 0.81,
            y = 0.16,
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

# Trim empty spaces
map2 <- image_read("Map/Map2.jpeg")
map2 <- image_trim(map2)
map2 <- image_border(map2, "#FFFFFF", "20x20")
image_write(map2, 
            path = "Map/Map2.jpeg", 
            format = "jpeg")