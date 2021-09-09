# Maps for Tech Report

# Libraries ---------------------------------------------------------------

library(sf)
library(dplyr)
library(readr)
#library(tidyverse)
library(fs)
library(here)
library(glue)
library(tmap)
library(tmaptools)
library(mapview)
mapviewOptions(fgb=FALSE, basemaps=c("Esri.WorldTopoMap", "Esri.WorldImagery",
                                     "OpenTopoMap", "OpenStreetMap",
                                     "CartoDB.Positron", "Stamen.TopOSMFeatures"))

# Data --------------------------------------------------------------------

# all data: catch_h10, evans, flowlines, h10, lsh_springs
load(here("data_output","little_shasta_catchment_flowlines.rda"))

# path to database:
db <- glue("{here()}/data/nhdplus_little_shasta.gpkg")
# layers:
st_layers(db)

# reduce fields
flowlines_map <- flowlines %>% select(id, comid, hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc)

# updated catchment areas # catch_final, df_catch_diss, df_da_final, df_coms (all attribs, n=142)
load(here("data_output/06_catcharea_final_adjust.rda"))

loi_comid <- df_da_final %>% filter(comid %in% c(3917946, 3917950, 3917198))

# reorder factors
df_da_final$comid_f <- factor(as.character(df_da_final$comid),
                              levels=c(3917198, 3917200, 3917948,
                                       3917950, 3917244, 3917946))
# path to database:
db <- here("data/nhdplus_little_shasta.gpkg")
st_layers(db)

# original catchments
catch_orig <- st_read(db, "catchments_ls_nhdplus18", quiet = TRUE)

# Gages -------------------------------------------------------------------


# USGS GAGE
gages_usgs <- read_rds(file = "data_output/gages_lshasta.rds") %>%
   mutate(site_number=as.character(site_number))

# LSR gage
gage_lsr <- st_as_sf(data.frame("site_longitude"=-122.350357, "site_latitude"=41.733093, "site_name"="LSR", "site_number"="LSR", "site_agency"="UCD"), coords=c("site_longitude", "site_latitude"), crs=4326, remove=FALSE)

# filter to just stations of interest
gages <- gages_usgs %>% filter(site_number %in% c("11517000","11516900")) %>%
   bind_rows(gage_lsr)


# Raster Data -------------------------------------------------------------


# get raster data
lsh_dem <- stars::read_stars("data/lshasta_dem.tif") # raster DEM
load("data_output/tmaptools_h10_osm_natgeo.rda") # topo base layer
tmap_options(max.raster = c(plot=1e6, view=1e6))

# Mapview Preview ---------------------------------------------------------

mapview(loi_comid, color="coral1", lwd=4, layer.name="LOI Comids") +
  mapview(df_catch_diss, zcol="comid_f", alpha.regions=0.4, layer.name="Revised Catchments") +
  mapview(flowlines_map, color="darkblue", legend=F, lwd=2) +
  mapview(gages, col.regions="black", color="white", cex=5, layer.name="Gages") +
  mapview(catch_final, color=scales::alpha("black",0.2), alpha.regions=0.2, col.regions=NA, legend=FALSE, lwd=0.2) +
  mapview(evans, layer.name="Evans Streamline", color="darkblue", lwd=1, legend=FALSE) +
  mapview(lsh_springs,layer.name="Springs", col.regions="cyan4")+
   mapview(h10_ls, col.regions="maroon", alpha.regions=0, color="maroon", lwd=3,
           layer.name="HUC10 Revised", legend=TRUE)

# Static Map --------------------------------------------------------------

## GOT WEIRD ERROR, see here: https://github.com/mtennekes/tmap/issues/551
## updating to dev version which
# remotes::install_github("mtennekes/tmap")
# remotes::install_github("mtennekes/tmaptools")


## DEM MAP -----------------------------------------------------------------

# LShasta map with DEM
(map_base <-
    # baselayer
    tm_shape(gm_osm) + tm_rgb() +
    # DEM raster
    tm_shape(lsh_dem[1],) + tm_raster(palette="viridis", alpha = 0.5, title = "DEM (m)") +
    # subcatchments
    tm_shape(catch_final) +
    tm_polygons(border.col="white", alpha = 0, border.alpha = 0.9, lwd=0.3, lty=2) +
    # HUC10 boundary
    tm_shape(h10_ls) +
    tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
    # flowlines
    tm_shape(flowlines_map) + tm_lines(col="darkblue", lwd="streamcalc", scale = 2.25, legend.lwd.show = FALSE) +
    tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +

    # springs
    tm_shape(lsh_springs) +
    tm_dots(col="skyblue", size = 1, title = "Springs", legend.show = TRUE, shape = 21) +
    tm_shape(lsh_springs[c(1,2),]) +
    tm_text("Name", auto.placement = TRUE, just = "bottom", ymod = -1.5, xmod=1.5, shadow = TRUE)+
    # gages
    tm_shape(gages) +
    tm_dots(col="darkblue", size = 1.2, title = "Gages", legend.show = TRUE, shape = 22) +
    tm_text("site_number", fontface = "bold", auto.placement = TRUE,
            just = "left", ymod = 1.5, xmod=0.6, shadow = TRUE)+

    # layout
    tm_layout(frame=FALSE) +
    tm_layout(title = "Little Shasta",
              frame = FALSE,
              fontfamily = "Roboto Condensed",
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              #legend.position = c(0.6,0.85),
              title.position = c(0.7, 0.95)) +
    tm_compass(type = "4star", position = c("left","bottom")) +
    tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_base, filename = "figs/map_of_h10_w_dem_overlay.jpg", height = 8.5, width = 11, units = "in", dpi = 300)


# BASE LAYER CATCH MAP ----------------------------------------------------

# LShasta map with DEM
(map_base <-
   # baselayer
   tm_shape(gm_osm) + tm_rgb() +
   # adjusted/revised catchments
   tm_shape(df_catch_diss) +
   tm_fill(col = "comid_f", alpha = 0.5, title = "Catchment COMID") +
    # subcatchments: all in white
    tm_shape(catch_final) +
    tm_polygons(border.col="gray10", alpha = 0, border.alpha = 0.9, lwd=0.3, lty=2) +
    # HUC10 boundary
   tm_shape(h10_ls) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
   # flowlines
   tm_shape(flowlines_map) + tm_lines(col="darkblue", lwd="streamcalc", scale = 4, legend.lwd.show = FALSE) +
   tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +
    # springs
    tm_shape(lsh_springs) +
    tm_dots(col="skyblue", size = 1, title = "Springs", legend.show = TRUE, shape = 21) +
    tm_shape(lsh_springs[c(1,2),]) +
    tm_text("Name", auto.placement = TRUE, just = "bottom", ymod = -1, xmod=2, shadow = TRUE)+
    # gages
    tm_shape(gages) +
    tm_dots(col="darkblue", size = 1.2, title = "Gages", legend.show = TRUE, shape = 22) +
    tm_text("site_number", fontface = "bold", auto.placement = TRUE,
            just = "left", ymod = 1.8, xmod=0.6, shadow = TRUE)+

   # layout
   tm_layout(frame=FALSE) +
   tm_layout(title = "Little Shasta",
             frame = FALSE,
             fontfamily = "Roboto Condensed",
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             #legend.position = c(0.6,0.85),
             title.position = c(0.7, 0.95)) +
   tm_compass(type = "4star", position = c("left","bottom")) +
   tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_base, filename = "figs/map_of_h10_w_COMID_catch_revised.jpg", height = 8.5, width = 11, units = "in", dpi = 300)

# BASE LAYER AOI MAP ----------------------------------------------------

# LShasta map with DEM
(map_base <-
   # baselayer
   tm_shape(gm_osm) + tm_rgb() +
   # adjusted/revised catchments
   tm_shape(df_catch_diss) +
   tm_fill(col = "comid_f", alpha = 0.5, title = "Catchment COMID") +
    # subcatchments: all in white
    tm_shape(catch_final) +
    tm_polygons(border.col="gray10", alpha = 0, border.alpha = 0.9, lwd=0.3, lty=2) +
    # HUC10 boundary
   tm_shape(h10_ls) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
   # flowlines
   tm_shape(flowlines_map) + tm_lines(col="darkblue", lwd="streamcalc", scale = 4, legend.lwd.show = FALSE) +
   tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +
    # springs
    tm_shape(lsh_springs) +
    tm_dots(col="skyblue", size = 1, title = "Springs", legend.show = TRUE, shape = 21) +
    tm_shape(lsh_springs[c(1,2),]) +
    tm_text("Name", auto.placement = TRUE, just = "bottom", ymod = -1, xmod=2, shadow = TRUE)+
    # gages
    tm_shape(gages) +
    tm_dots(col="darkblue", size = 0.8, title = "Gages", legend.show = TRUE, shape = 22) +
    tm_text("site_number", fontface = "bold", auto.placement = TRUE,
            just = "left", ymod = 1.8, xmod=0.6, shadow = TRUE) +
    # AOI lines
    tm_shape(loi_comid) + tm_lines(col="coral1", lwd=4) +

   # layout
   tm_layout(frame=FALSE) +
   tm_layout(title = "Little Shasta",
             frame = FALSE,
             fontfamily = "Roboto Condensed",
             legend.outside = FALSE, attr.outside = FALSE,
             inner.margins = 0.01, outer.margins = (0.01),
             #legend.position = c(0.6,0.85),
             title.position = c(0.7, 0.95)) +
   tm_compass(type = "4star", position = c("left","bottom")) +
   tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_base, filename = "figs/map_of_h10_w_COMID_catch_w_LOIsegs.jpg", height = 8.5, width = 11, units = "in", dpi = 300)

