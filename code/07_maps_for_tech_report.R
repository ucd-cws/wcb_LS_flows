# Maps for Tech Report

# Libraries ---------------------------------------------------------------

library(sf)
library(dplyr)
library(readr)
#library(tidyverse)
library(fs)
library(here)
library(tmap)
library(tmaptools)
library(mapview)
mapviewOptions(fgb=FALSE, basemaps=c("Esri.WorldTopoMap", "Esri.WorldImagery",
                                     "OpenTopoMap", "OpenStreetMap",
                                     "CartoDB.Positron", "Stamen.TopOSMFeatures"))

# Data --------------------------------------------------------------------

# all data: catch_h10, evans, flowlines, h10, lsh_springs
load(here("data_output","little_shasta_catchment_flowlines.rda"))

# reduce fields
flowlines_map <- flowlines %>% select(id, comid, hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc)

# updated catchment areas # catch_final, df_catch_diss, df_da_final, df_coms (all attribs, n=142)
load(here("data_output/06_catcharea_final_adjust.rda"))
aoi_comid <- df_da_final %>% filter(comid %in% c(3917946, 3917950, 3917198))

# reorder factors
df_da_final$comid_f <- factor(as.character(df_da_final$comid),
                              levels=c(3917198, 3917200, 3917948,
                                       3917950, 3917244, 3917946))
# path to database:
db <- here("data/nhdplus_little_shasta.gpkg")
st_layers(db)

# original catchments
catch_orig <- st_read(db, "catchments_ls_nhdplus18", quiet = TRUE)

# LSR GAGE
gage_lsr <- st_as_sf(data.frame("lon"=-122.350357, "lat"=41.733093, "SiteName"="LSR", "SiteAgency"="UCD"), coords=c("lon", "lat"), crs=4326, remove=FALSE)

# other gage stations
gages_act <- read_csv("data/nwis_surface_water_active.csv") %>%
  st_as_sf(coords=c("SiteLongitude","SiteLatitude"), crs=4326, remove=FALSE)
gages_inact <- read_csv("data/nwis_surface_water_inactive.csv") %>%
  st_as_sf(coords=c("SiteLongitude","SiteLatitude"), crs=4326, remove=FALSE)

# preview
#mapview(gage_lsr) + mapview(gages_act, col.regions="green") + mapview(gages_inact, col.regions="yellow")

# filter to just stations of interest
gages <- gages_act %>% filter(SiteNumber=="11517000") %>% bind_rows(., filter(gages_inact, SiteNumber=="11516900")) %>% select(SiteNumber, SiteName, SiteAgency, lon=SiteLongitude, lat=SiteLatitude) %>%
  bind_rows(., gage_lsr)

# add name
gages <- gages %>%
  mutate(Name = c("SHA", "LSH", "LSR"))
#mapview(gages, col.regions="orange")

# get raster data
lsh_dem <- stars::read_stars("data/lshasta_dem.tif") # raster DEM
load("data_output/tmaptools_h10_osm_natgeo.rda") # topo base layer
tmap_options(max.raster = c(plot=1e6, view=1e6))

# Mapview Preview ---------------------------------------------------------

mapview(aoi_comid, color="coral1", lwd=4, layer.name="AOI Comids") +
  mapview(df_catch_diss, zcol="comid_f", alpha.regions=0.4, layer.name="Revised Catchments") +
  mapview(flowlines_map, color="cyan4", legend=F, lwd=0.5) +
  mapview(gages, col.regions="black", color="white", cex=5, layer.name="Gages") +
  mapview(catch_final, color="black", alpha.col=0.8, col.regions=NA, legend=FALSE, lwd=0.6) +
  mapview(evans, layer.name="Evans Streamline", color="cyan4", legend=FALSE) +
  mapview(lsh_springs,layer.name="Springs", col.regions="cyan4")

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
    tm_shape(catch_h10) +
    tm_polygons(border.col="white", alpha = 0, border.alpha = 0.9, lwd=0.3, lty=2) +
    # HUC10 boundary
    tm_shape(h10) +
    tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
    # flowlines
    tm_shape(flowlines_map) + tm_lines(col="darkblue", lwd="streamcalc", scale = 2.25, legend.lwd.show = FALSE) +
    tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +
    # springs
    tm_shape(lsh_springs) +
    tm_dots(col="skyblue", size = 1.2, title = "Springs", legend.show = TRUE, shape = 21) +
    tm_text("Name", auto.placement = 0.7, just = "left", xmod = 0.35, shadow = TRUE)+
    # gages
    tm_shape(gages) +
    tm_dots(col="darkblue", size = 1.2, title = "Gages", legend.show = TRUE, shape = 21) +
    tm_text("Name", auto.placement = 0.7, just = "left", xmod = 0.35, shadow = TRUE)+
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
   # subcatchments: all in white
   tm_shape(catch_final) +
   tm_polygons(border.col="white", alpha = 0, border.alpha = 0.9, lwd=0.3, lty=2) +
   # adjusted/revised catchments
   tm_shape(df_catch_diss) +
   tm_fill(col = "comid_f", alpha = 0.5, title = "Catchment COMID") +
   # HUC10 boundary
   tm_shape(h10) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
   # flowlines
   tm_shape(flowlines_map) + tm_lines(col="darkblue", lwd="streamcalc", scale = 2.25, legend.lwd.show = FALSE) +
   tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +
   # springs
   tm_shape(lsh_springs) +
   tm_dots(col="skyblue", size = 1.2, title = "Springs", legend.show = TRUE, shape = 21) +
   tm_text("Name", auto.placement = TRUE, xmod=0.4, just = "left", shadow = TRUE )+
   # gages
   tm_shape(gages) +
   tm_dots(col="darkblue", size = 1.2, title = "Gages", legend.show = TRUE, shape = 21) +
   tm_text("Name",col = "darkblue", size = 1, fontface = "bold",
           auto.placement = TRUE, just = "left", xmod=.6, ymod=1, shadow = TRUE)+
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
   # subcatchments: all in white
   tm_shape(catch_final) +
   tm_polygons(border.col="white", alpha = 0, border.alpha = 0.9, lwd=0.3, lty=2) +
   # adjusted/revised catchments
   tm_shape(df_catch_diss) +
   tm_fill(col = "comid_f", alpha = 0.5, title = "Catchment COMID") +
   # HUC10 boundary
   tm_shape(h10) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
   # flowlines
   tm_shape(flowlines_map) + tm_lines(col="darkblue", lwd="streamcalc", scale = 2.25, legend.lwd.show = FALSE) +
   tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +
   # AOI lines
   tm_shape(aoi_comid) + tm_lines(col="coral1", lwd=3.2) +
   # springs
   tm_shape(lsh_springs) +
   tm_dots(col="skyblue", size = 1.2, title = "Springs", legend.show = TRUE, shape = 21) +
   tm_text("Name", auto.placement = TRUE, xmod=0.4, just = "left", shadow = TRUE )+
   # gages
   tm_shape(gages) +
   tm_dots(col="darkblue", size = 1.2, title = "Gages", legend.show = TRUE, shape = 21) +
   tm_text("Name",col = "darkblue", size = 1, fontface = "bold",
           auto.placement = TRUE, just = "left", xmod=.6, ymod=1, shadow = TRUE)+
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
tmap_save(map_base, filename = "figs/map_of_h10_w_COMID_catch_w_AOIsegs.jpg", height = 8.5, width = 11, units = "in", dpi = 300)

