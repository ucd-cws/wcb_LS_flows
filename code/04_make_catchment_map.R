
# Make Clean Catchment Map ------------------------------------------------

# Libraries ---------------------------------------------------------------

library(here)
library(sf)
library(glue)
library(tidyverse)
library(mapview)
mapviewOptions(fgb=FALSE, basemaps=c("Esri.WorldTopoMap", "Esri.WorldImagery",
                                           "OpenTopoMap", "OpenStreetMap",
                                           "CartoDB.Positron", "Stamen.TopOSMFeatures"))

# Database ----------------------------------------------------------------

# get DB path
db <- glue("{here()}/data/nhdplus_little_shasta.gpkg")
st_layers(db)

# get hucs
h10 <- read_sf(db, "h10_polys_ca") %>% st_transform(3310) %>% st_sf()
h12 <- read_sf(db, "h12_polys_ca") %>% st_transform(3310) %>% st_sf()
h10_ls <- read_sf(db, "lsh_huc10_final") %>% st_transform(3310) %>% st_sf()

# get clean streamline
flowlines <- read_sf(db, "lshasta_clean") %>% st_transform(3310)

# catchments: all
catch_ls <- read_sf(db, "catchments_ls_nhdplus18")
catch_ls_final <- read_sf(db, "lsh_catch_final_adj")

# stream network
nhd_flowlines <- st_read(db, "NHDFlowline_Network")
flowlines <- nhd_flowlines %>%
  filter(ftype=="StreamRiver") %>%
  # add a missing piece
  bind_rows(., nhd_flowlines %>% filter(comid==948010089))

flowlines <- st_transform(flowlines, 3310)[h10_ls,]

# springs
springs <- st_read("data/shps/DWR_Springs.shp") %>%
  st_transform(3310)
# crop by h10
springs <- springs[h10_ls,]
mapview(springs, zcol= "Discharge", layer.name="DWR Springs") +
  mapview(h10_ls, col.regions=NA, alpha.regions=0, color="darkblue", lwd=3)

# Plot --------------------------------------------------------------------

# plot
#png(filename = "figs/little_shasta_catch_map_w_huc10_boundary.png", width = 10, height = 8, units = "in", res = 300)

plot(h10_ls$geom, border = "slateblue", lwd=4)
plot(catch_ls_final$geom, col=alpha("maroon", 0.1), border = "orange", lwd = 0.8, add=T)
plot(catch_ls$geom, col=alpha("tan", 0.2), border = "black", lwd = 0.5, add=TRUE)
plot(flowlines$geom, col="dodgerblue", lwd=1.5, add=T)
#dev.off()

# add springs and streamlines ---------------------------------------------

evans <- st_read("data/Evans_Channel_Alignment_approx.kml") %>% st_transform(3310)

# springs
dwr_springs <- read_sf("data/shps/DWR_Springs.shp") %>% st_transform(3310)

## trim by watershed
lsh_springs <- st_intersection(dwr_springs, h10) %>%
  filter(Name %in% c("Evans Spring", "Cold Springs"))


# Add Gages ---------------------------------------------------------------

# USGS GAGE
gages_usgs <- read_rds(file = "data_output/gages_lshasta.rds") %>%
  mutate(site_number=as.character(site_number))

# LSR gage
gage_lsr <- st_as_sf(data.frame("site_longitude"=-122.350357, "site_latitude"=41.733093, "site_name"="LSR", "site_number"="LSR", "site_agency"="UCD"), coords=c("site_longitude", "site_latitude"), crs=4326, remove=FALSE)

# filter to just stations of interest
gages <- gages_usgs %>% filter(site_number %in% c("11517000","11516900")) %>%
  bind_rows(gage_lsr)


# Mapview -----------------------------------------------------------------

mapview(evans, color="steelblue") +
  mapview(lsh_springs, col.regions="skyblue", layer.name="Springs") +
  mapview(flowlines, zcol="streamorde", legend=FALSE) +
  mapview(gages, color="darkblue", layer.name="Gages")

# Final Map ---------------------------------------------------------------

library(tmap)
library(tmaptools)

# get raster data
tst1 <- stars::read_stars("data/lshasta_dem.tif")
#gm_osm <- read_osm(h10, type = "esri-topo", raster=TRUE)
#save(gm_osm, file = "data_output/tmaptools_h10_osm_natgeo.rda")
load("data_output/tmaptools_h10_osm_natgeo.rda")
tmap_options(max.raster = c(plot=1e6, view=1e6))

# add col for plotting lines
flowlines <- flowlines %>%
  mutate(streamorder_map = streamorde*2)

# make two versions of flowlines: mainstem and otherwise
flowlines_main <- flowlines %>% filter(streamorde>=3)
flowlines_tribs <- flowlines %>% filter(streamorde<3)


# first make CA map with no border
(map_base <-
    tm_shape(gm_osm) + tm_rgb() +
    tm_shape(tst1[1],) +
    tm_raster(palette="viridis", alpha = 0.5, title = "DEM (m)") +
    tm_shape(catch_ls_final) +
    tm_polygons(border.col="white", alpha = 0, border.alpha = 0.9, lwd=0.3, lty=2) +
    tm_shape(h10_ls) +
    tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
    tm_shape(flowlines) + tm_lines(col="darkblue", lwd="streamcalc", scale = 2.25, legend.lwd.show = FALSE) +
    tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +
    tm_shape(lsh_springs) +
    tm_dots(col="skyblue", size = 1, title = "Springs", legend.show = TRUE, shape = 21) +
    tm_shape(lsh_springs[c(1,2),]) +
    tm_text("Name", auto.placement = TRUE, just = "bottom", ymod = -1.5, xmod=1.5, shadow = TRUE)+
    tm_shape(gages) +
    tm_dots(col="darkblue", size = 1.2, title = "Gages", legend.show = TRUE, shape = 22) +
    tm_text("site_number", auto.placement = TRUE,
            just = "left", ymod = 1.5, xmod=1, shadow = TRUE)+

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

save(catch_ls_final, evans, gages, lsh_springs, h10_ls, flowlines, file = "data_output/little_shasta_catchment_flowlines.rda")
