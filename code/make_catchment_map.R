
# Make Clean Catchment Map ------------------------------------------------

# https://jsta.github.io/nhdR
# https://github.com/mikejohnson51/HydroData


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
h10 <- read_sf(db, "h10_lshasta") %>% st_transform(3310) %>% st_sf()

# get clean streamline
flowlines <- read_sf(db, "lshasta_clean") %>% st_transform(3310)

# catchments
catch_ls <- read_sf(db, "catchments_ls_nhdplus18")

# stream network
nhd_flowlines <- st_read(db, "NHDFlowline_Network")
flowlines <- nhd_flowlines %>%
  filter(ftype=="StreamRiver") %>%
  # add a missing piece
  bind_rows(., nhd_flowlines %>% filter(comid==948010089))

flowlines <- st_transform(flowlines, 3310)[h10,]

# get and crop nhdplus catchments to H10
#catch <- st_read("~/Downloads/NHDPlusCA/NHDPlus18/NHDPlusCatchment/", "Catchment")
#catch <- st_transform(catch, 3310)
#catch_ls <- catch[h10,] # crop
# write out
# st_write(catch_ls, db, "catchments_ls_nhdplus18")

# Crop Catchments by H10 --------------------------------------------------

catch_h10 <- st_intersection(st_buffer(catch_ls, dist = 0), h10)

# Plot --------------------------------------------------------------------

# plot
plot(catch_h10$geom, col="tan", border = "white", lwd = 0.5)
plot(h10$geom, border = "slateblue", lwd=4, add=T)
plot(flowlines$geom, col="dodgerblue", lwd=1.5, add=T)


# Mapview -----------------------------------------------------------------

# mapview
# mapview(h10, col.regions=NA, alpha.regions=0, col="slateblue",lwd=3, legend=FALSE) +
#   mapview(catch_h10, color="gray90", alpha=0.4, alpha.regions=0.1, lwd=0.5) +
#   mapview(flowlines, color="dodgerblue", lwd=1.2)


# Final Map ---------------------------------------------------------------

library(tmap)
library(tmaptools)

# get raster data
tst1 <- stars::read_stars("data/lshasta_dem.tif")
gm_osm <- read_osm(h10, type = "esri-topo", raster=TRUE)
tmap_options(max.raster = c(plot=1e7, view=1e6))

# first make CA map with no border
(map_base <-
    tm_shape(gm_osm) + tm_rgb() +
    tm_shape(tst1[1],) +
    tm_raster(palette="viridis", alpha = 0.5, title = "DEM (m)") +
    tm_shape(catch_h10) +
    tm_polygons(border.col="white", alpha = 0, lwd=0.3) +
    tm_shape(h10) +
    tm_polygons(border.col="gray30", alpha = 0, lwd=2.5) +
    tm_shape(flowlines) + tm_lines(col="darkblue", lwd=3) +
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

