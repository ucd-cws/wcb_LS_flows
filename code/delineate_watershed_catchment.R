# delineate watershed

# see here: https://streamstats.usgs.gov/ss/

# Library -----------------------------------------------------------------

#devtools::install_github("markwh/streamstats")
library(streamstats)
library(sf)
library(tidyverse)
library(glue)
library(here)
library(mapview)
mapviewOptions(fgb = FALSE)

# Get DataSet -------------------------------------------------------------

# gages
gages<- read_rds(file = "data_output/gages_lshasta.rds")

# path to database:
db <- glue("{here()}/data/nhdplus_little_shasta.gpkg")

# layers:
st_layers(db)

# cleaned little shasta streamlines
lshasta_clean <- read_sf(db, "lshasta_clean") %>% st_transform(3310)
st_geometry_type(lshasta_clean)
# make a single linestring version:
lshasta_linestring <- st_cast(lshasta_clean, "LINESTRING")

# cleaned catchments
lshasta_catch <- read_sf(db, "lshasta_catchments_clean") %>% st_transform(3310)

nhd_wb <- read_sf(db, "NHDWaterBody") %>% st_transform(3310)
h10 <- read_sf(db, "h10_lshasta") %>% st_transform(3310)
h12 <- read_sf(db, "h12_lshasta") %>% st_transform(3310)

# get streamline end point:
outpt <- st_line_sample(lshasta_linestring %>% filter(comid=="3917946"), sample = 1)

mapview(outpt) + mapview(lshasta_linestring)

# Quick Mapview -----------------------------------------------------------

# quick preview maps
mapview(h12, alpha.regions=0.1, color="cyan", layer.name="H12") +
  mapview(lshasta_clean, layer.name="Streamlines", color="darkblue") +
  mapview(h10, alpha.regions=0.2, color="skyblue", col.regions=NA, layer.name="H10", lwd=2) +
  mapview(outpt, col.regions="red", layer.name="outflow", legend=FALSE)

# Delineate Watershed -----------------------------------------------------

# convert to lat lon:
outpt <- outpt %>% st_transform(4326)

# get coords
(xloc <- st_coordinates(outpt)[1])
(yloc <- st_coordinates(outpt)[2])

ws1 <- delineateWatershed(xlocation = xloc, ylocation = yloc, crs = 4326, rcode = "CA",
                          includeparameters = "true", includeflowtypes = "true")


# view map of watershed:
leafletWatershed(ws1)

# data in crazy nested list:
class(ws1$featurecollection[[2]]$feature$features[[1]]$geometry)

# save out
writeShapefile(watershed = ws1,
               layer = "ws_boundary", dir = "data/shps", what = "boundary")

# get characteristics
chars1 <- computeChars(workspaceID = ws1$workspaceID, rcode = "CA")
chars1$parameters

stats1 <- computeFlowStats(workspaceID = ws1$workspaceID, rcode = "CA", simplify = TRUE)


# Manual Watershed --------------------------------------------------------

lsh_ws <- st_read("data/lshasta_ss_delineation.geojson")

# map
mapview(lsh_ws) +
  mapview(h12, alpha.regions=0.1, col.regions="cyan", color="cyan", layer.name="H12") +
  mapview(lshasta_clean, layer.name="Streamlines", color="darkblue") +
  mapview(h10, alpha.regions=0.2, color="maroon", col.regions="maroon", layer.name="H10", lwd=2)


# Get Raster of Watershed -------------------------------------------------

library(elevatr)
library(sp)
# make an sp version of the h10
h10_sp <- as_Spatial(h10)
sp::proj4string(h10_sp)


x <- elevatr::get_elev_raster(h10_sp, z = 13, clip = "locations")

# https://r-spatial.github.io/stars/index.html
mapview(x)

library(stars)
dem <- st_as_stars(x)
class(dem)
st_crs(dem)
plot(dem, axes=TRUE)

# plot
ggplot() +
  #geom_stars(data = dem[1], alpha = 0.8, downsample = c(10, 10, 1)) +
  geom_stars(data = tst1[1], alpha = 0.8, downsample = 3) +
  viridis::scale_fill_viridis("Elev (m)", na.value="white") +
  coord_equal() +
  ggthemes::theme_map(base_family = "Roboto") +
  labs(title = "Little Shasta DEM with Streamlines",
       caption="Data from {elevatr}, z=13")+
  geom_sf(data=lshasta_clean, color="darkblue")+
  theme(legend.position = "bottom")

ggsave(filename = "figs/lshasta_dem_clean_streamline.png", width = 11, height = 8, dpi=300, units="in")

# write out dem
write_stars(adrop(dem[1]), "data/lshasta_dem.tif")

# read in?
tst1 <- stars::read_stars("data/lshasta_dem.tif")
plot(tst1, axes=FALSE)
