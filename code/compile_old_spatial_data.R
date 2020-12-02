# COMPILE OLD SPATIAL DATA
# THESE DATA COME FROM:
## CWS Archives (:X drive)
## NWIS/USGS


# TODO

# map springs and segments to mainstem
# attributing the segments so we match what COMIDs/flow network is using



# Libraries ---------------------------------------------------------------

# remotes::install_github("r-spatial/mapview")

library(sf)
library(tidyverse)
library(janitor)
library(here)
library(glue)
library(mapview)
mapviewOptions(
  fgb = FALSE,
  basemaps = c("Esri.WorldTopoMap", "Esri.WorldImagery","OpenTopoMap", "CartoDB.Positron"))

# these don't work for some reason: http://leaflet-extras.github.io/leaflet-providers/preview/
##"USGS.USTopo", "USGS.USImageryTopo", "USGS.USImagery"))

# Data Sets ---------------------------------------------------------------

# huc12
h12 <- st_read("data/shps/HUC12s.shp") %>% st_transform(4269)

# get huc8
h8 <- read_rds(glue("{here()}/data_output/huc8_shasta.rds")) %>% st_transform(4269)

# trim by huc8
h12_shasta <- st_intersection(h12, h8) %>% select(-c(TNMID:Shape_Area.1)) %>%
  st_transform(4269)

# Little Shasta Watershed (HUC10)
lshasta_watershed <- read_sf("data/shps/Little_Shasta_watershed.shp", crs=3310)
st_crs(lshasta_watershed)
h10_shasta <- lshasta_watershed

# trimmed stream network (final version?)
cws_lshasta <- readRDS("data_output/lshasta_stream_network_sf.rds")

# transform:
cws_lshasta <- st_transform(cws_lshasta, 3310)

# NWIS springs (inactive)
nwis_springs <- read_csv("data/nwis_springs_inactive.csv") %>%
  rename("lon"=SiteLongitude, "lat"=SiteLatitude) %>%
  st_as_sf(coords=c("lon","lat"), remove=FALSE, crs=4326) %>%
  st_transform(3310)

# DWR springs
dwr_springs <- read_sf("data/shps/DWR_Springs.shp") %>% st_transform(3310)
## trim by watershed
dwr_springs <- st_intersection(dwr_springs, lshasta_watershed)


# Set Geopackage ----------------------------------------------------------

db <- "data/nhdplus_little_shasta.gpkg"

# layers from NHD
st_layers(db)

# write out h10 and h12
st_write(h10_shasta, dsn=db, layer = "h10_lshasta",
         layer_options = "OVERWRITE=YES", delete_layer = TRUE)
st_write(h12_shasta, dsn=db, layer = "h12_lshasta",
         layer_options = "OVERWRITE=YES", delete_layer = TRUE)

ls_catch <- read_sf("data/nhdplus_little_shasta.gpkg", "CatchmentSP") %>% st_transform(3310)
nhd_flow <- read_sf("data/nhdplus_little_shasta.gpkg", "NHDFlowline_Network") %>%
  select(id:streamcalc,hydroseq) %>%
  st_transform(3310)

# NWIS wetlands
nwis_hist_wetland <- read_sf("data/shps/HU8_18010207_Wetlands.shp") %>%
  st_transform(3310) %>%
  # need to check/fix polygons
  st_make_valid()

# crop by watershed
nwis_hist_wetland <- st_intersection(nwis_hist_wetland, lshasta_watershed)

# more cws layers
cws_lshasta_ownership <- read_sf("data/shps/Shasta_ownership.shp") %>% st_transform(3310)
cws_lshasta_irrigcanal <- read_sf("data/shps/Shasta_MajorIrrigationCanals.shp") %>% st_transform(3310)

#mapview(st_transform(cws_lshasta_irrigcanal, crs=4326), color="yellow") + mapview(st_transform(cws_lshasta_ownership, 4326))#, zcol="OWN_GROUP")

# Mapview -----------------------------------------------------------------

# make map: first add streamlines/canals
(m1 <- mapview(cws_lshasta, color="darkblue", layer.name="CWS Corrected Streams", lwd=4.5) +
  mapview(nhd_flow, color="skyblue", lwd=2.5, layer.name="NHD flowline")+
  #mapview(cws_lshasta_irrigcanal, color="orange", lwd=1, layer.name="Major Irrig Canals") +
  # then add springs
  #mapview(dwr_springs, col.regions="seagreen", layer.name="DWR springs") +
  # add ownership (non-private)
  #mapview(cws_lshasta_ownership, zcol="OWN_GROUP", alpha.regions=0.2, layer.name="Non-Private<br>Land Ownership") +
  # then add watershed & catchments
  mapview(lshasta_watershed, layer.name="HUC8 Watershed", color="gray20", lwd=5, alpha.regions=0, legend=FALSE)+
  mapview(ls_catch, alpha.regions=0.2, layer.name="NHD Catchments")
  # then wetlands
  #mapview(nwis_hist_wetland, col.regions="darkgreen", color="darkgreen", alpha.regions=.2, layer.name="Historic NWIS Wetlands")
)

mapview::mapshot(m1, url = "docs/little_shasta_nhd_map.html")
