# COMPILE OLD SPATIAL DATA
# THESE DATA COME FROM:
## CWS Archives (:X drive)
## NWIS/USGS


# TODO

# map springs and segments to mainstem
# attributing the segments so we match what COMIDs/flow network is using



# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(janitor)
library(mapview)
mapviewOptions(
  fgb = FALSE,
  basemaps = c("Esri.WorldTopoMap", "Esri.WorldImagery","OpenTopoMap", "CartoDB.Positron"))

# these don't work for some reason: http://leaflet-extras.github.io/leaflet-providers/preview/
##"USGS.USTopo", "USGS.USImageryTopo", "USGS.USImagery"))

# Data Sets ---------------------------------------------------------------

# Little Shasta Watershed
lshasta_watershed <- read_sf("data/shps/Little_Shasta_watershed.shp", crs=3310)
st_crs(lshasta_watershed)

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

# layers from NHD
st_layers("data/nhdplus_little_shasta.gpkg")

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
mapview(cws_lshasta, color="darkblue", layer.name="CWS Corrected Streams", lwd=4.5) +
  mapview(nhd_flow, color="skyblue", lwd=2.5, layer.name="NHD flowline")+
  mapview(cws_lshasta_irrigcanal, color="orange", lwd=1, layer.name="Major Irrig Canals") +
  # then add springs
  mapview(dwr_springs, col.regions="seagreen", layer.name="DWR springs") +
  # add ownership (non-private)
  mapview(cws_lshasta_ownership, zcol="OWN_GROUP", alpha.regions=0.2, layer.name="Non-Private<br>Land Ownership") +
  # then add watershed & catchments
  mapview(lshasta_watershed, color="gray20", lwd=5, alpha.regions=0, legend=FALSE)+
  mapview(ls_catch, alpha.regions=0.2, layer.name="NHD Catchments") +
  # then wetlands
  mapview(nwis_hist_wetland, col.regions="darkgreen", color="darkgreen", alpha.regions=.2, layer.name="Historic NWIS Wetlands")


