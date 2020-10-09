# Get nhd flow network data

# Wed Oct  7 14:53:33 2020 ------------------------------

# this pulls flow network from NHD and NHD+ attributes for cleaning
# see here for NHDplus National Data
# https://www.epa.gov/waterdata/nhdplus-national-data

# Libraries ---------------------------------------------------------------

library(sf)
library(nhdplusTools)
library(tidyverse)

# Identify the COMID ------------------------------------------------------

# comid for confluence segment Shasta-Little Shasta
PMK <- st_sfc(st_point(c(-122.51016, 41.70364)), crs=4326) # peacemaker gage
LSR <- st_sfc(st_point(c(-122.34792, 41.73355)), crs=4326) # upstream gage

(PMK_comid <- discover_nhdplus_id(point = PMK))
(LSR_comid <- discover_nhdplus_id(point = LSR))

pts <- tibble("site"=c("PMK","LSR"), "comid"=c(PMK_comid, LSR_comid))

# what sources of data?
# discover_nldi_sources()$source

# Use NHD to Download Flow Network ----------------------------------------

# make a list of comids
pt_list <- map(pts$comid, ~list(featureSource = "comid", featureID=.x))

# Get stream segments, this can take a minute
us_main <- map(pt_list[1], ~navigate_nldi(nldi_feature = .x,
                                       mode="UM",
                                       distance_km = 110))

us_trib <- map(pt_list[1], ~navigate_nldi(nldi_feature = .x,
                                       mode="UT",
                                       distance_km = 110))

# make a single flat layer
um_flat <- us_main %>%
  set_names(., pts$comid[1]) #%>%
  #map2(pts$site, ~mutate(.x, site=.y))


ut_flat <- us_trib %>%
  set_names(., pts$comid[1])


# bind together
# mainstems
lshasta_um <- do.call(what = sf:::rbind.sf,
                      args = um_flat) %>%
  mutate(type="UM")

# Tributaries
lshasta_ut <- do.call(what = sf:::rbind.sf,
                      args = ut_flat) %>%
  mutate(type="UT")

# preview
#mapview::mapview(lshasta_um, zcol="nhdplus_comid", legend = F) + mapview::mapview(lshasta_ut, color="slateblue", lwd=0.5, legend=F)

# DOWNLOAD NHD ATTRIBUTES AND WRITE TO GEOPACKAGE----------------------------

# download the added data
output_file_download <- file.path(here::here("data", "nhdplus_little_shasta.gpkg"))

# this retrieves all the detailed information for both mainstem and tribs
output_flowlines <-subset_nhdplus(comids = lshasta_ut$nhdplus_comid,
                                      output_file = output_file_download,
                                      flowline_only = FALSE,
                                      overwrite = TRUE,
                                      nhdplus_data = "download", return_data = FALSE)


# add the simple mainstem (without all the NHD attributes)
st_write(lshasta_um, dsn="data/nhdplus_little_shasta.gpkg", layer = "flowlines_um_lshasta",
         layer_options = "OVERWRITE=YES", delete_layer = TRUE)

# add the simple tribs (without all the NHD attributes)
st_write(lshasta_ut, dsn="data/nhdplus_little_shasta.gpkg", layer = "flowlines_ut_lshasta",
         layer_options = "OVERWRITE=YES", delete_layer = TRUE)

# get the full basin with nldi function and dataRetrieval package
basin <- dataRetrieval::findNLDI(comid = PMK_comid, find = "basin") %>%
  lapply(st_as_sf)
#mapview::mapview(basin$basin) + mapview::mapview(basin$origin)

# write to gpkg
st_write(basin$basin, dsn="data/nhdplus_little_shasta.gpkg", layer = "basin_lshasta",
         layer_options = "OVERWRITE=YES", delete_layer = TRUE)


# CHECK Layers in DB --------------------------------------------------

# see what layers
sf::st_layers("data/nhdplus_little_shasta.gpkg")

# this explains NHD attributes:
# https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/Data/NationalData/0Release_Notes_NationalData_Seamless_GeoDatabase.pdf


