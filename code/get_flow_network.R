# Get nhd flow network data

# Wed Oct  7 14:53:33 2020 ------------------------------

# this pulls flow network from NHD and NHD+ attributes for cleaning
# see here for NHDplus National Data
# https://www.epa.gov/waterdata/nhdplus-national-data

# Libraries ---------------------------------------------------------------

library(sf)
library(nhdplusTools)
library(tidyverse)
library(mapview)
library(mapedit)
library(leaflet)
library(leafpm)

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
mapview(lshasta_um, zcol="nhdplus_comid", legend = F) + mapview(lshasta_ut, color="slateblue", lwd=0.5, legend=F)


# download the added data
output_file_download <- file.path(here::here("data", "nhdplus_little_shasta.gpkg"))

# this retrieves all the detailed information
output_file_download <-subset_nhdplus(comids = lshasta_um$nhdplus_comid,
                                      output_file = output_file_download,
                                      flowline_only = FALSE,
                                      overwrite = TRUE,
                                      nhdplus_data = "download", return_data = FALSE)


st_write(lshasta_ut, dsn="data/nhdplus_little_shasta.gpkg", layer = "flowlines_ut_lshasta",
         layer_options = "OVERWRITE=YES", delete_layer = TRUE)


# READ IN DATA AND CHECK --------------------------------------------------

# see what layers
sf::st_layers("data/nhdplus_little_shasta.gpkg")

# this explains NHD attributes:
# https://s3.amazonaws.com/edap-nhdplus/NHDPlusV21/Data/NationalData/0Release_Notes_NationalData_Seamless_GeoDatabase.pdf


# QUICK PLOTS -------------------------------------------------------------

#mapviewOptions(fgb=FALSE) #This argument didn't mean anything when Ann ran the code

# read data in: flowlines
flowlines <- sf::read_sf("data/nhdplus_little_shasta.gpkg", "NHDFlowline_Network")
catchment <- sf::read_sf("data/nhdplus_little_shasta.gpkg", "CatchmentSP")

# quick plot
plot(catchment$geom,  lwd = 4, border = "maroon3")
plot(sf::st_geometry(flowlines), lwd = 7, col = alpha("blue", 0.5), add=TRUE)
plot(lshasta_ut$geometry, lwd = 1.2, col = "dodgerblue", add = TRUE)

# mapview
lshasta_rawmap <- mapview(catchment, color="maroon3", col.regions="gray50", alpha.regions=0, alpha=0.9, lwd=2) +
  mapview(flowlines, zcol="ftype") +
  mapview(lshasta_ut, color="dodgerblue", lwd=2, alpha=0.9)

lshasta_rawmap
# so mainstem seems to be ok, but the associated tributaries are a jumble of diversions, canals, etc
# Also, catchment outline is incomplete. See map in Little Shasta Report, figure 1 for the HUC catchment


# Clean streamlines -------------------------------------------------------

# First, create a list of all segments using their comids so we can add an attribute to them describing whether they are a stream channel (natural) or canal/ditch (canal)

all_segments_w_sf <- rbind(lshasta_um,lshasta_ut)

all_segments <- st_set_geometry(all_segments_w_sf,NULL)

all_segments$channel_type <- with(all_segments, ifelse(type=="UM","natural",""))

# Add another column to indicate whether the natural channel is altered (e.g., the runoff is natural, but it's been run into a ditch that's routed to the stream), unaltered (e.g., the natural channel), or NA (e.g., it's a canal)

all_segments$altered <- with(all_segments, ifelse(type=="UM","unaltered",""))

# Review each segment individually and update all_segments df

natural_unaltered_segs <- as.character(c("3917194", "3917136", "3917138", "3917914", "3917916", "3917082", "3917114", "3917154", "3917156", "3917172", "3917164", "3917158", "3917160", "3917922", "3917364","3917372", "3917326", "3917330", "3917328", "3917374","3917392", "3917084", "3917084", "3917106", "3917130", "3917162", "3917176", "3917178", "3917198", "3917200", "3917244", "3917912", "3917918", "3917946", "3917948", "3917950"))

natural_altered_segs <- as.character(c("948010089", "3917920"))

canal_segs <- as.character(c("3917230", "3917954", "3918004", "3917382", "3917266", "3917222", "3917212", "3917214", "3917218", "3917228", "3917270", "3917250", "3917256", "3917268", "3917284", "3917282", "3917274", "3917278", "3917276", "3917958", "3917960", "3917384",  "3917370",  "3917376"))

all_segments <- all_segments %>%
  mutate(channel_type = case_when(nhdplus_comid %in% natural_unaltered_segs ~ "natural",
                                  nhdplus_comid %in% natural_altered_segs ~ "natural",
                                  nhdplus_comid %in% canal_segs ~ "canal"))

all_segments <- all_segments %>%
  mutate(altered = case_when(nhdplus_comid %in% natural_unaltered_segs ~ "unaltered",
                                  nhdplus_comid %in% natural_altered_segs ~ "altered",
                                  nhdplus_comid %in% canal_segs ~ "NA"))

# filter all_segments df so that only natural channels w/in the Little Shasta watershed are included; UM segments are duplicated in the UT record, so filter out duplicate comids

lshasta_stream_network <- all_segments %>%
  filter(channel_type == "natural", type == "UT")

#write out Little Shasta stream network as csv

write_csv(lshasta_stream_network, path = "data/lshasta_stream_network.csv")

