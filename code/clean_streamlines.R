# Clean streamlines -------------------------------------------------------


# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(mapview)
mapviewOptions(fgb=FALSE)


# Get Data ----------------------------------------------------------------

# layers:
st_layers("data/nhdplus_little_shasta.gpkg")

# read data in: flowlines
flowlines <- sf::read_sf("data/nhdplus_little_shasta.gpkg", "NHDFlowline_Network")
catchment <- sf::read_sf("data/nhdplus_little_shasta.gpkg", "CatchmentSP")
lshasta_ut <- sf::read_sf("data/nhdplus_little_shasta.gpkg", "flowlines_ut_lshasta")



# List Streamline Segments ------------------------------------------------


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

