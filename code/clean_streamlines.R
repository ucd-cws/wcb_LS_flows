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
flowlines <- read_sf("data/nhdplus_little_shasta.gpkg", "NHDFlowline_Network")
lshasta_um <-read_sf("data/nhdplus_little_shasta.gpkg", "flowlines_um_lshasta")
lshasta_ut <-read_sf("data/nhdplus_little_shasta.gpkg", "flowlines_ut_lshasta")
catchment <- read_sf("data/nhdplus_little_shasta.gpkg", "CatchmentSP")
basin <- read_sf("data/nhdplus_little_shasta.gpkg", "basin_lshasta")

# trim metadata down to just important stuff
flowlines_trim <- flowlines %>% select(id:flowdir, ftype:pathlength, -fdate, -resolution, -flowdir, -levelpathi)

# mapview
(map_raw <- mapview(basin, color="darkblue", col.regions="steelblue", alpha.regions=0.1, alpha=0.5, lwd=2, layer="Catchment") +
    mapview(flowlines_trim, zcol="ftype", layer="Flowlines") +
    mapview(flowlines_trim, zcol="hydroseq", layer="HydroSeq", legend=FALSE))

# List Streamline Segments ------------------------------------------------

# First, create a list of all segments using their comids so we can add an attribute to them describing whether they are a stream channel (natural) or canal/ditch (canal)

all_segments <-  flowlines %>% select(comid, gnis_name, hydroseq, fromnode, tonode) #rbind(lshasta_um,lshasta_ut)

# is this to remove the sf part? not sure needed
# all_segments <- st_drop_geometry(all_segments_w_sf)

# add mainstem designation and column to indicate whether the natural channel
# is altered (e.g., the runoff is natural, but it's been run into a ditch that's
# routed to the stream), unaltered (e.g., the natural channel), or NA (e.g., it's a canal)

all_segments <- all_segments %>%
  mutate(
    channel_type = case_when(
      gnis_name == "Little Shasta River" ~ "natural",
      TRUE ~ "NA"),
    altered = case_when(
      gnis_name=="Little Shasta River" ~ "unaltered",
      TRUE ~ "NA")
  )

# Review each segment individually and update all_segments df

natural_unaltered_segs <- as.integer(c("3917194", "3917136", "3917138", "3917914", "3917916", "3917082", "3917114", "3917154", "3917156", "3917172", "3917164", "3917158", "3917160", "3917922", "3917364","3917372", "3917326", "3917330", "3917328", "3917374","3917392", "3917084", "3917084", "3917106", "3917130", "3917162", "3917176", "3917178", "3917198", "3917200", "3917244", "3917912", "3917918", "3917946", "3917948", "3917950"))

natural_altered_segs <- as.integer(c("948010089", "3917920"))

canal_segs <- as.integer(c("3917230", "3917954", "3918004", "3917382", "3917266", "3917222", "3917212", "3917214", "3917218", "3917228", "3917270", "3917250", "3917256", "3917268", "3917284", "3917282", "3917274", "3917278", "3917276", "3917958", "3917960", "3917384",  "3917370",  "3917376"))

all_segments <- all_segments %>%
  mutate(channel_type = case_when(comid %in% natural_unaltered_segs ~ "natural",
                                  comid %in% natural_altered_segs ~ "natural",
                                  comid %in% canal_segs ~ "canal",
                                  TRUE ~ channel_type))

all_segments <- all_segments %>%
  mutate(altered = case_when(comid %in% natural_unaltered_segs ~ "unaltered",
                             comid %in% natural_altered_segs ~ "altered",
                             comid %in% canal_segs ~ "NA",
                             TRUE ~ channel_type))


# FILTER ------------------------------------------------------------------

# filter all_segments df so that only natural channels w/in the Little Shasta watershed are included; UM segments are duplicated in the UT record, so filter out duplicate comids

lshasta_stream_network <- all_segments %>%
  filter(channel_type == "natural")


# REVIEW ------------------------------------------------------------------

mapview(lshasta_stream_network, lwd=2) + mapview(lshasta_ut, color="orange", lwd=1)

# ?? do we need to keep these or should they be dropped for simplicity?
extra_comids <- c(3917364, 3917372, 3917392, 3917374, 3917328, 3917330, 3917326)

# filter out
lshasta_stream_network <- lshasta_stream_network %>% filter(!comid %in% extra_comids)

# review
mapview(lshasta_stream_network, color="darkblue",lwd=4) + mapview(lshasta_ut, color="purple", lwd=2)

# write out Little Shasta stream network as csv
st_write(lshasta_stream_network, dsn = "data/lshasta_stream_network_sf.csv")

# write out as sf RDS object
write_rds(lshasta_stream_network, file="data/lshasta_stream_network_sf.rds")

