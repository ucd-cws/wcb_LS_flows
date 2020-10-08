# visualizing with mapview


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


# Static Plot -------------------------------------------------------------

# quick static plot
plot(catchment$geom,  lwd = 4, border = "gray30")
plot(st_geometry(flowlines), lwd = 7, col = alpha("blue", 0.5), add=TRUE)
plot(st_geometry(lshasta_ut), lwd = 1.2, col = "dodgerblue", add = TRUE)



# nhdplusTools plot and summary stats for comid SEG ------------------------

library(dplyr)
library(nhdplusTools)

# this is the most downstream NHD segment on Little Shasta
nldi_feature <- list(featureSource = "comid",
                     featureID = "3917946")

# pull flowlines and full basin, make plot of catchments and flowlines
data <- plot_nhdplus(nldi_feature, flowline_only = FALSE)

# hmmm

# Use the COMID to look up NLDI characteristics and return a summary table
# see here: https://waterdata.usgs.gov/blog/nldi_update/

# this is all the possible NLDI data we can look at
chars <- discover_nldi_characteristics()

# this is to pull the totals for our COMID
outlet_total <- get_nldi_characteristics(nldi_feature, type = "total")

# join with the names and info
outlet_total <- left_join(outlet_total$total, chars$total,
                          by = "characteristic_id")

# format and update
outlet_total <- outlet_total %>%
  select(ID = characteristic_id,
         Description = characteristic_description,
         Value = characteristic_value,
         Units = units,
         link = dataset_url) %>%
  mutate(link = paste0('<a href="', link, '">link</a>'))

#knitr::kable(outlet_total)
# make a table
DT::datatable(outlet_total)


# Mapview -----------------------------------------------------------------


# mapview
map_raw <- mapview(catchment, color="gray50", col.regions="gray50", alpha.regions=0, alpha=0.5, lwd=2) +
  mapview(flowlines, zcol="ftype") +
  mapview(lshasta_ut, color="dodgerblue", lwd=0.75, alpha=0.9)


# so mainstem seems to be ok, but the associated tributaries are a jumble of diversions, canals, etc.

# this is a way to edit that map should we choose to
library(mapedit)
library(leafem)
library(leaflet)

#lshasta_network <- map_raw %>% mapedit::editMap(targetLayerId="lshasta_ut", editor="leafpm")
