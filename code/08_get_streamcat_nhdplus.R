# retrieve NHDPlus Data

library(tidyverse)
library(fs)
library(here)
library(lubridate)
library(sf)
library(mapview)
mapviewOptions(fgb=FALSE)


# Set up Directory of Data ------------------------------------------------

dat_dir <- "/Volumes/GENOMICS/NHDplus_Accumulated_attributes"


# Get COMID List ----------------------------------------------------------

# get spatial data
st_layers("data/nhdplus_little_shasta.gpkg")

# all data: catch_h10, evans, flowlines, h10, lsh_springs
load(here("data_output","little_shasta_catchment_flowlines.rda"))
mapview(flowlines)

# reduce fields
flowlines_map <- flowlines %>% select(id, comid, hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc)

# updated catchment areas # catch_final, df_catch_diss, df_da_final
load(here("data_output/06_catcharea_final_adjust.rda"))
# aoi_comid <- df_da_final %>% filter(comid %in% c(3917946, 3917950, 3917198))

# reorder factors
df_da_final$comid_f <- factor(as.character(df_da_final$comid),
                              levels=c(3917198, 3917200, 3917948,
                                       3917950, 3917244, 3917946))

mapview(flowlines_map) + mapview(catch_final) + mapview(df_catch_diss, zcol="comid_f")

# save out just this:
write_rds(flowlines_map, file = "data_output/08_flowlines_final_lshasta.rds")

write_rds(catch_final, file = "data_output/08_catchments_final_lshasta.rds")

write_rds(df_catch_diss, file = "data_output/08_catchments_dissolve_final_lshasta.rds")
