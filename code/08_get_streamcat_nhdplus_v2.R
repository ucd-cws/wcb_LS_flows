# retrieve NHDV2 Stream Cat Data for specific COMID list

library(tidyverse)
library(vroom)
library(fs)
library(glue)
library(sf)
library(here)
library(mapview)
mapviewOptions(fgb=FALSE)

# FLOWLINES: Get Final Little Shasta FLOWLINE -------------------------------------------

flowlines <- read_rds(here("data_output/final_flowlines_w_full_nhd_vaa.rds"))

# reduce fields for plotting purposes
flowlines_trim <- flowlines %>% select(id, comid, contains("seq"), hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc, geom)

# fix flowlines comid to factor in correct order
flowlines_trim <- flowlines_trim %>%
  mutate(comid_f = factor(as.character(comid),
                          levels=c(3917198, 3917200, 3917948,
                                   3917950, 3917244, 3917946)),
         comid_ff = as.factor(comid))

# drop sinks (isolated channels)
sinks <- c(3917228, 3917212, 3917214, 3917218, 3917220,
           3917960, 3917958, 3917276, 3917278, 3917274,
           3917282, 3917284, 3917286, 3917280, 3917268,
           3917256, 3917250, 3917272, 3917956)

flowlines_trim <- flowlines_trim %>%
  filter(!comid %in% sinks)

# preview
mapview(flowlines_trim, zcol="comid_ff", legend=FALSE)


# Get CATCHMENTS ----------------------------------------------------------

# updated catchment areas # catch_final, df_catch_diss, df_da_final, df_coms (all attribs, n=142)
# load(here("data_output/06_catcharea_final_adjust.rda"))
#
# rm(df_catch_rev, df_catch_diss, df_da_final)
#
# # make comid char for plotting
# catch_final <- catch_final %>%
#   mutate(comid_c = as.factor(comid),
#          upper = if_else(is.na(comid_f), TRUE, FALSE))
#
# # save out
# write_rds(catch_final, "data_output/08_catch_final_lshasta.rds")

catch_final <- read_rds("data_output/08_catch_final_lshasta.rds")

mapview(flowlines_trim,  zcol="comid_ff", legend=FALSE) +
  mapview(catch_final, zcol="comid_c", legend=FALSE)

# n=33 unique COMIDs
(coms <- unique(flowlines_trim$comid))


# Set up Directories & Files ----------------------------------------------

# main directory
dat_dir <- "/Volumes/GENOMICS/NHDplus_Accumulated_attributes"
dir_exists(dat_dir)

# the subdir
# subfold_dir <- ("Climate_Attributes") # all zips and xml
# subfold_dir <- ("Climate_Water") # all zips
subfold_dir <- ("Geologic_Attributes") # has .txt and zip, lu_soller.zip is broken? won't unzip (in combination_of_landuse_and_geology)
# subfold_dir <- ("Hydrologic_Attributes")
# subfold_dir <- ("Regional_Attributes")
# subfold_dir <- ("Soil_Attributes")
# subfold_dir <- ("Topographic_Attributes")

# Load Functions ----------------------------------

source("code/f_functions.R")

# Get List of Files of Interest -------------------------------------------

# look for zips here
ziplist <- get_zip_list(glue("{dat_dir}/{subfold_dir}"), "*zip")
ziplist <- get_zip_list(glue("{dat_dir}/{subfold_dir}"), "*txt")
#ziplist <- get_zip_list(glue("{dat_dir}"), "*csv")
ziplist

# Filter to Comids --------------------------------------------------------

# filter to comids
alldat <- map(ziplist$path, ~comid_filter(coms, .x))

# check dimensions (one way to filter out zeros)
map(alldat, ~nrow(.x)>0)

# set names using ziplist
alldat_src <- alldat %>%
  # sets list names to filename
  set_names(fs::path_ext_remove(ziplist$filename)) %>%
  # adds a "source" column with filename
  imap(., ~ add_column(.x, source = .y))

names(alldat_src) # check

# check source in everything?
map_depth(alldat_src, .depth = 1, ~head(.x))

# Drop Zero Data --------------------------------------------------------------

# drop data with zero rows
alldat_filt <- discard( alldat_src, ~nrow(.x)==0)

# Write each file to single csv -----------------------------------------------

# use names of list to make filenames
pmap(list(alldat_filt, "data_output/nhdv2", names(alldat_filt)), ~comid_writeout(..1, ..2, ..3))

# Save into one file ------------------------------------------------------

alldat_combine <- alldat_filt %>%
  reduce(left_join, by = c("COMID")) %>% # join by COMID
  # drop dup columns
  select(-c(ends_with(".x"), contains("NODATA"), starts_with("source")))

# write out
write_csv(alldat_combine, file = glue("data_output/little_shasta_{janitor::make_clean_names(subfold_dir)}_comids.csv"))


