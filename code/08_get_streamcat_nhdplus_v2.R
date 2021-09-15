# retrieve NHDV2 Stream Cat Data for specific COMID list

library(tidyverse)
library(vroom)
library(fs)
library(glue)
library(sf)
library(mapview)
mapviewOptions(fgb=FALSE)

# Get Final Little Shasta COMIDs -------------------------------------------

flowlines_final <- read_rds(file = "data_output/08_flowlines_final_lshasta.rds")
flowlines_final <- flowlines_final %>%
  mutate(comid_f = factor(as.character(comid),
                          levels=c(3917198, 3917200, 3917948,
                                   3917950, 3917244, 3917946)))

catch_final <- read_rds(file = "data_output/08_catchments_final_lshasta.rds") %>%
  mutate(upper = ifelse(is.na(upper), FALSE, upper))

# n=52 unique COMIDs
(coms <- unique(flowlines_final$comid))

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


