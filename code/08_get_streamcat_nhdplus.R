# retrieve NHDPlus Data

library(tidyverse)
library(vroom)
library(archive)
library(fs)
library(glue)
library(here)
library(lubridate)
library(sf)
library(mapview)
mapviewOptions(fgb=FALSE)

# Get Final Little Shasta COMIDs -------------------------------------------

flowlines_final <- read_rds(file = "data_output/08_flowlines_final_lshasta.rds")
flowlines_final <- flowlines_final %>% mutate(comid_f = as.factor(comid))
catch_final <- read_rds(file = "data_output/08_catchments_final_lshasta.rds") %>%
  mutate(upper = ifelse(is.na(upper), FALSE, upper))

mapview(flowlines_final, zcol="comid_f", layer.name="COMID", legend=FALSE) +
  mapview(catch_final, zcol="upper", layer.name="Catch", legend =FALSE)

# n=52 unique COMIDs
coms <- unique(flowlines_final$comid)

# Set up Directories & Files ----------------------------------------------

# main directory
dat_dir <- "/Volumes/GENOMICS/NHDplus_Accumulated_attributes"
dir_exists(dat_dir)

# the subdir
subfold_dir <- ("Climate_Attributes")

#subfile_name <- "PRSNOW_CONUS"

# file exists?
#list.files(path = glue("{dat_dir}/{subfold_dir}/{subsub}"), pattern = glue("^{subfile_name}*"))
#file_size(glue("{dat_dir}/{subfold_dir}/{subsub}/{subfile_name}.txt"))
#file_size(glue("{dat_dir}/{subfold_dir}/{subsub}/{subfile_name}.zip"))


# Function to Get List of Zips --------------------------------------------

# get list of zips in given folder:
get_zip_list <- function(folder, extension){
  fs::dir_info(folder, recurse = TRUE,
               type = "file",
               regexp = glue::glue("{extension}")) %>%
    dplyr::select(path, size) %>%
    dplyr::mutate(filename = fs::path_file(path))
}

tst <- get_zip_list(glue("{dat_dir}/{subfold_dir}"), "*zip")


# Function to Filter to Specific COMIDS ----------------------------------

comid_filter <- function(comids, fullpath){
  library(purrr)
  f1 <- vroom(fullpath) %>% # fast readin of zips
    dplyr::filter({if("COMID" %in% names(.)) COMID else NULL} %in% comids)
}

# test the function
# ftst <- comid_filter(coms, tst[1,]$path) # it works!

# Implement ---------------------------------------------------------------

alldat <- map(tst$path, ~comid_filter(coms, .x))

# check dimensions (one way to filter out zeros)
map(alldat, ~dim(.x))
map(alldat, ~nrow(.x)>0)

# set names
alldat_src <- alldat %>% set_names(fs::path_ext_remove(tst$filename)) %>%
  imap(., ~ add_column(.x, source = .y))
names(alldat_src)

# check source in everything?
map_depth(alldat_src, .depth = 1, ~head(.x))

# drop data with zero rows
alldat_filt <- discard( alldat_src, ~nrow(.x)==0)

# WRITE OUT ---------------------------------------------------------------

comid_writeout <- function(data, fileName){
  write_csv(data, file = glue("data_output/nhdv2/{fileName}.csv"))
}

# tst
# comid_writeout(alldat_names[[1]], names(alldat_names[1]))

# now run all
pmap(list(alldat_filt, names(alldat_filt)), ~comid_writeout(.x, .y))


# Save into one file ------------------------------------------------------

alldat_combine <- alldat_filt %>% reduce(left_join, by = c("COMID")) %>%
  select(-c(ends_with(".x"), contains("NODATA"), starts_with("source")))

# write out
write_csv(alldat_combine, file = glue("data_output/little_shasta_{janitor::make_clean_names(subfold_dir)}_comids.csv"))





# Z. ARCHIVED: Get Spatial data for COMID List ----------------------------------------------------------

# # get spatial data
# st_layers("data/nhdplus_little_shasta.gpkg")
#
# # all data: catch_h10, evans, flowlines, h10, lsh_springs
# load(here("data_output","little_shasta_catchment_flowlines.rda"))
# mapview(flowlines)
#
# # reduce fields
# flowlines_map <- flowlines %>% select(id, comid, hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc)
#
# # updated catchment areas # catch_final, df_catch_diss, df_da_final
# load(here("data_output/06_catcharea_final_adjust.rda"))
# # aoi_comid <- df_da_final %>% filter(comid %in% c(3917946, 3917950, 3917198))
#
# # reorder factors
# df_da_final$comid_f <- factor(as.character(df_da_final$comid),
#                               levels=c(3917198, 3917200, 3917948,
#                                        3917950, 3917244, 3917946))
#
# mapview(flowlines_map) + mapview(catch_final) + mapview(df_catch_diss, zcol="comid_f")
#
# # save out just this:
# write_rds(flowlines_map, file = "data_output/08_flowlines_final_lshasta.rds")
#
# write_rds(catch_final, file = "data_output/08_catchments_final_lshasta.rds")
#
# write_rds(df_catch_diss, file = "data_output/08_catchments_dissolve_final_lshasta.rds")


