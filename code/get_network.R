# get flowline network


# Libraries ---------------------------------------------------------------

library(nhdplusTools)
library(glue)
library(sf)
library(here)
library(tidyverse)
library(mapview)
mapviewOptions(fgb = FALSE)

# Get Data ----------------------------------------------------------------

flowlines_final <- read_rds(file = "data_output/08_flowlines_final_lshasta.rds")

# get full flowlines dataset
load("data_output/little_shasta_catchment_flowlines.rda")

catch_final <- read_rds(file = "data_output/08_catchments_final_lshasta.rds") %>%
  mutate(upper = ifelse(is.na(upper), FALSE, upper))

# path to database:
db <- glue("{here()}/data/nhdplus_little_shasta.gpkg")
# layers:
st_layers(db)

# reduce fields
flowlines_map <- flowlines %>% select(id, comid, contains("seq"), hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc)

# updated catchment areas # catch_final, df_catch_diss, df_da_final, df_coms (all attribs, n=142)
load(here("data_output/06_catcharea_final_adjust.rda"))

loi_comid <- df_da_final %>% filter(comid %in% c(3917946, 3917950, 3917198))

# fix flowlines
flowlines_map <- flowlines_map %>%
  mutate(comid_f = factor(as.character(comid),
                          levels=c(3917198, 3917200, 3917948,
                                   3917950, 3917244, 3917946)))


# n=52 unique COMIDs
(coms <- unique(flowlines_map$comid))


# rm unused
rm(catch_ls_final)

# Mapview -----------------------------------------------------------------


mapview(flowlines_map, zcol="hydroseq")


# Get NLDI ----------------------------------------------------------------

# get index COMID based on precision or search area
# get_flowline_index("download_nhdplusv2",
#                    sf::st_sfc(sf::st_point(c(-76.87479, 39.48233)),
#                               crs = 4326), precision = 30)


# look up sources
# dataRetrieval::get_nldi_sources()
# "comid"

# COMID above diversion: 3917948
# COMID at confluence: 3917946

# get list of comids:
nld_ls <- list(featureSource = "comid",
               featureID = flowlines_map$comid[which(flowlines_map$comid==3917948)])

# now navigate and plot (UPSTREAM)
get_us <- navigate_nldi(nldi_feature = nld_ls,
              mode = "upstreamTributaries",
              distance_km = 120)$UT #%>%
  #st_geometry()
  #plot()
mapview(get_us, zcol="nhdplus_comid") +
  mapview(flowlines_map, color="steelblue", lwd=0.3)

# now get DOWNSTREAM
get_dm <- navigate_nldi(nldi_feature = nld_ls,
                           mode = "DM",
                           distance_km = 15)$DM #%>%
mapview(get_dm, zcol="nhdplus_comid") +
  mapview(flowlines_map, color="steelblue", lwd=0.3)


# GET NODES ---------------------------------------------------------------

# can we use these to move u/s to d/s for any given chunk?

# get nodes
starts <- get_node(get_us, "start") %>%
  mutate(comid=get_us$nhdplus_comid,
         ds_ord = seq(1:nrow(.)))
ends <- get_node(get_us, "end") %>%
  mutate(comid=get_us$nhdplus_comid)

mapview(get_us, zcol="nhdplus_comid", legend=FALSE) +
  mapview(flowlines_map, color="steelblue", lwd=0.3, legend=FALSE) +
  mapview(starts, col.regions="green") +
  mapview(starts, zcol="ds_ord") +
  mapview(ends, col.regions="red3", cex=1.5, legend=FALSE)

# Add Lucy's Functions ----------------------------------------------------

# function to grab the length of the longest list in a list of lists
find_max_list <- function(list_of_lists) {
  list_len <- c()
  for(i in list_of_lists) {
    list_len <- c(list_len, length(i))
  }
  return(max(list_len))
}


# function to recursively move up the tree and produce full paths from leaves to roots
get_next_down <- function(end, df, path, outlets) {

  path <- paste(path, end, collapse = " ")

  if(end %in% outlets) {
    return(sub(".*? ", "", path))
  }

  next_down <- df[df$from == end, "to"]
  # get_next_down(end = next_down,
  #               to_from = df,
  #               path = path,
  #               outlets = outlets)
  return(next_down)
}


# Build Adjacency table ---------------------------------------------------


# flowlines to from using hydroseq for adjacency table
flowlines_tofrom <- flowlines_map %>% st_drop_geometry() %>%
  # filter to just upstream since we don't need sink coms
  filter(comid %in% get_us$nhdplus_comid) %>%
  select(from = hydroseq,
         to = dnhydroseq)

## outlets (roots)
outlets_list <- flowlines_tofrom %>%
  filter(!to %in% from) %>%
  pull(to) %>%
  unique()

## ends (leaves)
ends <- flowlines_tofrom %>%
  filter(!from %in% to) %>%
  pull(from)


# Implement ---------------------------------------------------------------

# make empty df
output_df <- tibble(end = as.character(),
                    path_up_to_down = as.character(),
                    stringsAsFactors = FALSE)

# implement in loop
for(end in ends) {

  path_full <- get_next_down(end = end,
                             df = flowlines_tofrom,
                             path = c(),
                             outlets = outlets_list)

  row <- tibble(end = as.character(end),
                path_up_to_down = as.character(path_full))

  output_df <- rbind(output_df, row)

}

# massage the results to show both upstream and downstream path directions
output_df <- output_df %>%
  mutate(as_list = strsplit(path_up_to_down, " ")) %>%
  pull(as_list) %>%
  lapply(rev) %>%
  lapply(paste0, collapse = " ") %>%
  sapply(toString) %>%
  tibble() %>%
  cbind(output_df, .) %>%
  set_names(c("start", "path_up_to_down", "path_down_to_up"))


# create a single dataframe that shows all paths from upstream to downstream
up_to_down <- output_df %>%
  select(start, path_up_to_down) %>%
  mutate(across(.cols = where(is.character), as.numeric))

num_cols <- output_df %>%
  mutate(as_list = strsplit(path_up_to_down, " ")) %>%
  pull(as_list) %>%
  lapply(rev) %>%
  find_max_list()

col_names <- paste0("up_", 1:num_cols)

# create down_to_up dataframe
down_to_up <- output_df %>%
  select(path_down_to_up) %>%
  separate(col = path_down_to_up, into = col_names, sep = " ", remove = TRUE, fill = "right")

# iteratively pivot_longer to generate full long format
down_to_up_long <- map_dfr(1:(num_cols - 1), ~down_to_up %>%
                             select(.x:(num_cols)) %>%
                             rename(down = 1) %>%
                             pivot_longer(-down, values_to = "up") %>%
                             select(-name)) %>%
  filter(!is.na(up)) %>%
  unique() %>%
  arrange(down, up)
