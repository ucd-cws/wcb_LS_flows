# calculate diff in watershd area


# Libraries ---------------------------------------------------------------

library(sf)
library(tidyverse)
library(mapview)
mapviewOptions(fgb = FALSE)
library(fs)
library(purrr)

# Data --------------------------------------------------------------------

# list of everything
load("data_output/little_shasta_catchment_flowlines.rda")

# df_coms = selected comids
load("data_output/05_selected_comids_AOI.rda")

# FFC predictions
ffc_preds <- read_csv("data_output/lshasta_ffc_predictions.csv")

# get adjusted catchments
catchs <- fs::dir_ls(path = "data_output", type = "file", regexp = "ls_[0-9]*?_")
# read in and combine
catchments <- map(catchs, ~read_rds(.x))
df_catch <- bind_rows(catchments) %>% st_transform(3310)

# Tidy and Clean Data -----------------------------------------------------

# reduce fields
flowlines_map <- flowlines %>% select(id, comid, hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc)

# fix comid issue *39171948 should be 3917948
df_catch <- df_catch %>% mutate(comid = case_when(
  grepl(39171948, comid) ~ 3917948,
  TRUE ~ comid))

# make factor
df_catch$comid_f <- as.factor(df_catch$comid)
flowlines_map$comid_f <- as.factor(flowlines_map$comid)

# make a sum area table of drainage area for each COMID (original area_sqkm)
df_catch_summary <- df_catch %>% st_drop_geometry %>% group_by(comid_f) %>%
  summarize(areasqkm_orig = sum(AreaSqKM))

# filter catchs from upper watershed:
catch_h10_us <- catch_h10 %>% filter(!FEATUREID %in% df_catch$FEATUREID)
catch_h10_us$comid <- catch_h10_us$FEATUREID
catch_h10_us$upper <- TRUE

# bind together into one, fix crs first
st_crs(df_catch)==st_crs(catch_h10_us)
catch_final <- bind_rows(df_catch, catch_h10_us)
st_crs(catch_final)

# make comid a factor
catch_final$comid_f <- as.factor(catch_final$comid)

# Mapview map -------------------------------------------------------------

# make a map
mapview(df_catch, zcol="comid_f", col.regions=colorspace::qualitative_hcl(6, palette = "dark2"), legend=TRUE, lwd=0.6, layer.name="COMID") +
  mapview(flowlines_map, color="cyan4", legend=F) +
  mapview(catch_h10_us, color="black", alpha.col=0.8, col.regions=NA, legend=FALSE, lwd=0.6) +
  mapview(evans, layer.name="Evans Streamline", color="cyan4") +
  mapview(lsh_springs,layer.name="Springs", col.regions="cyan4") +
  mapview(h10, layer.name="HUC10",col.regions="gray20", alpha.col=0.5, alpha.regions=0, color="gray20", lwd=3)

# Dissolve by COMID -------------------------------------------------------

df_catch_diss <- rmapshaper::ms_dissolve(df_catch, field="comid_f")

plot(df_catch_diss$geom, lwd=2, col="gray70")
plot(df_catch$geom, border="maroon", add=T, lty=2)
plot(flowlines_map$geom, col = "steelblue", lwd=2, add=T)

# Calculate Revised Drainage Area -------------------------------------------------

df_catch_diss$area_sqkm <- st_area(df_catch_diss$geom) %>% units::set_units(km^2)

df_catch_diss$area_sqkm_new <- df_catch_diss$area_sqkm %>% units::drop_units() %>% round(3)

# make comid a integer
df_catch_diss$comid <- as.integer(as.character(df_catch_diss$comid_f))

# map
mapview(df_catch_diss, zcol="comid_f") +
  mapview(df_coms, zcol="comid")

# select flowline area cols
df_coms_sel <- df_coms %>%
  select(comid, contains("sqkm"), geom, -hwnodesqkm)

# join with updated catch
df_catch_final <- left_join(df_coms_sel, st_drop_geometry(df_catch_diss), by=c("comid")) %>%
  select(comid, comid_f, areasqkm, area_sqkm_new, totdasqkm:divdasqkm, -c(area_sqkm), geom)

# order the comids correctly
df_catch_final$comid_f <- forcats::fct_reorder(df_catch_final$comid_f,
                                               c("3917198", "3917200","3917948",
                                                 "3917950", "3917944","3917946"))

df_catch_final <- df_catch_final %>% arrange(comid_f)

# Finally Calculate Percentage Difference to Adjust Metrics ---------------

# mapview(df_catch_final, zcol="comid_f", lwd=4) +
#   mapview(df_catch_diss, zcol="comid_f") +
#   mapview(flowlines_map, color="cyan4", legend=F, lwd=0.5) +
#   mapview(catch_final, color="black", alpha.col=0.8, col.regions=NA, legend=FALSE, lwd=0.6) +
#   mapview(evans, layer.name="Evans Streamline", color="cyan4") +
#   mapview(lsh_springs,layer.name="Springs", col.regions="cyan4")

# adjust percentage diff in watershed drainage areas per comid
df_da_final <- df_catch_final %>% #st_drop_geometry() %>%
  arrange(comid_f) %>%
  mutate(totdasqkm_new=cumsum(area_sqkm_new)+145.0116,
         adjusted_da_prcnt=((totdasqkm_new-totdasqkm)/totdasqkm)*100) %>%
  select(-areasqkm)

# view
df_da_final

# the diff between totdasqkm and divdasqkm (326.6136)

# save
save(df_catch_diss, df_da_final, catch_final, file = "data_output/06_catcharea_final_adjust.rda")

