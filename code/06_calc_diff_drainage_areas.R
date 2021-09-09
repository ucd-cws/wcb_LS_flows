# calculate diff in watershd area and assign COMIDs to catchments
# for LOI (locations of interest)

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

# df_coms = selected flowline comids
load("data_output/05_selected_comids_AOI.rda")

# FFC predictions
ffc_preds <- read_csv("data_output/lshasta_ffc_predictions.csv")

# get adjusted catchments
(catchs <- fs::dir_ls(path = "data_output", type = "file", regexp = "ls_[0-9]*?_"))
# read in and combine
catchments <- map(catchs, ~read_rds(.x))
df_catch <- bind_rows(catchments) %>% st_transform(3310)

# need to fix an error in the comid:
df_catch <- df_catch %>%
  mutate(comid=case_when(
    grepl("39171948", comid) ~ 3917948,
    TRUE ~ comid),
    comid_f = as.factor(comid))
# check:
mapview(df_catch, zcol="comid_f")

# filter to these catch from updated later
df_catch_rev <- catch_ls_final %>% filter(FEATUREID %in% df_catch$FEATUREID) %>%
  left_join(st_drop_geometry(df_catch[,c("FEATUREID","comid", "comid_f")]))

# update individual drainage area:
df_catch_rev$area_sqkm <- st_area(df_catch_rev$geom) %>%
  units::set_units(km^2) %>% units::drop_units()
df_catch_rev <- df_catch_rev %>% select(GRIDCODE:AreaSqKM, area_sqkm, comid, comid_f)

mapview(df_catch, zcol = "comid_f", alpha.regions=0.5) +
  mapview(df_catch_rev, zcol="comid_f",color="forestgreen", lwd=2, alpha.regions=0.5) +
  mapview(catch_ls_final, col.regions=NA, alpha.regions=0)

# Tidy and Clean Data -----------------------------------------------------

# reduce fields
flowlines_map <- flowlines %>% select(id, comid, hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc) %>%
  mutate(comid_f = as.factor(comid))

# filter catchments from upper watershed:
catch_h10_us <- catch_ls_final %>% filter(!FEATUREID %in% df_catch_rev$FEATUREID) %>%
  mutate(comid = FEATUREID,
         upper = TRUE)

# bind together into one, fix crs first
st_crs(df_catch_rev)==st_crs(catch_h10_us)
catch_final <- bind_rows(df_catch_rev, catch_h10_us) %>%
  mutate(upper = if_else(is.na(upper), FALSE, TRUE))

# Dissolve by COMID -------------------------------------------------------

df_catch_diss <- rmapshaper::ms_dissolve(df_catch_rev, field="comid_f")

plot(df_catch_diss$geom, lwd=2, col="gray70")
plot(df_catch_rev$geom, border="maroon", add=T, lty=2)
plot(flowlines_map$geom, col = "steelblue", lwd=2, add=T)

# Calculate Revised Drainage Area -------------------------------------------------

# update the area in sq km
df_catch_diss$area_sqkm <- st_area(df_catch_diss$geom) %>%
  units::set_units(km^2) %>% units::drop_units()

# make comid a integer
df_catch_diss$comid <- as.integer(as.character(df_catch_diss$comid_f))

# select flowline area cols
df_coms_sel <- df_coms %>%
  select(comid, contains("sqkm"), geom, -hwnodesqkm)

# join flow comids with updated catch
df_flow_final <- left_join(df_coms_sel,
                           st_drop_geometry(df_catch_diss), by=c("comid")) %>%
  select(comid, comid_f, areasqkm, area_sqkm, totdasqkm:divdasqkm, geom)

# order the comids correctly
df_flow_final$comid_f <- forcats::fct_reorder(df_flow_final$comid_f,
                                               c("3917198", "3917200","3917948",
                                                 "3917950", "3917244","3917946"))

df_flow_final <- df_flow_final %>% arrange(comid_f)
levels(df_flow_final$comid_f)

levels(df_catch_diss$comid_f) # should match above but they don't...
# drop and rejoin
df_catch_diss <- df_catch_diss %>% select(-comid_f) %>%
  left_join(., st_drop_geometry(df_flow_final[,c("comid","comid_f")]))
levels(df_catch_diss$comid_f)

# Mapview map -------------------------------------------------------------

# use this color palette
cols <- colorspace::qualitative_hcl(6, palette = "dark2")

# make a map
mapview(catch_final %>% filter(upper==FALSE), zcol="comid_f",
        col.regions=cols,
        color=scales::alpha("gray50", 0.5), legend=TRUE, lwd=1, layer.name="Catchment") +
  mapview(flowlines_map, color="darkblue", lwd=2.5, legend=F) +
  mapview(catch_final %>% filter(upper==TRUE), color=alpha("gray50",0.25),
          alpha.regions=0, col.regions=NA, legend=FALSE, lwd=0.5, layer.name="Upper Catchment") +
  mapview(evans, layer.name="Evans Streamline", color="darkblue", legend=FALSE) +
  mapview(lsh_springs,layer.name="Springs", col.regions="cyan4") +
  mapview(h10_ls, layer.name="HUC10",col.regions="gray20", alpha.regions=0, color="gray20", lwd=3) +
  mapview(df_flow_final %>% filter(comid %in% c(3917946, 3917950, 3917198)), color="yellow", lwd=4.5, legend=T, layer.name="LOI")


# Finally Calculate Percentage Difference to Adjust Metrics ---------------

mapview(df_flow_final, zcol="comid_f", lwd=4, layer.name="Flowline") +
  mapview(df_catch_diss, zcol="comid_f", layer.name="Catchments") +
  mapview(flowlines_map, color="cyan4", legend=F, lwd=0.5) +
  mapview(catch_final, color="black", alpha.col=0.8, col.regions=NA, legend=FALSE, lwd=0.6) +
  mapview(evans, layer.name="Evans Streamline", color="cyan4") +
  mapview(lsh_springs,layer.name="Springs", col.regions="cyan4")

# adjust percentage diff in watershed drainage areas per comid

# the entire drainage area upstream:
# catch_final %>% filter(upper==TRUE) %>%
# st_area() %>% sum() %>% units::set_units(km^2) # 145.0116 [km^2]
df_da_final <- df_flow_final %>%
  arrange(comid_f) %>%
  mutate(totdasqkm_new=cumsum(area_sqkm)+145.0116, # this is from upstream,
         adjusted_da_prcnt=((totdasqkm_new-totdasqkm)/totdasqkm)*100) %>%
  rename(areasqkm_old = areasqkm)

# view
mapview(df_da_final, zcol="comid_f", lwd=4) +
  mapview(df_flow_final, zcol="comid_f", lwd=4, layer.name="Flowline")
# the diff between totdasqkm and divdasqkm (326.6136)

# save
save(df_catch_diss, df_catch_rev, df_da_final, catch_final, file = "data_output/06_catcharea_final_adjust.rda")

