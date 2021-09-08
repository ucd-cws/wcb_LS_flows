# export to flat for Lucy

library(sf)
library(tidyverse)
library(mapview)
mapviewOptions(fgb = FALSE)

load("/Users/rapeek/Documents/github/wcb_LS_flows/data_output/05_selected_comids_AOI.rda")

flow_attrib <- st_read("data_output/shps/revised_lshasta_flowlines_catchments/little_shasta_revised_flowlines.shp") %>%
  select(comid, reachcd, fcode,
         fromnode=fromnod, tonode,
         hydroseq=hydrosq,
         uphydroseq=uphydrs, dnhydroseq=dnhydrs,
         areasqkm=aresqkm, totdsqkm=ttdsqkm,
         termnlp, divrgnc, strtflg, trmnlfl,
         dnlevel)

flowlines <- read_rds("data_output/08_flowlines_final_lshasta.rds")
catch_split <- read_rds("data_output/08_catchments_final_lshasta.rds")
catch_diss <- read_rds("data_output/08_catchments_dissolve_final_lshasta.rds")

mapview(catch_split, alpha.regions=0) +
  mapview(catch_diss, zcol="comid_f") +
  mapview(flow_attrib, zcol="fromnode")

save(flow_attrib, catch_diss, catch_split, file = "data_output/little_shasta_cleaned_spatial_flowcatch.rda")
