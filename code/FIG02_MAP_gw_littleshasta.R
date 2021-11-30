# Maps for Tech Report

# Libraries ---------------------------------------------------------------

library(sf)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)
#library(tidyverse)
library(fs)
library(here)
library(glue)
library(tmap)
library(tmaptools)
library(mapview)
mapviewOptions(fgb=FALSE, basemaps=c("Esri.WorldTopoMap", "Esri.WorldImagery",
                                     "OpenTopoMap", "OpenStreetMap",
                                     "CartoDB.Positron", "Stamen.TopOSMFeatures"))

# Data --------------------------------------------------------------------

# all data: catch_h10, evans, flowlines, h10, lsh_springs
load(here("data_output","little_shasta_catchment_flowlines.rda"))

# path to database:
db <- glue("{here()}/data/nhdplus_little_shasta.gpkg")
# layers:
st_layers(db)

# reduce fields
flowlines_map <- flowlines %>% select(id, comid, contains("seq"), hydroseq, gnis_name, areasqkm:divdasqkm, shape_length, streamorde, streamorder_map, streamcalc)

# updated catchment areas # catch_final, df_catch_diss, df_da_final, df_coms (all attribs, n=142)
load(here("data_output/06_catcharea_final_adjust.rda"))

loi_comid <- df_da_final %>% filter(comid %in% c(3917946, 3917950, 3917198))

# write_rds(loi_comid, file = "data_output/07_lshasta_loi_comids_flowline.rds")

# reorder factors
df_da_final$comid_f <- factor(as.character(df_da_final$comid),
                              levels=c(3917198, 3917200, 3917948,
                                       3917950, 3917244, 3917946))
# path to database:
db <- here("data/nhdplus_little_shasta.gpkg")
st_layers(db)

# original catchments
catch_orig <- st_read(db, "catchments_ls_nhdplus18", quiet = TRUE)

catch_final <- catch_final %>%
  mutate(comid_ff = as.factor(comid))

# Gages -------------------------------------------------------------------


# USGS GAGE
gages_usgs <- read_rds(file = "data_output/gages_lshasta.rds") %>%
   mutate(site_number=as.character(site_number))

# LSR gage
gage_lsr <- st_as_sf(data.frame("site_longitude"=-122.350357, "site_latitude"=41.733093, "site_name"="LSR", "site_number"="LSR", "site_agency"="UCD"), coords=c("site_longitude", "site_latitude"), crs=4326, remove=FALSE)

# filter to just stations of interest
gages <- gages_usgs %>% filter(site_number %in% c("11517000","11516900")) %>%
   bind_rows(gage_lsr)


# GDE Data ----------------------------------------------------------------

# # Groundwater Dependent Ecosystems
# st_layers("~/Downloads/i02_naturalcommunitiescommonlyassociatedwithgroundwater/i02_NCCAG_Wetlands.shp")
#
# gde_wet <- st_read("~/Downloads/i02_naturalcommunitiescommonlyassociatedwithgroundwater/i02_NCCAG_Wetlands.shp") %>% st_zm()
# gde_veg <- st_read("~/Downloads/i02_naturalcommunitiescommonlyassociatedwithgroundwater/i02_NCCAG_Vegetation.shp") %>% st_zm()
#
# st_crs(gde_wet)$epsg
# st_crs(gde_veg)$epsg
#
# # crop to LS
# gde_wls <- gde_wet[h10_ls,]
# gde_vls <- gde_veg[h10_ls,]
#
# mapview(gde_wls) + mapview(gde_vls, col.regions="orange", alpha.regions=0.4)
# st_write(gde_vls, dsn = db, "gde_veg_lsh")
# st_write(gde_wls, dsn = db, "gde_wet_lsh")

st_layers(db)
gde_veg_lsh <- st_read(db, "gde_veg_lsh")
gde_wet_lsh <- st_read(db, "gde_wet_lsh")


# Add StreamTypes and Klamath ---------------------------------------------

shasta_main <- st_read("data/shps/ShastaRiver.shp")

lsh_strmtype <- st_read("data/shps/LS_3reaches.shp")

# now subselect down:
mapview(lsh_strmtype, zcol="Name", lwd=5) + mapview(flowlines_map, color="blue")

lsh_foothills <- c(10040642, 10035874,10033986, 10032346, 10038060)
lsh_bottomlands <- c(10021351, 10020862, 10020426)
lsh_hw <- c(10043744, 10047601, 10052559, 10059365, 10069513, 0135206)

# update flowlines_map layer:
flowlines_map <- flowlines_map %>%
  mutate(strmtype=case_when(
    hydroseq %in% lsh_foothills ~ "Foothills",
    hydroseq %in% lsh_bottomlands ~ "Bottomlands",
    hydroseq %in% lsh_hw ~ "Headwaters",
    TRUE ~ "Tributaries"))

# Make LOI ----------------------------------------------------------------

loi_pts <- st_point_on_surface(loi_comid) %>%
  mutate(loi_id = case_when(
    comid_f=="3917946" ~ "LOI-1",
    comid_f=="3917950" ~ "LOI-2",
    comid_f=="3917198" ~ "LOI-3"
  ))

# Raster Data -------------------------------------------------------------

# get raster data
lsh_dem <- stars::read_stars("data/lshasta_dem.tif") # raster DEM
load("data_output/tmaptools_h10_osm_natgeo.rda") # topo base layer
tmap_options(max.raster = c(plot=1e6, view=1e6))

#gm_osm <- read_osm(h10_ls, type = "esri-topo", raster=TRUE, zoom = 12)
#save(gm_osm, file = "data_output/tmaptools_h10_osm_natgeo.rda")
#load("data_output/tmaptools_h10_osm_natgeo.rda")
#tmap_options(max.raster = c(plot=1e6, view=1e6))
#tm_shape(gm_osm) + tm_rgb()


# Get NLCD ----------------------------------------------------------------

# devtools::install_github("ropensci/FedData")
library(FedData)

nlcd <- get_nlcd(template = h10_ls, label = "lshasta",
                 extraction.dir = "data/nlcd")
unique(getValues(nlcd))

# crop
library(stars)
nlcd_st <- read_stars("data/nlcd/lshasta_NLCD_Land_Cover_2019.tif")
st_crs(nlcd_st)
h10_ls_crop <- st_transform(h10_ls, st_crs(nlcd_st))
st_crs(nlcd_st) == st_crs(h10_ls_crop)
nlcd_crop <- st_crop(nlcd_st, h10_ls_crop)

# plot
tm_shape(nlcd_crop) + tm_raster(title = "2019 NLCD") +
   tm_shape(h10_ls) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
   tm_legend(frame=FALSE,
             legend.text.size=0.8, legend.position=c(0.01,0.03),
             legend.show=TRUE, legend.outside = FALSE) +
   tm_compass(type = "4star", position = c(0.82, 0.12), text.size = 0.5) +
   tm_scale_bar(position = c(0.77,0.05), breaks = c(0,2.5,5),
                text.size = 0.6)

tmap_save(filename = "figs/map_nlcd_2019_lshasta.png", dpi=300,
       width = 11, height = 8)

# summarize by watershed
library(raster)

e <- extract(nlcd, h10_ls_crop, method="simple")
lc_counts <- lapply(e, table)
lc_prop <- lapply(e, FUN=function(x){prop.table(table(x))}) %>% as.data.frame() %>%
   dplyr::rename(ID = x, Prcnt = Freq) %>%
   mutate(ID = as.numeric(as.character(ID)),
          Prcnt = Prcnt*100)
lc_prop

# add the labels
nlcd_labs <- as.data.frame(nlcd@data@attributes[[1]]) %>%
   filter(category!="") %>%
   mutate(category = as.character(category))
nlcd_labs

# join
lsh_nlcd_prop <- left_join(lc_prop, nlcd_labs, by=c("ID")) %>%
   arrange(Prcnt)

#View(lsh_nlcd_prop, title = "v2")

# GDE w springs -----------------------------------------------------------

# see
#tmaptools::palette_explorer()
#get_brewer_pal("Blues", n = 3, contrast = c(0.5, 0.8))
library(randomcoloR)
col4 <- randomcoloR::distinctColorPalette(k=4, runTsne = TRUE)
colblind <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
              "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#pie(rep(1, 8), col = colblind)
colblind4 <- c("#0072B2", "#009E73", "#F0E442", "#CC79A7")
#pie(rep(1, 4), col = colblind4)

# LShasta map with DEM
(loi_map <-
   # baselayer
   tm_shape(gm_osm) + tm_rgb() +
   # adjusted/revised catchments
   # tm_shape(df_catch_diss) +
   # tm_fill(col = "comid_f", alpha = 0.3, title = "Catchment COMID", legend.show = TRUE) +
   # subcatchments: all in gray dashed
  # tm_shape(catch_final) +
   #tm_polygons(border.col="gray10", alpha = 0, border.alpha = 0.9, lwd=0.2, lty=2) +
   # HUC10 boundary
   tm_shape(h10_ls) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +

   # gde
   tm_shape(gde_veg_lsh) +
   tm_fill(col = "aquamarine3", alpha = 0.8, title = "GDE", legend.show = TRUE) +
   tm_shape(gde_wet_lsh) +
   tm_fill(col = "aquamarine3", alpha = 0.8, title = "GDE", legend.show = TRUE) +
   tm_add_legend('fill', col=c('aquamarine3', 'seashell', 'darkseagreen3'), border.col='black', size=1,
                 labels=c(' GDE', " Private Land", " US Forest Service")) +

   # flowlines
   tm_shape(flowlines_map) +
   tm_lines(col="strmtype", lwd="streamcalc", scale = 4,
            # palette = "Dark2", n = 4, contrast = c(0.3, 0.9),
            palette = colblind4,
            legend.lwd.show = FALSE,
            legend.col.show = TRUE, title.col = "") +

   tm_shape(flowlines_map) +
   tm_lines(col="darkblue", lwd=0.1, scale = 4, legend.lwd.show = FALSE) +
   # tm_lines(col="darkblue", lwd="streamcalc", scale = 4, legend.lwd.show = FALSE) +
   #tm_shape(evans) + tm_lines(col="darkblue", lwd=0.5) +

   # flowlines shasta
   tm_shape(shasta_main) + tm_lines(col="darkblue", lwd=4, legend.lwd.show = FALSE) +
   #tm_text("Name", shadow = TRUE, along.lines = TRUE, auto.placement = TRUE) +

   # springs
   tm_shape(lsh_springs) +
   tm_dots(col="skyblue", size = 0.4, title = "Springs", legend.show = TRUE, shape = 21) +
   tm_shape(lsh_springs[c(1,2),]) +
   tm_text("Name", auto.placement = FALSE, col="gray10", size=0.8,
           just = "bottom", ymod = -1, xmod=2, shadow = FALSE)+
   tm_add_legend('symbol', col='skyblue', border.col='black', size=1,
                 labels=c(' Springs')) +
   # gages
   tm_shape(gages) +
   tm_dots(col="darkblue", size = 0.8, title = "Gages",
           legend.show = TRUE, legend.col="Gages", shape = 23) +
   tm_text("site_number", col = "darkblue", auto.placement = FALSE,size = 0.8,
           just = "left", ymod = 1.2, xmod=0.5, shadow = TRUE) +
   tm_add_legend('symbol',shape = 23, col='darkblue', border.col='black', size=1,
                 labels=c(' Gages')) +
   # LOI lines
   #tm_shape(loi_comid) + tm_lines(col="coral1", lwd=4) +
   tm_shape(loi_pts) + tm_dots(col="darkgoldenrod1", shape=22, size=0.5) +
   tm_text("loi_id", fontface = "bold", auto.placement = FALSE,
           bg.color = "white", bg.alpha = 0.1, col = "darkgoldenrod2",
           just = "right", xmod=-0.4, ymod=-1, shadow = TRUE) +
   tm_add_legend('symbol',shape = 22, col='darkgoldenrod1', border.col='black', size=1,
                 labels=c(' Location of Interest')) +


   # layout
   tm_legend(bg.color="white", frame=TRUE, legend.text.size=0.8, legend.position=c(0.82,0.05),
             legend.show=TRUE, legend.outside = FALSE) +
   tm_layout(#title = "Little Shasta",
     frame = FALSE,
     #fontfamily = "Roboto Condensed",
     attr.outside = FALSE)+
     #outer.margins = c(0.03,0.03, 0.03, 0.03)) +
   #legend.position = c(0.6,0.85),
   #title.position = c(0.7, 0.95)) +
   tm_compass(type = "4star", position = c(0.12, 0.12), text.size = 0.5) +
   tm_scale_bar(position = c(0.05,0.05), breaks = c(0,2.5,5),
                text.size = 0.6))

# save
#tmap_save(loi_map, filename = "figs/map_of_loi_w_gdes.jpg", height = 8.5, width = 11, units = "in", dpi = 300)
tmap_save(loi_map, filename = "figs/map_of_loi_w_gdes_w_comids_d2.jpg", height = 8.5, width = 11, units = "in", dpi = 300)



# Add Inset ---------------------------------------------------------------

# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

ca <- get_urbn_map(map = "states", sf = TRUE) %>% filter(state_abbv=="CA")

# get shasta watershed
klam <- st_read("data/shps/KlamathR.shp")
shasta <- st_read("data/shps/ShastaRwatershed.shp")

# mapview(ca, alpha.regions=0, color="black") +
#   mapview(klam, color="steelblue", lwd=5) +
#   mapview(shasta_main, color="darkblue", lwd=3) +
#   mapview(flowlines_map, color="cyan4", lwd=1) +
#   mapview(shasta, col.regions="orange", alpha.regions=0.2)

(inset_ca <- tm_shape(ca) + tm_polygons(col=NA, border.col = "black", lwd=1.5) +
  tm_shape(shasta) + tm_polygons(col = "orange", border.col = "orange", alpha = 0.5, lwd=0.1) +
  tm_shape(h10_ls) + tm_polygons(col="yellow", border.col="black", border.alpha = 0.5, lwd=0.2) +
  tm_shape(klam) + tm_lines(col = "steelblue", lwd=1.5) +
  tm_shape(shasta_main) + tm_lines(col = "darkblue", lwd=0.5, scale = 1) +
  tm_layout(frame=TRUE))

#tmap_save(inset_ca, filename = "figs/inset_map.jpg", height = 3, width = 2.5, units = "in", dpi = 300)


# make grobs
library(cowplot)
ca_grob <- tmap_grob(inset_ca)
loi_grob <- tmap_grob(loi_map)

# simple
(lsh_map <- ggdraw(loi_grob) +
  draw_plot(ca_grob,scale = 0.8,
            width = 0.25, height = 0.27,
            x = 0, y = 0.7))

cowplot::save_plot(lsh_map, filename = "figs/map_lsh_of_loi_w_gdes_w_inset.jpg",
                   base_height = 6, base_width = 11)
cowplot::save_plot(lsh_map, filename = "figs/map_lsh_of_loi_w_gdes_w_inset.pdf",
                   base_height = 6, base_width = 11)
cowplot::save_plot(lsh_map, filename = "figs/map_lsh_of_loi_w_gdes_w_inset.tiff",
                   base_height = 6, base_width = 11)


# save
# ggsave(lsh_map, filename = "figs/map_of_loi_w_gdes_w_inset.jpg",
#        width = 11, height = 7, scale = 1.1,
#        dpi=300, units = "in")

ggsave(lsh_map,
       filename = "figs/map_of_loi_w_gdes_w_inset.pdf", width = 11, height = 6, scale = 1.1,
       dpi=300, units = "in")

# tiff
tiff(filename="figs/map_of_loi_w_gdes_w_inset.tiff",
     width = 11, height = 8, type="cairo", res=300, units = "in")
lsh_map
dev.off()
