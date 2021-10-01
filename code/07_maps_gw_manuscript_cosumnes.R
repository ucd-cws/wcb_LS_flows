# Maps for Tech Report

# Libraries ---------------------------------------------------------------

library(sf)
library(dplyr)
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


# path to database:
db <- "~/Downloads/cosumnes.gdb/"
# layers:
st_layers(db)

loi <- st_read(db, "LOIs_Cosumnes") %>%
  st_transform(3310)
huc8 <- st_read(db, "Cosumnes_HUC8") %>%
  st_transform(3310)



# Gages -------------------------------------------------------------------

library(nhdplusTools)
library(dataRetrieval)
library(purrr)

# USGS GAGE flow network
gage_cos <- findNLDI(nwis=loi$SITE_NO[1])
loi_list <- map(loi$SITE_NO, ~list(featureSource = "", featureID=.x))


# Get stream segments, this can take a minute
us_main <- findNLDI(nwis=loi$SITE_NO[1], nav = "UM", find = c("basin", "flowlines"),
                    distance_km = 140)
us_tribs <- findNLDI(nwis=loi$SITE_NO[1], nav = "UT", find = c("flowlines"),
                                distance_km = 140)

mapview(us_main$UM_flowlines, show.legend=FALSE) + mapview(loi) + mapview(us_tribs$UT_flowlines, lwd=0.5, color="steelblue", show.legend=FALSE) + mapview(huc8)

# GDE Data ----------------------------------------------------------------

# # Groundwater Dependent Ecosystems
st_layers("~/Downloads/i02_naturalcommunitiescommonlyassociatedwithgroundwater/i02_NCCAG_Wetlands.shp")

gde_wet <- st_read("~/Downloads/i02_naturalcommunitiescommonlyassociatedwithgroundwater/i02_NCCAG_Wetlands.shp") %>% st_zm()
gde_veg <- st_read("~/Downloads/i02_naturalcommunitiescommonlyassociatedwithgroundwater/i02_NCCAG_Vegetation.shp") %>% st_zm()

st_crs(gde_wet)$epsg
# st_crs(gde_veg)$epsg
#
# crop to COS
gde_cos_wet <- gde_wet[huc8,]
gde_cos_veg <- gde_veg[huc8,]

mapview(gde_cos_wet) + mapview(gde_cos_veg, col.regions="orange", alpha.regions=0.4)
#st_write(gde_cos_veg, dsn = db, "gde_veg_lsh")
#st_write(gde_wls, dsn = db, "gde_wet_lsh")


# Raster Data -------------------------------------------------------------

# get raster data
#gm_osm <- read_osm(huc8, type = "esri-topo", raster=TRUE)
#save(gm_osm, file = "data_output/tmaptools_h10_osm_natgeo_cosumnes.rda")
load("data_output/tmaptools_h10_osm_natgeo_cosumnes.rda") # topo base layer
tmap_options(max.raster = c(plot=1e6, view=1e6))

# GDE w springs -----------------------------------------------------------

# see
#tmaptools::palette_explorer()
#get_brewer_pal("Blues", n = 3, contrast = c(0.5, 0.8))
colblind2 <- c("#0072B2", "#F0E442")
pie(rep(1, 2), col = colblind2)

# cos map with base
(loi_map <-
   # baselayer
   tm_shape(gm_osm) + tm_rgb() +
   # subcatchments: all in white
   # HUC boundary
   tm_shape(huc8) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +

   # gde
   tm_shape(gde_cos_veg) +
   tm_fill(col = "aquamarine3", alpha = 0.8, title = "GDE", legend.show = TRUE) +
   tm_shape(gde_cos_wet) +
   tm_fill(col = "aquamarine3", alpha = 0.8, title = "GDE", legend.show = TRUE) +
   tm_add_legend('fill', col='aquamarine3', border.col='black', size=1,
                 labels=c(' GDE')) +

   # flowlines
    tm_shape(us_tribs$UT_flowlines) +
    tm_lines(col="darkblue", lwd=0.1, scale = 2, alpha = 0.8, legend.lwd.show = FALSE) +
    tm_shape(us_main$UM_flowlines) +
   tm_lines(col="darkblue", lwd=3,
            legend.lwd.show = FALSE,
            legend.col.show = TRUE, title.col = "") +

   # LOI
   tm_shape(loi) + tm_dots(col="darkgoldenrod1", shape=22, size=0.5) +
   tm_text("LOI", fontface = "bold", auto.placement = FALSE,
           bg.color = "white", bg.alpha = 0.1, col = "darkgoldenrod2",
           just = "left", xmod=1.4, ymod=-1, shadow = TRUE) +
   tm_add_legend('symbol',shape = 22, col='darkgoldenrod1', border.col='black', size=1,
                 labels=c(' Location of Interest')) +

   # layout
   tm_legend(bg.color="white", frame=FALSE, legend.text.size=0.8, legend.position=c(0.82,0.05),
             legend.show=TRUE, legend.outside = FALSE) +
   tm_layout(#title = "Little Shasta",
     frame = FALSE,
     #fontfamily = "Roboto Condensed",
     attr.outside = FALSE,
     inner.margins = c(0,0.01,0, 0.01), outer.margins = c(0.01,0.03, 0.01, 0.03)) +
   #legend.position = c(0.6,0.85),
   #title.position = c(0.7, 0.95)) +
   tm_compass(type = "4star", position = c(0.55, 0.05), text.size = 0.5) +
   tm_scale_bar(position = c(0.45,0.05), breaks = c(0,2.5,5,10),
                text.size = 0.6))

# save
#tmap_save(loi_map, filename = "figs/map_of_loi_w_gdes.jpg", height = 8.5, width = 11, units = "in", dpi = 300)
tmap_save(loi_map, filename = "figs/map_of_loi_w_gdes_w_comids_cos.jpg", height = 8.5, width = 11, units = "in", dpi = 300)



# Add Inset ---------------------------------------------------------------

# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

ca <- get_urbn_map(map = "states", sf = TRUE) %>% filter(state_abbv=="CA")


(inset_ca <- tm_shape(ca) + tm_polygons(col=NA, border.col = "black", lwd=1.5) +
  tm_shape(huc8) + tm_polygons(col="yellow", border.col="black", border.alpha = 0.5, lwd=0.2) +
  tm_shape(us_main$UM_flowlines) + tm_lines(col = "darkblue", lwd=0.5, scale = 1) +
  tm_layout(frame=TRUE))

tmap_save(inset_ca, filename = "figs/inset_map_cos.jpg", height = 3, width = 2.5, units = "in", dpi = 300)


# make grobs
library(cowplot)
ca_grob <- tmap_grob(inset_ca)
loi_grob <- tmap_grob(loi_map)

# simple
(lsh_map <- ggdraw() +
  draw_plot(loi_grob) +
  draw_plot(ca_grob,scale = 0.8,
            width = 0.25, height = 0.27,
            x = -0.03, y = 0.6))

# save
ggsave(lsh_map, filename = "figs/map_of_loi_w_gdes_w_inset_cos.jpg", width = 11, height = 8, scale = 1.1,
       dpi=300, units = "in")


ggsave(lsh_map,
       filename = "figs/map_of_loi_w_gdes_w_inset_cos.pdf", width = 11, height = 8, scale = 1.1,
       dpi=300, units = "in")

tiff(filename="figs/map_of_loi_w_gdes_w_inset_cos.tiff",
     width = 11, height = 8, type="cairo", res=300, units = "in")
lsh_map
dev.off()
