# Maps for Tech Report

# Libraries ---------------------------------------------------------------

library(sf)
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(ggplot2)
library(fs)
library(here)
library(glue)
library(tmap)
library(tmaptools)
library(mapview)
mapviewOptions(fgb=FALSE)

# Data --------------------------------------------------------------------

# path to database:
db <- "data/cosumnes.gdb/"
DB <- "data_output/cosumnes_map.gpkg"
# layers:
st_layers(db)
st_layers(DB)

loi <- st_read(DB, "cos_loi") %>%
   # fix the names that are reversed
   mutate(LOI = case_when(
      grepl("11336000", SITE_NO) ~ "LOI 2 (McConnell)",
      grepl("11335000", SITE_NO) ~ "LOI 1 (Michigan Bar)",
   ))
loi %>% select(SITE_NO, STATION_NM, LOI) %>% View() # check!

huc8 <- st_read(DB, "cos_huc8")

cos_nhd_us_main <- st_read(DB,
                           "cos_nhd_us_main")
cos_nhd_us_tribs <- st_read(DB,
                           "cos_nhd_us_tribs")

cos_gde_veg <- st_read(DB, layer = "cos_gde_veg")

cos_gde_wet <- st_read(DB, layer = "cos_gde_wet")


# Gages -------------------------------------------------------------------

# library(nhdplusTools)
# library(dataRetrieval)
# library(purrr)
#
# # USGS GAGE flow network
# gage_cos <- findNLDI(nwis=loi$SITE_NO[1])
# loi_list <- map(loi$SITE_NO, ~list(featureSource = "", featureID=.x))
#
#
# # Get stream segments, this can take a minute
# us_main <- findNLDI(nwis=loi$SITE_NO[1], nav = "UM", find = c("basin", "flowlines"),
#                     distance_km = 140)
# us_tribs <- findNLDI(nwis=loi$SITE_NO[1], nav = "UT", find = c("flowlines"),
#                                 distance_km = 140)
#
# mapview(us_main$UM_flowlines, show.legend=FALSE) + mapview(loi) + mapview(us_tribs$UT_flowlines, lwd=0.5, color="steelblue", show.legend=FALSE) + mapview(huc8)
#

# Write Layers ------------------------------------------------------------

# write layers to db
# st_write(us_tribs$UT_flowlines,
#          dsn = "data_output/cosumnes_map.gpkg",
#          layer = "cos_nhd_us_tribs",
#          delete_layer = TRUE)
#
# st_write(us_main$UM_flowlines,
#          dsn = "data_output/cosumnes_map.gpkg",
#          layer = "cos_nhd_us_main",
#          delete_layer = TRUE)
#
# st_write(loi,
#          dsn = "data_output/cosumnes_map.gpkg",
#          layer = "cos_loi",
#          delete_layer = TRUE)
#
# st_write(huc8,
#          dsn = "data_output/cosumnes_map.gpkg",
#          layer = "cos_huc8",
#          delete_layer = TRUE)
#

# st_layers("data_output/cosumnes_map.gpkg")


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
# # crop to COS
# gde_cos_wet <- gde_wet[huc8,]
# gde_cos_veg <- gde_veg[huc8,]
#
# mapview(gde_cos_wet) + mapview(gde_cos_veg, col.regions="orange", alpha.regions=0.4)
#
# st_write(gde_cos_veg,
#          dsn = "data_output/cosumnes_map.gpkg",
#          layer = "cos_gde_veg")
# st_write(gde_cos_wet,
#          dsn = "data_output/cosumnes_map.gpkg",
#          layer = "cos_gde_wet")
#
# st_layers("data_output/cosumnes_map.gpkg")

# Raster Data -------------------------------------------------------------

# get raster data
#gm_osm <- read_osm(huc8, type = "esri-topo", raster=TRUE)
#save(gm_osm, file = "data_output/tmaptools_h10_osm_natgeo_cosumnes.rda")
load("data_output/tmaptools_h10_osm_natgeo_cosumnes.rda") # topo base layer
tmap_options(max.raster = c(plot=1e6, view=1e6))


# Get NLCD ----------------------------------------------------------------

# devtools::install_github("ropensci/FedData")
library(FedData)

# nlcd <- get_nlcd(template = huc8, label = "cos",
#                  extraction.dir = "data/nlcd")

# STARS
library(stars)
nlcd_st <- read_stars("data/nlcd/cos_NLCD_Land_Cover_2019.tif")
st_crs(nlcd_st)
nlcd_st <- droplevels(nlcd_st)
nlcd_st
plot(nlcd_st)
h8_crop <- st_transform(huc8, st_crs(nlcd_st))
st_crs(nlcd_st) == st_crs(h8_crop)
nlcd_crop <- st_crop(nlcd_st, h8_crop)
# can also crop with brackets: nlcd_st[h8_crop]
# preview
plot(nlcd_crop)

# get summary
nlcd_crop %>%
   pull %>%
   table

# plot
tm_shape(nlcd_crop) + tm_raster(title = "2019 NLCD") +
   tm_shape(huc8) +
   tm_polygons(border.col="gray30", alpha = 0, lwd=3) +
   tm_layout(frame=FALSE) +
   tm_legend(frame=FALSE,
             legend.text.size=0.8, #legend.position=c(0.01,0.93),
             legend.show=TRUE, legend.outside = TRUE) +
   tm_compass(type = "4star", position = c(0.83, 0.1), text.size = 0.5) +
   tm_scale_bar(position = c(0.7,0.08), breaks = c(0,2,5,10),
                text.size = 1)

tmap_save(filename = "figs/map_nlcd_2019_cos.png", dpi=300,
          width = 11, height = 7)

# STARS summarize by watershed --------------------

# view levels or pull as vector:
levels(nlcd_crop[[1]])
nlcd_crop %>% pull %>% levels

# just get forested areas?
forest_types <- c('Evergreen Forest', 'Deciduous Forest', 'Mixed Forest')
forest_mask <- nlcd_crop
forest_mask[!(forest_mask %in% forest_types)] <- NA
plot(forest_mask)

# combine and reduce resolution based on aggregate pixels:
nlcd_agg <- st_warp(nlcd_crop,
                    cellsize = 1500,
                    method = 'mode',
                    use_gdal = TRUE)

nlcd_agg <- droplevels(nlcd_agg)
nlcd_agg %>% pull %>% levels
levels(nlcd_agg[[1]]) <- levels(nlcd_crop[[1]])
plot(nlcd_agg)

# get most common type
cos_mode <- aggregate(nlcd_crop, h8_crop, FUN=function(x) names(which.max(table(x))))
cos_mode %>% pull %>% table
# dominant type?: cos_mode[[1]]

# get prop of watershed
cos_prop <- nlcd_crop %>% pull %>% table() %>% prop.table() %>% as.data.frame %>%
   rename(ID = 1, Prcnt=Freq) %>%
   mutate(Prcnt = Prcnt*100) %>%
   arrange(desc(Prcnt))

cos_prop
write_csv(cos_prop, file = "data_output/2019_nlcd_cos_landcover_summary.csv")
plot(nlcd_crop)

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
   tm_shape(cos_gde_veg) +
   tm_fill(col = "aquamarine3", alpha = 0.8, title = "GDE", legend.show = TRUE) +
   tm_shape(cos_gde_wet) +
   tm_fill(col = "aquamarine3", alpha = 0.8, title = "GDE", legend.show = TRUE) +
   tm_add_legend('fill', col=c('aquamarine3', 'seashell', 'darkseagreen3'),
                 border.col='black', size=1,
                 labels=c(' GDE', " Private Land", " US Forest Service")) +

   # flowlines
    tm_shape(cos_nhd_us_tribs) +
    tm_lines(col="darkblue", lwd=0.1, scale = 2, alpha = 0.8, legend.lwd.show = FALSE) +
    tm_shape(cos_nhd_us_main) +
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
   tm_legend(bg.color="white", frame=TRUE,
             legend.position=c(0.8,0.05),
             legend.show=TRUE, legend.outside = FALSE) +
   tm_layout(#title = "Little Shasta",
     frame = FALSE,
     legend.width=0.5, legend.text.size=0.8,
     #fontfamily = "Roboto Condensed",
     attr.outside = FALSE,
     inner.margins = c(0.01,0.01,0.01, 0.01),
     outer.margins = c(0.01,0.03, 0.01, 0.03)) +
   #legend.position = c(0.6,0.85),
   #title.position = c(0.7, 0.95)) +
   tm_compass(type = "4star", position = c(0.55, 0.05), text.size = 0.5) +
   tm_scale_bar(position = c(0.45,0.05), breaks = c(0,2,5,10),
                text.size = 0.7))

# save
tmap_save(loi_map, filename = "figs/map_of_loi_w_gdes.jpg", height = 8, width = 11, units = "in", dpi = 300)
#tmap_save(loi_map, filename = "figs/map_of_loi_w_gdes_w_comids_cos.jpg", height = 8.5, width = 11, units = "in", dpi = 300)



# Add Inset ---------------------------------------------------------------

# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

ca <- get_urbn_map(map = "states", sf = TRUE) %>% filter(state_abbv=="CA")


(inset_ca <- tm_shape(ca) + tm_polygons(col=NA, border.col = "black", lwd=1.5) +
  tm_shape(huc8) + tm_polygons(col="yellow", border.col="black", border.alpha = 0.5, lwd=0.2) +
  tm_shape(cos_nhd_us_main) + tm_lines(col = "darkblue", lwd=0.5, scale = 1) +
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

# cowplot save: shrinks legend?
cowplot::save_plot(lsh_map, filename = "figs/map_cos_cow_loi_w_gdes_w_inset.tiff",
                   base_height = 7.5, base_width = 11.5)

# ggplot save: shrinks width?
# ggsave(lsh_map, filename = "figs/map_cos_gg_loi_w_gdes_w_inset.jpg", width = 11.5, height = 7.5, scale = 1.1,
#        dpi=300, units = "in")

# manual
# tiff(filename="figs/map_of_loi_w_gdes_w_inset_cos.tiff",
#      width = 11, height = 7.5, type="cairo", res=300, units = "in")
# lsh_map
# dev.off()
