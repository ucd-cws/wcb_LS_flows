# delineate watershed

# the catchments don't align with the HUC10, so draw boundaries based on catchments that border/with HUC10.

# Library -----------------------------------------------------------------
# see here: https://streamstats.usgs.gov/ss/
# devtools::install_github("markwh/streamstats")
# library(streamstats)
library(sf)
library(tidyverse)
library(glue)
library(here)
library(mapview)
mapviewOptions(fgb = FALSE)

# Get DataSet -------------------------------------------------------------

# gages
gages<- read_rds(file = "data_output/gages_lshasta.rds")

# path to database:
db <- glue("{here()}/data/nhdplus_little_shasta.gpkg")

# layers:
st_layers(db)

# cleaned little shasta streamlines
lshasta_clean <- read_sf(db, "lshasta_clean") %>% st_transform(3310)
st_geometry_type(lshasta_clean)

# make a single linestring version:
lshasta_linestring <- st_cast(lshasta_clean, "LINESTRING")

# cleaned catchments
lshasta_catch <- read_sf(db, "lshasta_catchments_clean") %>% st_transform(3310)
lshasta_catch_all <- read_sf(db, "catchments_ls_nhdplus18") %>% st_transform(3310)

nhd_wb <- read_sf(db, "NHDWaterBody") %>% st_transform(3310)
h10 <- read_sf(db, "h10_lshasta") %>% st_transform(3310)
basin <- read_sf(db, "basin_lshasta") %>% st_transform(3310)

# get streamline end point:
outpt <- st_line_sample(lshasta_linestring %>% filter(comid=="3917946"), sample = 1)

#mapview(outpt) + mapview(lshasta_linestring)

# Quick Mapview -----------------------------------------------------------

# quick preview maps
mapview(basin, alpha.regions=0.1, color="cyan4", layer.name="Basin") +
  #mapview(nhd_wb, alpha.regions=0.5, color="steelblue", layer.name="Lakes") +
  mapview(lshasta_clean, layer.name="Streamlines", color="darkblue") +
  mapview(lshasta_catch_all, layer.name="Catchments", color="gray20") +
  mapview(h10, alpha.regions=0.2, color="skyblue", col.regions=NA, layer.name="H10", lwd=2) +
  mapview(outpt, col.regions="red", layer.name="outflow", legend=FALSE)


# Select Catchments that Intersect or Touch with HUC10 --------------------

# we need to adjust the boundary of the HUC10 to match the catchments.
# this is important to permit dealing with re-running accumulation stats based on existing NHD landscape data.

ls_catch_revised <- lshasta_catch_all[h10,] # this selects anything that touches
ls_catch_clip <- st_intersection(lshasta_catch_all, h10) # this clips

# preview
(m1 <- mapview(ls_catch_revised, color="gray", lwd=1, lty=2, col.regions="gray", alpha.regions=0.3) +
    mapview(h10, color="maroon", lwd=2, col.regions="maroon", alpha.regions=0) +
    mapview(ls_catch_clip, color="steelblue", lwd=1, lty=2, col.regions="steelblue", alpha.regions=0))

# now select the pieces we need...but they all MATCH!
#st_intersects(ls_catch_revised, ls_catch_clip, sparse = FALSE, join=st_within)

# select only the stuff on the inside...then select anything that touches this
# from the original layer and should have what we need
ls_catch_inner <- ls_catch_revised[ls_catch_clip, , op=st_within]
ls_catch_final <- ls_catch_revised[ls_catch_inner, , op=st_intersects]

# check
(m2 <- mapview(h10, color="maroon", lwd=3, col.regions="maroon", alpha.regions=0) +
    mapview(ls_catch_inner, color="yellow", lwd=2, col.regions="yellow", alpha.regions=0.3, layer.name="inner") +
    mapview(ls_catch_revised, color="blue", lwd=1, col.regions="blue", alpha.regions=0, layer.name="orig") +
    mapview(ls_catch_final, color="black", lwd=1, col.regions="gray", alpha.regions=0.3, layer.name="final"))

# good but need to manually add a few using FEATUREID
catch_to_add <- c(3917928,3917194, 3917082, 3917100)

# bind together
ls_catch_final <- bind_rows(ls_catch_final,
                            ls_catch_revised %>% filter(FEATUREID %in% catch_to_add))

# map the final version
mapview(h10, color="maroon", lwd=3, col.regions="maroon", alpha.regions=0) +
  mapview(ls_catch_revised, color="blue", lwd=1, col.regions="blue", alpha.regions=0, layer.name="orig") +
  mapview(ls_catch_final, color="black", lwd=1, col.regions="gray", alpha.regions=0.3, layer.name="final")

# GREAT! Now save this
st_write(ls_catch_final, dsn = db, layer = "lsh_catch_final_adj", delete_layer = TRUE)
st_layers(db)

# ADJUST HUC10 to MATCH ---------------------------------------------------

# adjust the HUC10 to match
h10_adj <- rmapshaper::ms_dissolve(ls_catch_final) %>%
  select(geom) %>%
  mutate(huc10="1801020703")

mapview(h10_adj, lwd=3, color="steelblue",
        col.regions="steelblue", alpha.regions=0, layer.name="H10 Adj") +
  mapview(h10, color="maroon", lwd=3,
          col.regions="maroon", alpha.regions=0, layer.name="H10") +
  mapview(ls_catch_final, color="gray20", lwd=0.7, linetype=2, col.regions="gray",
          alpha.regions=0, layer.name="final")

# write it out!
st_write(h10_adj, dsn = db, layer = "lsh_huc10_final", delete_layer = TRUE)
st_layers(db)

# STREAMSTATS: Delineate Watershed -----------------------------------------------------

# delineate watershed based on topography using the X/Y of outflow point.

# # convert to lat lon:
# outpt <- outpt %>% st_transform(4326)
#
# # get coords
# (xloc <- st_coordinates(outpt)[1])
# (yloc <- st_coordinates(outpt)[2])
#
# ws1 <- delineateWatershed(xlocation = xloc, ylocation = yloc, crs = 4326, rcode = "CA",
#                           includeparameters = "true", includeflowtypes = "true")
#
#
# # view map of watershed:
# leafletWatershed(ws1)
#
# # data in crazy nested list:
# class(ws1$featurecollection[[2]]$feature$features[[1]]$geometry)
#
# # save out
# writeShapefile(watershed = ws1,
#                layer = "ws_boundary", dir = "data/shps", what = "boundary")
#
# # get characteristics
# chars1 <- computeChars(workspaceID = ws1$workspaceID, rcode = "CA")
# chars1$parameters
#
# stats1 <- computeFlowStats(workspaceID = ws1$workspaceID, rcode = "CA", simplify = TRUE)


## STREAMSTATS: Plot --------------------------------------------------------

lsh_ws <- st_read("data/lshasta_ss_delineation.geojson") # from streamstats

# map
mapview(lsh_ws) +
  mapview(lshasta_clean, layer.name="Streamlines", color="darkblue") +
  mapview(h10, alpha.regions=0.2, color="maroon", col.regions="maroon", layer.name="H10", lwd=2)


# Get Raster of Watershed -------------------------------------------------

library(elevatr)
library(sp)
# make an sp version of the h10
h10_sp <- as_Spatial(h10_adj)
sp::proj4string(h10_sp)


x <- elevatr::get_elev_raster(h10_sp, z = 12, clip = "locations")

# https://r-spatial.github.io/stars/index.html
# mapview(x)

library(stars)
dem <- st_as_stars(x)
class(dem)
st_crs(dem)
plot(dem, axes=TRUE)

# write out dem
write_stars(adrop(dem[1]), "data/lshasta_dem.tif")

# read in?
tst1 <- stars::read_stars("data/lshasta_dem.tif")
plot(tst1, axes=FALSE)

# Plot ggplot DEM ---------------------------------------------------------

# plot
ggplot() +
  geom_stars(data = dem[1], alpha = 0.8, downsample = 3) +
  #geom_stars(data = tst1[1], alpha = 0.8, downsample = 3) +
  viridis::scale_fill_viridis("Elev (m)", na.value="white") +
  coord_equal() +
  ggthemes::theme_map(base_family = "Roboto") +
  labs(title = "Little Shasta DEM with Streamlines",
       caption="Data from {elevatr}, z=13")+
  geom_sf(data=lshasta_clean, color="darkblue")+
  theme(legend.position = "bottom")

ggsave(filename = "figs/lshasta_dem_clean_streamline.png", width = 11, height = 8, dpi=300, units="in")


# Plot Tmap version -------------------------------------------------------

library(USAboundaries)
library(tmap)
library(tmaptools)
ca<-us_counties(states="ca")
tst1 <- stars::read_stars("data/lshasta_dem.tif")
gm_osm <- read_osm(h10_adj, type = "esri-topo", raster=TRUE)

tmap_options(max.raster = c(plot=1e8, view=1e6))

# first make CA map with no border
(map_base <-
    tm_shape(gm_osm) + tm_rgb() +
    tm_shape(ca) + tm_polygons(border.col = "purple", border.alpha = 0.3, alpha=0.1) +
    tm_shape(tst1[1],) + tm_raster(palette="viridis", alpha = 0.5, title = "DEM (m)") +
    tm_shape(lshasta_clean) + tm_lines(col="darkblue") +
    tm_layout(frame=FALSE) +
    tm_layout(title = "Little Shasta",
              frame = FALSE,
              fontfamily = "Roboto Condensed",
              legend.outside = FALSE, attr.outside = FALSE,
              inner.margins = 0.01, outer.margins = (0.01),
              #legend.position = c(0.6,0.85),
              title.position = c(0.7, 0.95)) +
    tm_compass(type = "4star", position = c("left","bottom")) +
    tm_scale_bar(position = c("left","bottom")))

# save
tmap_save(map_base, filename = "figs/map_of_h10_w_dem_overlay.png", height = 11, width = 8.5, units = "in", dpi = 300)

#cairo_pdf(filename = "output/maps/tmap_field_museum_samples_ca_topobase.pdf", height = 11, width = 8.5)
#map_base
#dev.off()
