# look at gages2 data:

library(sf)
library(tidyverse)


# Get Data ----------------------------------------------------------------

durl <- "https://water.usgs.gov/GIS/dsdl/gagesII_9322_point_shapefile.zip"
webpath <- durl

get_shp_zip <- function(webpath){

  dest_dir <- tempdir() # make temp dir
  temp_shp <- tempfile(tmpdir = dest_dir) # make tempfile for shp
  download.file(webpath,temp_shp) # download the zipped file
  # now unzip the file depending on extension
  if( grepl('.tgz$|.tar.gz$', webpath) ){
    utils::untar(temp_shp, exdir = dest_dir)
  } else if(grepl('.zip$', webpath)){
    utils::unzip(temp_shp, exdir = dest_dir)
  } else{
    stop('not *.zip or *.tgz or *.tar.gz!')
  }
  # now get shp name with full temp path
  shp_file <- list.files(dest_dir, pattern = ".shp$", full.names=TRUE)
  # read it in!
  st_read(shp_file)

  # rm temp files

}

gages2 <- get_shp_zip(durl)

mapview::mapview(gages2)

# filter for gage of interest:
gages2_filt <- gages2 %>% filter(STAID %in% c("11516900"))

gages2_filt$CLASS # looks like it's classified as REF!
