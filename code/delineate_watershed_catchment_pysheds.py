# watershed delineation
# using pysheds: https://github.com/mdbartos/pysheds

conda config --add channels conda-forge

# Read elevation and flow direction rasters
# ----------------------------
from pysheds.grid import Grid

grid = Grid.from_raster('n30w100_con', data_name='dem')
grid.read_raster('n30w100_dir', data_name='dir')
grid.view('dem')

## elevation to flow direction
# Determine D8 flow directions from DEM
# ----------------------
# Fill depressions in DEM
grid.fill_depressions('dem', out_name='flooded_dem')
    
# Resolve flats in DEM
grid.resolve_flats('flooded_dem', out_name='inflated_dem')
    
# Specify directional mapping
dirmap = (64, 128, 1, 2, 4, 8, 16, 32)
    
# Compute flow directions
# -------------------------------------
grid.flowdir(data='inflated_dem', out_name='dir', dirmap=dirmap)
grid.view('dir')

# Delineate a catchment
# ---------------------
# Specify pour point
x, y = -97.294167, 32.73750

# Delineate the catchment
grid.catchment(data='dir', x=x, y=y, dirmap=dirmap, out_name='catch',
               recursionlimit=15000, xytype='label')

# Crop and plot the catchment
# ---------------------------
# Clip the bounding box to the catchment
grid.clip_to('catch')
grid.view('catch')


# Calculate flow accumulation
# --------------------------
grid.accumulation(data='catch', dirmap=dirmap, out_name='acc')
grid.view('acc')


# Calculate distance to outlet from each cell
# -------------------------------------------
grid.flow_distance(data='catch', x=x, y=y, dirmap=dirmap,
                   out_name='dist', xytype='label')
grid.view('dist')


# Extract river network
# ---------------------
branches = grid.extract_river_network(fdir='catch', acc='acc',
                                      threshold=50, dirmap=dirmap)


# Convert catchment raster to vector and combine with soils shapefile
# ---------------------
# Read soils shapefile
import geopandas as gpd
from shapely import geometry, ops
soils = gpd.read_file('nrcs-soils-tarrant_439.shp')
# Convert catchment raster to vector geometry and find intersection
shapes = grid.polygonize()
catchment_polygon = ops.unary_union([geometry.shape(shape)
                                     for shape, value in shapes])
soils = soils[soils.intersects(catchment_polygon)]
catchment_soils = soils.intersection(catchment_polygon)
