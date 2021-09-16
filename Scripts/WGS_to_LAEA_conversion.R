# LAEA conversion script


source('~/R/startup.R') 
#c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/

crs <- '+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'



# Vars:
# TC
# NDVI
# veg
# SLP
# ASP
# DEM
# BURN
# NGP_soils
# CV
# STATE

# MAT_CHELSA
# MAP_CHELSA
# MAT_dif
# MAP_dif
# pr_spring
# pr_winter
# pr_summer
# temp_winter
# temp_summer
 
# for others, see Predictor_vars_500m_aggregation.R

var <- 'veg'
method = c('bilnear', 'ngb')
method.pick <- 2

raster(paste0(var,'_WGS_500.tif')) %>%
  projectRaster(crs = crs, method = method[method.pick]) %>%
  writeRaster(paste0('../LAEA/',var,'_LAEA_500.tif'), overwrite = T)
