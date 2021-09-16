# Creates the response var tifs

source('~/R/startup.R') 
# c:/users/bryce/onedrive/documents/Current Projects/NGP/2019_veg_change/Data
# packages: T


all.data <- read_csv('Veg_change_data.csv')
grass.data <- read_csv('Grass_change_data.csv')


ngp <- readOGR(dsn = 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Shapefile/ngp_boundary_WGS.shp')
ext <- extent(ngp)
r.proj <- crs(ngp)




# Grassland
grass <- grass.data %>% dplyr::select(x,y,veg) %>%
     mutate(veg = as.numeric(veg=='Grass' | veg=='Shrub')) %>%
     rasterFromXYZ(crs = r.proj)
extent(grass) <- ext
grass.m <- mask(grass, ngp)
plot(grass.m)
writeRaster(grass.m, 'c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Grasslands/Grasslands.tif', overwrite = T)


# average NDVI
ndvi_m <- grass.data %>% dplyr::select(x,y,ndvi_m) %>% rasterFromXYZ(crs = r.proj)
extent(ndvi_m) <- ext
ndvi_m.m <- mask(ndvi_m, grass.m)
summary(ndvi_m.m)
plot(ndvi_m.m)
writeRaster(ndvi_m.m, 'c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Grasslands/NDVI_mean_grass_change.tif', overwrite = T)


# Peak NDVI
ndvi <- grass.data %>% select(x,y,ndvi) %>% rasterFromXYZ(crs = r.proj)
extent(ndvi) <- ext
ndvi.m <- mask(ndvi, grass.m)
plot(ndvi.m)
writeRaster(ndvi.m, 'c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Grasslands/NDVI_peak_grass_change.tif', overwrite = T)


# LAI NDVI
lai <- grass.data %>% select(x,y,lai) %>% rasterFromXYZ(crs = r.proj)
extent(lai) <- ext
lai.m <- mask(lai, grass.m)
plot(lai.m)
writeRaster(lai.m, 'c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Grasslands/LAI_grass_change.tif', overwrite = T)


# Tree cover
tc <- grass.data %>% select(x,y,tc) %>% rasterFromXYZ(crs = r.proj)
extent(tc) <- ext
tc.m <- mask(tc, grass.m)
plot(tc.m)
writeRaster(tc.m, 'c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Grasslands/TC_grass_change.tif', overwrite = T)


# 2000 Tree cover
tc00 <- grass.data %>% select(x,y,tc00) %>% rasterFromXYZ(crs = r.proj)
extent(tc00) <- ext
tc00.m <- mask(tc00, grass.m)
plot(tc00.m)
summary(tc00.m)
writeRaster(tc00.m, 'c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Grasslands/TC00_grass.tif', overwrite = T)


# 2019 Tree cover
tc19 <- grass.data %>% select(x,y,tc19) %>% rasterFromXYZ(crs = r.proj)
extent(tc19) <- ext
tc19.m <- mask(tc19, grass.m)
plot(tc19.m)
summary(tc19.m)
writeRaster(tc19.m, 'c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Grasslands/TC19_grass.tif', overwrite = T)
