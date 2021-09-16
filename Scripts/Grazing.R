# Grazer quanity from the global grazer data set from Gilbert et al, 2018



source('~/R/startup.R') 
#c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster

state <- raster('Predictor_vars_500/WGS/STATE_WGS_500.tif'); dim(state) # 1243 x 1623
ex <- extent(state)




cow.AW <- raster('c:/users/bryce/Dropbox/GIS_data/Misc/Cattle/6_Ct_2010_Aw.tif') %>% crop(state)
cow.DA <- raster('c:/users/bryce/Dropbox/GIS_data/Misc/Cattle/5_Ct_2010_Da.tif') %>% crop(state)
area <- raster('c:/users/bryce/Dropbox/GIS_data/Misc/Cattle/8_Areakm.tif') %>% crop(state) 



cow.area <- cow.DA/area 
cow.area <- cow.area %>%
  projectRaster(res = res(state), crs = crs(state), method = 'bilinear') %>%
  resample(state) %>%
  mask(state)

plot(cow.area)

cow.rf <- cow.AW/area 
cow.rf <- cow.rf %>%
  projectRaster(res = res(state), crs = crs(state), method = 'bilinear') %>%
  resample(state) %>%
  mask(state)

plot(cow.rf)
writeRaster(cow.area, 'c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Cattle_aw_Gilbert_NGP.tif', overwrite = T)
