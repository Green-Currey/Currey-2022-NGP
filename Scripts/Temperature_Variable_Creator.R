# Monthly temperature analysis


source('c:/users/bryce/onedrive/documents/R/startup.R') 
# z:/GIS_data/Climate/daymet/Monthly

ngp2 <- readOGR(dsn = 'z:/GIS_data/NGP/Shapefile/ngp_boundary_LCC2P_WWF.shp')
ngp <- readOGR(dsn = 'c:/users/bryce/onedrive/documents/Current Projects/NGP/2019_Veg_change/Data/Shapefile/ngp_boundary_WGS.shp')
dem <- raster('c:/users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Raster/Predictor_vars_500/WGS/DEM_WGS_500.tif')


# Calculating annual temperature from daymet monthly data ----------------
# Also calculates MAT

# Calculate historical MAT (1980-1999) to compare with current MAT
hist <- seq(1980,1999)

for (j in hist) {
  st <- Sys.time()
  tmax.ann <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tmin.ann <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tave.ann <- (tmax.ann+tmin.ann)/2
  if (j == 1980) {temp.ann.stack <- tave.ann} else {temp.ann.stack <- stack(temp.ann.stack, tave.ann)}
  print(Sys.time()-st)
}

temp.ann.mean <- mean(temp.ann.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

temp.ann.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/MAT_hist_WGS.tif', overwrite = T)




# Calculate current MAT
present <- seq(2000,2019)

for (j in present) {
  st <- Sys.time()
  tmax.ann <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tmin.ann <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tave.ann <- (tmax.ann+tmin.ann)/2

  if (j == 2000) {
    temp.ann.stack <- tave.ann
    temp.df <- tave.ann %>% as.data.frame(xy = T)
  } else {
    temp.ann.stack <- stack(temp.ann.stack, tave.ann)
  }
  print(Sys.time()-st)
}

temp.ann.mean <- mean(temp.ann.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

temp.ann.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/MAT_00_19_WGS.tif', overwrite = T)


# Calculating seasonal temperature from monthly daymet data ---------------

# Calculate historical precip (1980-1999) to compare with current precip

summer <- c(7, 8, 9)
winter <- c(1, 2, 3, 12)


hist <- seq(1980,1999)
for (j in hist) {
  st <- Sys.time()
  tmax.winter <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tmin.winter <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tave.winter <- (tmax.winter+tmin.winter)/2

  # tmax.summer <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  # tmin.summer <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  # tave.summer <- (tmax.summer+tmin.summer)/2

  if (j == 1980) {
    temp.winter.stack <- tave.winter
    # temp.summer.stack <- tave.summer
  } else {
    temp.winter.stack <- stack(temp.winter.stack, tave.winter)
    # temp.summer.stack <- stack(temp.summer.stack, tave.summer)
  }
  print(Sys.time()-st)
}

temp.w.mean <- mean(temp.winter.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

# temp.su.mean <- mean(temp.summer.stack) %>% 
#   projectRaster(dem, method = 'bilinear') %>%
#   resample(dem) %>%
#   mask(ngp)

temp.w.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/WinterTemp_hist_WGS.tif', overwrite = T)
temp.su.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/SummerTemp_hist_WGS.tif', overwrite = T)



# Calculate present precip to compare against historical precip

present <- seq(2000,2019)

for (j in present) {
  st <- Sys.time()
  tmax.winter <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tmin.winter <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tave.winter <- (tmax.winter+tmin.winter)/2
  
  tmax.summer <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tmin.summer <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp2) %>% mean %>% mask(ngp2)
  tave.summer <- (tmax.summer+tmin.summer)/2
  
  if (j == 2000) {
    temp.winter.stack <- tave.winter
    temp.summer.stack <- tave.summer
  } else {
    temp.winter.stack <- stack(temp.winter.stack, tave.winter)
    temp.summer.stack <- stack(temp.summer.stack, tave.summer)
  }  
  print(Sys.time()-st)
}

temp.w.mean <- mean(temp.winter.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

temp.su.mean <- mean(temp.summer.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

temp.w.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/WinterTemp_00_19_WGS.tif', overwrite = T)
temp.su.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/SummerTemp_00_19_WGS.tif', overwrite = T)


# raster creation  --------------------------------------------------------


source('c:/users/bryce/onedrive/documents/R/startup.R') 
#z:/gis_data/NGP/Raster/Predictor_vars_500/WGS/


# change in MAT

MAT.hist <- raster('MAT_hist_WGS.tif'); (MAT.hist)
MAT.pres <- raster('MAT_00_19_WGS.tif'); (MAT.pres)
(MAT.pres - MAT.hist) %>%
  writeRaster('NGP_MAT_diff_WGS.tif', overwrite = T)

# MAT.pchange <- (MAT.pres - MAT.hist)/MAT.hist * 100
# MAT.pchange[MAT.pchange > 10] <- 10
# MAT.pchange[MAT.pchange < -10] <- -10
# MAT.pchange %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear') %>%
#   writeRaster('NGP_MAT_change_pct.tif', overwrite = T)



# Change in summer temp

summer.hist <- raster('SummerTemp_hist_WGS.tif'); (summer.hist)
summer.pres <- raster('SummerTemp_00_19_WGS.tif'); (summer.pres)
(summer.pres - summer.hist) %>% 
  writeRaster('NGP_SummerTemp_diff_WGS.tif', overwrite = T)

# summer.pchange <- (summer.pres - summer.hist)/summer.hist * 100; plot(summer.pchange)
# summer.pchange %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear') %>%
#   writeRaster('NGP_summerTemp_change_pct.tif', overwrite = T)



# Change in winter temp

winter.hist <- raster('WinterTemp_hist_WGS.tif'); (winter.hist)
winter.pres <- raster('WinterTemp_00_19_WGS.tif'); (winter.pres)
(winter.pres - winter.hist) %>% 
  writeRaster('NGP_WinterTemp_diff_WGS.tif', overwrite = T)

# winter.pchange <- (winter.pres - winter.hist)/winter.hist * 100;
# winter.pchange[winter.pchange > 5] <- 5
# winter.pchange[winter.pchange < -50] <- -50; plot(winter.pchange)
# winter.pchange %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear') %>% 
#   writeRaster('NGP_winterTemp_change_pct.tif', overwrite = T)
