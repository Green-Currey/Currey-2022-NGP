# Monthly temperature analysis


source('c:/users/bryce/onedrive/documents/R/startup.R') 
#D:/GIS_data/Climate/daymet/Monthly

ngp <- readOGR('d:/GIS_data/NGP/Shapefile/ngp_boundary_LCC2P_WWF.shp')
ngp.wgs <- readOGR('d:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')


# Calculating annual temperature from daymet monthly data ----------------
# Also calculates MAT

# Calculate historical MAT (1980-1999) to compare with current MAT
hist <- seq(1980,1999)

for (j in hist) {
  st <- Sys.time()
  tmax.ann <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% crop(ngp) %>% mean %>% mask(ngp)
  tmin.ann <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% crop(ngp) %>% mean %>% mask(ngp)
  tave.ann <- (tmax.ann+tmin.ann)/2
  if (j == 1980) {temp.ann.stack <- tave.ann} else {temp.ann.stack <- stack(temp.ann.stack, tave.ann)}
  print(Sys.time()-st)
}

mean(temp.ann.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_MAT_80_99.tif', overwrite = T)



# Calculate current MAT
present <- seq(2000,2019)

for (j in present) {
  st <- Sys.time()
  tmax.ann <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% crop(ngp) %>% mean %>% mask(ngp)
  tmin.ann <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% crop(ngp) %>% mean %>% mask(ngp)
  tave.ann <- (tmax.ann+tmin.ann)/2

  if (j == 2000) {
    temp.ann.stack <- tave.ann
    temp.df <- tave.ann %>% as.data.frame(xy = T)
  } else {
    temp.ann.stack <- stack(temp.ann.stack, tave.ann)
    temp.df <- cbind.data.frame(temp.df, tave.ann %>% as.data.frame())
  }
  print(Sys.time()-st)
}

mean(temp.ann.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_MAT_00_19.tif', overwrite = T)
# writeRaster(temp.ann.stack, 'c:/users/bryce/Dropbox/GIS_data/NGP/Raster/daymet/Precip/MAT_stack.tif', overwrite = T)
# write.csv(temp.df, 'c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/NGP_annual_temperature.csv')


# Calculating seasonal temperature from monthly daymet data ---------------

# Calculate historical precip (1980-1999) to compare with current precip

summer <- c(7, 8, 9)
winter <- c(1, 2, 3, 12)


# hist <- seq(1980,1999)
# for (j in hist) {
#   st <- Sys.time()
#   tmax.winter <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp) %>% mean %>% mask(ngp)
#   tmin.winter <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp) %>% mean %>% mask(ngp)
#   tave.winter <- (tmax.winter+tmin.winter)/2
#   
#   tmax.summer <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp) %>% mean %>% mask(ngp)
#   tmin.summer <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp) %>% mean %>% mask(ngp)
#   tave.summer <- (tmax.summer+tmin.summer)/2
#   
#   if (j == 1980) {
#     temp.winter.stack <- tave.winter
#     temp.summer.stack <- tave.summer
#   } else {
#     temp.winter.stack <- stack(temp.winter.stack, tave.winter)
#     temp.summer.stack <- stack(temp.summer.stack, tave.summer)
#   }
#   print(Sys.time()-st)
# }
# print(j)
# mean(temp.winter.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_winterTemp_80_99.tif', overwrite = T)
# mean(temp.summer.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_summerTemp_80_99.tif', overwrite = T)



# Calculate present precip to compare against historical precip

present <- seq(2000,2019)

for (j in present) {
  st <- Sys.time()
  tmax.winter <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp) %>% mean %>% mask(ngp)
  tmin.winter <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp) %>% mean %>% mask(ngp)
  tave.winter <- (tmax.winter+tmin.winter)/2
  
  tmax.summer <- stack(paste0('daymet_v3_tmax_monavg_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp) %>% mean %>% mask(ngp)
  tmin.summer <- stack(paste0('daymet_v3_tmin_monavg_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp) %>% mean %>% mask(ngp)
  tave.summer <- (tmax.summer+tmin.summer)/2
  
  if (j == 2000) {
    temp.winter.stack <- tave.winter
    temp.winter.df <- tave.winter %>% as.data.frame(xy = T)
    temp.summer.stack <- tave.summer
    temp.summer.df <- tave.summer %>% as.data.frame(xy = T)
  } else {
    temp.winter.stack <- stack(temp.winter.stack, tave.winter)
    temp.winter.df <- cbind.data.frame(temp.winter.df, tave.winter %>% as.data.frame())
    temp.summer.stack <- stack(temp.summer.stack, tave.summer)
    temp.summer.df <- cbind.data.frame(temp.summer.df, tave.summer %>% as.data.frame())
  }  
  print(Sys.time()-st)
}
print(j)
# units in mm/mo
mean(temp.winter.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_winterTemp_00_19.tif', overwrite = T)
mean(temp.summer.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_summerTemp_00_19.tif', overwrite = T)

# writeRaster(temp.winter.stack, 'c:/users/bryce/Dropbox/GIS_data/NGP/Raster/daymet/Precip/temp_winter_stack.tif', overwrite = T)
# writeRaster(temp.summer.stack, 'c:/users/bryce/Dropbox/GIS_data/NGP/Raster/daymet/Precip/temp_summer_stack.tif', overwrite = T)
# 
# write_csv(temp.winter.df, 'c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/NGP_winter_temperature.csv')
# write_csv(temp.summer.df, 'c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/NGP_summer_temperature.csv')



# # Change over time analysis -----------------------------------------------
# 
# all <- read.csv('c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Data/NGP_annual_precipiation.csv')
# grow <- read.csv('c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Data/NGP_grow_precipiation.csv')
# spring <- read.csv('c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Data/NGP_spring_precipiation.csv')
# summer <- read.csv('c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Data/NGP_summer_precipiation.csv')
# 
# 
# names(grow) <- c('x','y', seq(2000,2018))
# names(spring) <- c('x','y', seq(2000,2018))
# names(summer) <- c('x','y', seq(2000,2018))
# names(all) <- c('x','y', seq(2000,2018))
# xy <- all[,1:2]
# 
# all <- all[,-c(1,2)]
# grow <- grow[,-c(1,2)]
# spring <- spring[,-c(1,2)]
# summer <- summer[,-c(1,2)]
# 
# 
# slope.all <- rep(0, times = nrow(xy))
# slope.grow <- rep(0, times = nrow(xy))
# slope.spring <- rep(0, times = nrow(xy))
# slope.summer <- rep(0, times = nrow(xy))
# 
# pval.all <-  rep(0, times = nrow(xy))
# pval.grow <-  rep(0, times = nrow(xy))
# pval.spring <-  rep(0, times = nrow(xy))
# pval.summer <-  rep(0, times = nrow(xy))
# 
# percent.diff.all <- rep(0, times = nrow(xy))
# percent.diff.grow <- rep(0, times = nrow(xy))
# percent.diff.spring <- rep(0, times = nrow(xy))
# percent.diff.summer <- rep(0, times = nrow(xy))
# 
# 
# pb <- txtProgressBar(char = '-', min = 1, max = nrow(xy), style = 3)
# for (i in seq(nrow(xy))){
#   
#   all.i <- all[i,] %>% as.numeric()
#   grow.i <- grow[i,] %>% as.numeric()
#   spring.i <- spring[i,] %>% as.numeric()
#   summer.i <- summer[i,] %>% as.numeric()
#   
#   na.test.all <-  sum(is.na(all.i))==length(all.i)
#   na.test.grow <-  sum(is.na(grow.i))==length(grow.i)
#   na.test.spring <-  sum(is.na(spring.i))==length(spring.i)
#   na.test.summer <- sum(is.na(summer.i))==length(summer.i)
#   
#   
#   
#   # Linear Slopes
#   
#   slope.all[i] <- ifelse(
#     test = na.test.all,
#     yes = NA,
#     no = coefficients(lm(all.i~seq(19)))[2]
#   )
#   
#   slope.grow[i] <- ifelse(
#     test = na.test.grow,
#     yes = NA,
#     no = coefficients(lm(grow.i~seq(19)))[2]
#   )
#   
#   slope.summer[i] <- ifelse(
#     test = na.test.spring,
#     yes = NA,
#     no = coefficients(lm(summer.i~seq(19)))[2]
#   )
#   
#   slope.spring[i] <- ifelse(
#     test = na.test.summer,
#     yes = NA,
#     no = coefficients(lm(spring.i~seq(19)))[2]
#   )
#   
#   
#   
#   
#   # Mann-Kendall pval
#   
#   pval.all[i] <- ifelse(
#     test = na.test.all,
#     yes = NA,
#     no = MannKendall(all.i)$sl %>% as.numeric
#   )
#   
#   pval.grow[i] <- ifelse(
#     test = na.test.grow,
#     yes = NA,
#     no = MannKendall(grow.i)$sl %>% as.numeric
#   )
#   
#   pval.spring[i] <- ifelse(
#     test = na.test.spring,
#     yes = NA,
#     no = MannKendall(spring.i)$sl %>% as.numeric
#   )
#   
#   pval.summer[i] <- ifelse(
#     test = na.test.summer,
#     yes = NA,
#     no = MannKendall(summer.i)$sl %>% as.numeric
#   )
#   
#   
#   
#   # percent difference between 2018 and 2000
#   
#   percent.diff.all[i] <- ifelse(
#     test = na.test.all,
#     yes = NA,
#     no = (all.i[length(all.i)] - all.i[1])/all.i[1]*100
#   )
#   
#   percent.diff.grow[i] <- ifelse(
#     test = na.test.grow,
#     yes = NA,
#     no = (grow.i[length(grow.i)] - grow.i[1])/grow.i[1]*100
#   )
#   
#   percent.diff.spring[i] <- ifelse(
#     test = na.test.spring,
#     yes = NA,
#     no = (spring.i[length(spring.i)] - spring.i[1])/spring.i[1]*100
#   )
#   
#   percent.diff.summer[i] <- ifelse(
#     test = na.test.summer,
#     yes = NA,
#     no = (summer.i[length(summer.i)] - summer.i[1])/summer.i[1]*100
#   )
#   
#   setTxtProgressBar(pb, i)
# }
# close(pb)
# 
# 
# 
# precipitation <- cbind(xy,
#                        percent.diff.all, percent.diff.grow, percent.diff.spring, percent.diff.summer,
#                        pval.all, pval.grow, pval.spring, pval.summer,
#                        slope.all, slope.grow, slope.spring, slope.summer)
# 
# 
# write_csv(precipitation, 'c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Data/NGP_precip_change.csv')
# 
# 
# 

# raster creation  --------------------------------------------------------


source('c:/users/bryce/onedrive/documents/R/startup.R') 
#c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster


ngp <- readOGR('d:/GIS_data/NGP/Shapefile/ngp_boundary_LCC2P_WWF.shp')
ngp.wgs <- readOGR('d:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')



# change in MAT

MAT.hist <- raster('NGP_MAT_80_99.tif'); plot(MAT.hist)
MAT.pres <- raster('NGP_MAT_00_19.tif'); plot(MAT.pres)
(MAT.pres - MAT.hist) %>%
  writeRaster('NGP_MAT_change_ttl.tif', overwrite = T)

# MAT.pchange <- (MAT.pres - MAT.hist)/MAT.hist * 100
# MAT.pchange[MAT.pchange > 10] <- 10
# MAT.pchange[MAT.pchange < -10] <- -10
# MAT.pchange %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear') %>%
#   writeRaster('NGP_MAT_change_pct.tif', overwrite = T)



# Change in summer temp

summer.hist <- raster('NGP_summerTemp_80_99.tif'); plot(summer.hist)
summer.pres <- raster('NGP_summerTemp_00_19.tif'); plot(summer.pres)
(summer.pres - summer.hist) %>% 
  writeRaster('NGP_summerTemp_change_ttl.tif', overwrite = T)

# summer.pchange <- (summer.pres - summer.hist)/summer.hist * 100; plot(summer.pchange)
# summer.pchange %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear') %>%
#   writeRaster('NGP_summerTemp_change_pct.tif', overwrite = T)



# Change in winter temp

winter.hist <- raster('NGP_winterTemp_80_99.tif'); plot(winter.hist)
winter.pres <- raster('NGP_winterTemp_00_19.tif'); plot(winter.pres)
(winter.pres - winter.hist) %>% 
  writeRaster('NGP_winterTemp_change_ttl.tif', overwrite = T)

# winter.pchange <- (winter.pres - winter.hist)/winter.hist * 100;
# winter.pchange[winter.pchange > 5] <- 5
# winter.pchange[winter.pchange < -50] <- -50; plot(winter.pchange)
# winter.pchange %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear') %>% 
#   writeRaster('NGP_winterTemp_change_pct.tif', overwrite = T)
