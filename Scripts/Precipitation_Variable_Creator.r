# Monthly climate analysis


source('c:/users/bryce/onedrive/documents/R/startup.R') 
#z:/GIS_data/Climate/daymet/Monthly

ngp2 <- readOGR(dsn = 'z:/GIS_data/NGP/Shapefile/ngp_boundary_LCC2P_WWF.shp')
ngp <- readOGR(dsn = 'c:/users/bryce/onedrive/documents/Current Projects/NGP/2019_Veg_change/Data/Shapefile/ngp_boundary_WGS.shp')
dem <- raster('c:/users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Raster/Predictor_vars_500/WGS/DEM_WGS_500.tif')


# # Calculating annual precipiation from daymet monthly data ----------------
# Also calculates MAP

# Calculate historical MAP (1980-1999) to compare with current MAP
hist <- seq(1980,1999)

for (j in hist) {
  st <- Sys.time()
  pr.ann <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% crop(ngp2) %>% sum %>% mask(ngp2)
  if (j == 1980) {pr.ann.stack <- pr.ann} else {pr.ann.stack <- stack(pr.ann.stack, pr.ann)}
  print(Sys.time()-st)
}

pr.ann.mean <- mean(pr.ann.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

# mean(pr.ann.stack) %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/MAP_hist_WGS2.tif', overwrite = T)
pr.ann.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/MAP_hist_WGS.tif', overwrite = T)



# Calculate current MAP to compare against historical as well as calculate significant changes over time
present <- seq(2000,2019)

for (j in present) {
  st <- Sys.time()
  pr.stack <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% crop(ngp2) %>% sum %>% mask(ngp2)
  if (j == 2000) {
    pr.ann.stack <- pr.stack
    pr.df <- pr.stack %>% as.data.frame(xy = T)
  } else {
    pr.ann.stack <- stack(pr.ann.stack, pr.stack)
    pr.df <- cbind.data.frame(pr.df, pr.stack %>% as.data.frame())
  }
  print(Sys.time()-st)
}
names(pr.df) <- c('x','y',present)

pr.ann.mean <- mean(pr.ann.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

pr.ann.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/MAP_00_19_WGS.tif', overwrite = T)
# writeRaster(pr.ann.stack, 'c:/users/bryce/Dropbox/GIS_data/NGP/Raster/daymet/Precip/MAP_stack.tif', overwrite = T)
# write_csv(pr.df, 'c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/NGP_annual_precipiation.csv')


# Calculating seasonal precipitation from monthly daymet data ---------------

# Calculate historical precip climatology (1980-1999)

# grow <- c(5:9)
spring <- c(4, 5, 6)
summer <- c(7, 8, 9)
winter <- c(1, 2, 3, 12)


hist <- seq(1980,1999)
for (j in hist) {
  st <- Sys.time()
  prcp.spring <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(spring) %>% crop(ngp2) %>% sum %>% mask(ngp2)
  prcp.summer <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp2) %>% sum %>% mask(ngp2)
  prcp.winter <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp2) %>% sum %>% mask(ngp2)
  
  if (j == 1980) {
    prcp.spring.stack <- prcp.spring
    prcp.summer.stack <- prcp.summer
    prcp.winter.stack <- prcp.winter
    
  } else {
    prcp.spring.stack <- stack(prcp.spring.stack, prcp.spring)
    prcp.summer.stack <- stack(prcp.summer.stack, prcp.summer)
    prcp.winter.stack <- stack(prcp.winter.stack, prcp.winter)
    
  }
  print(Sys.time()-st)
}

pr.sp.mean <- mean(prcp.spring.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

pr.su.mean <- mean(prcp.summer.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

pr.w.mean <- mean(prcp.winter.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)




# units in mm
pr.sp.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/SpringPr_hist_WGS.tif', overwrite = T)
pr.su.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/SummerPr_hist_WGS.tif', overwrite = T)
pr.w.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/WinterPr_hist_WGS.tif', overwrite = T)



# Calculate present-day precip climatology

present <- seq(2000,2019)
for (j in present) {
  st <- Sys.time()
  prcp.spring <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(spring) %>% crop(ngp2) %>% sum %>% mask(ngp2)
  prcp.summer <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp2) %>% sum %>% mask(ngp2)
  prcp.winter <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp2) %>% sum %>% mask(ngp2)
  
  if (j == 2000) {
    prcp.spring.stack <- prcp.spring
    prcp.summer.stack <- prcp.summer
    prcp.winter.stack <- prcp.winter

  } else {
    prcp.spring.stack <- stack(prcp.spring.stack, prcp.spring)
    prcp.summer.stack <- stack(prcp.summer.stack, prcp.summer)
    prcp.winter.stack <- stack(prcp.winter.stack, prcp.winter)
  }  
  print(Sys.time()-st)
}

# units in mm
pr.sp.mean <- mean(prcp.spring.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

pr.su.mean <- mean(prcp.summer.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)

pr.w.mean <- mean(prcp.winter.stack) %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)



# units in mm
pr.sp.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/SpringPr_00_19_WGS.tif', overwrite = T)
pr.su.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/SummerPr_00_19_WGS.tif', overwrite = T)
pr.w.mean %>% writeRaster('z:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/WinterPr_00_19_WGS.tif', overwrite = T)





# Calculating precipitation CV --------------------------------------------

pr <- read.csv('c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/NGP_annual_precipiation.csv')
xy <- pr[,c(1:2)]
map <- pr[,-c(1:2)]

mean <- apply(map, 1, mean, na.rm = T)
sd <- apply(map, 1, sd, na.rm = T)

pr.cv <- cbind.data.frame(xy, cv = sd/mean)

crs <- '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'
cv <- rasterFromXYZ(pr.cv, crs = crs) 

writeRaster(cv, 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/NGP_pr_CV.tif', overwrite = T)



# raster creation  --------------------------------------------------------


source('c:/users/bryce/onedrive/documents/R/startup.R') 
#z:/gis_data/NGP/Raster/Predictor_vars_500/WGS/


# Spring
MAP.pres <- raster('MAP_00_19_WGS.tif'); MAP.pres
MAP.hist <- raster('MAP_hist_WGS.tif'); MAP.hist
MAP.change <- (MAP.pres - MAP.hist)
MAP.pchange <- ((MAP.pres - MAP.hist)/MAP.hist * 100)

writeRaster(MAP.change, 'NGP_MAP_diff_WGS.tif', overwrite = T)
writeRaster(MAP.pchange, 'NGP_MAP_pdiff_WGS.tif', overwrite = T)


# Summer
summer.pres <- raster('SummerPr_00_19_WGS.tif'); summer.pres
summer.hist <- raster('SummerPr_hist_WGS.tif'); summer.hist
summer.change <- (summer.pres - summer.hist) 
# summer.pchange <- ((summer.pres - summer.hist)/summer.hist * 100)

writeRaster(summer.change, 'NGP_SummerPr_diff_WGS.tif', overwrite = T)
# writeRaster(summer.pchange, 'NGP_SummerPr_change_pct.tif', overwrite = T)


spring.pres <- raster('SpringPr_00_19_WGS.tif'); spring.pres
spring.hist <- raster('SpringPr_hist_WGS.tif'); spring.hist
spring.change <- (spring.pres - spring.hist)
# spring.pchange <- ((spring.pres - spring.hist)/spring.hist * 100)

writeRaster(spring.change, 'NGP_SpringPr_diff_WGS.tif', overwrite = T)
# writeRaster(spring.pchange, 'NGP_SpringPr_change_pct.tif', overwrite = T)


winter.pres <- raster('WinterPr_00_19_WGS.tif'); winter.pres
winter.hist <- raster('WinterPr_hist_WGS.tif'); winter.hist
winter.change <- (winter.pres - winter.hist)
# winter.pchange <- ((winter.pres - winter.hist)/winter.hist * 100)

writeRaster(winter.change, 'NGP_WinterPr_diff_WGS.tif', overwrite = T)
# writeRaster(winter.pchange, 'NGP_WinterPr_change_pct.tif', overwrite = T)

