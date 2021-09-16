# Monthly climate analysis


source('c:/users/bryce/onedrive/documents/R/startup.R') 
#z:/GIS_data/Climate/daymet/Monthly

ngp <- readOGR('d:/GIS_data/NGP/Shapefile/ngp_boundary_LCC2P_WWF.shp')
ngp.wgs <- readOGR('d:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')


# # Calculating annual precipiation from daymet monthly data ----------------
# Also calculates MAP

# Calculate historical MAP (1980-1999) to compare with current MAP
hist <- seq(1980,1999)

for (j in hist) {
  st <- Sys.time()
  pr.ann <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% crop(ngp) %>% sum %>% mask(ngp)
  if (j == 1980) {pr.ann.stack <- pr.ann} else {pr.ann.stack <- stack(pr.ann.stack, pr.ann)}
  print(Sys.time()-st)
}

mean(pr.ann.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_MAP_80_99.tif', overwrite = T)



# Calculate current MAP to compare against historical as well as calculate significant changes over time
present <- seq(2000,2019)

for (j in present) {
  st <- Sys.time()
  pr.stack <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% crop(ngp) %>% sum %>% mask(ngp)
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


mean(pr.ann.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_MAP_00_19.tif', overwrite = T)
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
  prcp.spring <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(spring) %>% crop(ngp) %>% sum %>% mask(ngp)
  prcp.summer <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp) %>% sum %>% mask(ngp)
  prcp.winter <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp) %>% sum %>% mask(ngp)
  
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

# units in mm
mean(prcp.spring.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_SpringPr_80_99.tif', overwrite = T)
mean(prcp.summer.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_SummerPr_80_99.tif', overwrite = T)
mean(prcp.winter.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_WinterPr_80_99.tif', overwrite = T)



# Calculate present-day precip climatology

present <- seq(2000,2019)
for (j in present) {
  st <- Sys.time()
  prcp.spring <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(spring) %>% crop(ngp) %>% sum %>% mask(ngp)
  prcp.summer <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(summer) %>% crop(ngp) %>% sum %>% mask(ngp)
  prcp.winter <- stack(paste0('daymet_v3_prcp_monttl_',j,'_na.tif')) %>% subset(winter) %>% crop(ngp) %>% sum %>% mask(ngp)
  
  if (j == 2000) {
    prcp.spring.stack <- prcp.spring
    prcp.spring.df <- prcp.spring %>% as.data.frame(xy = T)
    prcp.summer.stack <- prcp.summer
    prcp.summer.df <- prcp.summer %>% as.data.frame(xy = T)
    prcp.winter.stack <- prcp.winter
    prcp.winter.df <- prcp.winter %>% as.data.frame(xy = T)
    
  } else {
    prcp.spring.stack <- stack(prcp.spring.stack, prcp.spring)
    prcp.spring.df <- cbind.data.frame(prcp.spring.df, prcp.spring %>% as.data.frame())
    prcp.summer.stack <- stack(prcp.summer.stack, prcp.summer)
    prcp.summer.df <- cbind.data.frame(prcp.summer.df, prcp.summer %>% as.data.frame())
    prcp.winter.stack <- stack(prcp.winter.stack, prcp.winter)
    prcp.winter.df <- cbind.data.frame(prcp.winter.df, prcp.winter %>% as.data.frame())
  }  
  print(Sys.time()-st)
}

# units in mm
mean(prcp.spring.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_SpringPr_00_19.tif', overwrite = T)
mean(prcp.summer.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_SummerPr_00_19.tif', overwrite = T)
mean(prcp.winter.stack) %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_WinterPr_00_19.tif', overwrite = T)

# writeRaster(prcp.spring.stack, 'c:/users/bryce/Dropbox/GIS_data/NGP/Raster/daymet/Precip/Pr_Spring_stack.tif', overwrite = T)
# writeRaster(prcp.summer.stack, 'c:/users/bryce/Dropbox/GIS_data/NGP/Raster/daymet/Precip/Pr_Summer_stack.tif', overwrite = T)
# writeRaster(prcp.winter.stack, 'c:/users/bryce/Dropbox/GIS_data/NGP/Raster/daymet/Precip/Pr_Winter_stack.tif', overwrite = T)
# 
# write_csv(prcp.spring.df, 'c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/NGP_spring_precipiation.csv')
# write_csv(prcp.summer.df, 'c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/NGP_summer_precipiation.csv')
# write_csv(prcp.winter.df, 'c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/NGP_winter_precipiation.csv')






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
#c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster

ngp <- readOGR('d:/GIS_data/NGP/Shapefile/ngp_boundary_LCC2P_WWF.shp')
ngp.wgs <- readOGR('d:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')



MAP.pres <- raster('NGP_MAP_00_19.tif'); plot(MAP.pres)
MAP.hist <- raster('NGP_MAP_80_99.tif'); plot(MAP.hist)
MAP.change <- (MAP.pres - MAP.hist)
# MAP.pchange <- ((MAP.pres - MAP.hist)/MAP.hist * 100) %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear')

writeRaster(MAP.change, 'NGP_MAP_change_ttl.tif', overwrite = T)
# writeRaster(MAP.pchange, 'NGP_MAP_change_pct.tif', overwrite = T)


# grow.pres <- raster('NGP_GrowPr_00_18.tif'); plot(grow.pres)
# grow.hist <- raster('NGP_GrowPr_80_99.tif'); plot(grow.hist)
# grow.change <- (grow.pres - grow.hist) %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear')
# grow.pchange <- ((grow.pres - grow.hist)/grow.hist * 100) %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear')
# 
# writeRaster(grow.change, 'NGP_GrowPr_change_ttl.tif', overwrite = T)
# writeRaster(grow.pchange, 'NGP_GrowPr_change_pct.tif', overwrite = T)


summer.pres <- raster('NGP_SummerPr_00_19.tif'); plot(summer.pres)
summer.hist <- raster('NGP_SummerPr_80_99.tif'); plot(summer.hist)
summer.change <- (summer.pres - summer.hist) 
# summer.pchange <- ((summer.pres - summer.hist)/summer.hist * 100) %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear')

writeRaster(summer.change, 'NGP_SummerPr_change_ttl.tif', overwrite = T)
# writeRaster(summer.pchange, 'NGP_SummerPr_change_pct.tif', overwrite = T)


spring.pres <- raster('NGP_SpringPr_00_19.tif'); plot(spring.pres)
spring.hist <- raster('NGP_SpringPr_80_99.tif'); plot(spring.hist)
spring.change <- (spring.pres - spring.hist)
# spring.pchange <- ((spring.pres - spring.hist)/spring.hist * 100) %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear')

writeRaster(spring.change, 'NGP_SpringPr_change_ttl.tif', overwrite = T)
# writeRaster(spring.pchange, 'NGP_SpringPr_change_pct.tif', overwrite = T)


winter.pres <- raster('NGP_WinterPr_00_19.tif'); plot(winter.pres)
winter.hist <- raster('NGP_WinterPr_80_99.tif'); plot(winter.hist)
winter.change <- (winter.pres - winter.hist)
# winter.pchange <- ((winter.pres - winter.hist)/winter.hist * 100) %>% projectRaster(crs = crs(ngp.wgs), method = 'bilinear') 

writeRaster(winter.change, 'NGP_WinterPr_change_ttl.tif', overwrite = T)
# writeRaster(winter.pchange, 'NGP_WinterPr_change_pct.tif', overwrite = T)

