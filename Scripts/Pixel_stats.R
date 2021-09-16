# Pixel Statistics for NGP, Greening and TC change
# Calculates changes in shrub vs glassland


source('~/R/startup.R')
#c:/Users/bryce/onedrive/documents/ngp/2019_Veg_change/

# Overall NGP stats -------------------------------------------------------
r <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/DEM_projected.tif')
df <- read.csv('c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Data/Veg_change_data.csv') %>%
     mutate(veg = factor(veg))
k <- 0.8938274^2 # cell size in km2
r.df <- r %>% as.data.frame() %>% na.exclude()




### Overall NGP ###

n.all <- nrow(r.df) # 931,565 cells in total
a.ngp <- k * n.all # total size of NGP = 744,253 sq km



sum(df$veg=='Grass', na.rm = T)/sum(!is.na(df$veg)) #54.8%
sum(df$veg=='Shrub', na.rm = T)/sum(!is.na(df$veg)) #7.7%


# Response vars -----------------------------------------------------------

source('~/R/startup.R') 
#c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster

ngp <- readOGR(dsn = 'D:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')
dem <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/DEM_WGS_500.tif')




#  ~~~~~~~~~~ Vegetation metrics ~~~~~~~~~~~~~

# tree cover year 2000
tc2000 <- fread(file = 'd:/GIS_data/NGP/Raster/TreeCover_MOD44B_V006/TC_2000_2019.csv') %>%
     dplyr::select(lon,lat, `2000`) %>% rasterFromXYZ(crs = crs(dem)) %>%
     resample(dem, method = 'ngb') %>%
     mask(ngp)

# tree cover year 2019
tc2019 <- fread(file = 'd:/GIS_data/NGP/Raster/TreeCover_MOD44B_V006/TC_2000_2019.csv') %>%
     dplyr::select(lon,lat, `2019`) %>% rasterFromXYZ(crs = crs(dem)) %>%
     resample(dem, method = 'ngb') %>%
     mask(ngp)



# peak 2000
peak2000 <- fread(file = 'd:/GIS_data/NGP/Raster/NDVI/Max/Max_NDVI_2000_2019.csv') %>%
     dplyr::select(lon,lat, `2000`) %>% rasterFromXYZ(crs = crs(dem)) %>%
     resample(dem) %>%
     mask(ngp)

# peak 2019
peak2019 <- fread(file = 'd:/GIS_data/NGP/Raster/NDVI/Max/Max_NDVI_2000_2019.csv') %>%
     dplyr::select(lon,lat, `2019`) %>% rasterFromXYZ(crs = crs(dem)) %>%
     resample(dem) %>%
     mask(ngp)



# mean 2000
mean2000 <- fread(file = 'd:/GIS_data/NGP/Raster/NDVI/Mean/Mean_NDVI_2000_2019.csv') %>%
     dplyr::select(lon,lat, `2000`) %>% rasterFromXYZ(crs = crs(dem)) %>%
     resample(dem) %>%
     mask(ngp)

# Mean 2019
mean2019 <- fread(file = 'd:/GIS_data/NGP/Raster/NDVI/Mean/Mean_NDVI_2000_2019.csv') %>%
     dplyr::select(lon,lat, `2019`) %>% rasterFromXYZ(crs = crs(dem)) %>%
     resample(dem) %>%
     mask(ngp)



# LAI 2000
lai2000 <- fread(file = 'd:/GIS_data/NGP/Raster/LAI/LAI_2000_2019.csv') %>%
     dplyr::select(lon,lat, `2000`) %>% rasterFromXYZ(crs = crs(dem)) %>%
     resample(dem) %>%
     mask(ngp)

# LAI 2019
lai2019 <- fread(file = 'd:/GIS_data/NGP/Raster/LAI/LAI_2000_2019.csv') %>%
     dplyr::select(lon,lat, `2019`) %>% rasterFromXYZ(crs = crs(dem)) %>%
     resample(dem) %>%
     mask(ngp)

# Veg
veg <- raster('Predictor_vars_500/WGS/VEG_WGS_500.tif')

veg.data <- cbind(
     as.data.frame(veg),
     as.data.frame(lai2000),
     as.data.frame(lai2019),
     as.data.frame(peak2000),
     as.data.frame(peak2019),
     as.data.frame(mean2000),
     as.data.frame(mean2019),
     as.data.frame(tc2000),
     as.data.frame(tc2019))
names(veg.data) <- c('veg', 'lai00', 'lai19', 'peak00', 'peak19', 'mean00', 'mean19', 'tc00', 'tc19')
veg.data <- veg.data %>%
     mutate(veg = factor(veg,
                         labels = c('Needleleaf', 'Deciduous', 'Mixed', 'Shrub', 'Grass', 'Crop', 'Other'))) %>%
     filter(veg == 'Grass' | veg == 'Shrub')



n.grass <- nrow(veg.data) 

# Tree cover 2000
summary(veg.data$tc00)
gm.mean(veg.data$tc00)
# Tree cover 2019
summary(veg.data$tc19)
gm.mean(veg.data$tc19)


# Peak 2000
summary(veg.data$peak00)
gm.mean(veg.data$peak00)
# Peak 2019
summary(veg.data$peak19)
gm.mean(veg.data$peak19)


# Mean 2000
summary(veg.data$mean00)
gm.mean(veg.data$mean00)
# Mean 2019
summary(veg.data$mean19)
gm.mean(veg.data$mean19)


# LAI 2000
summary(veg.data$lai00)
gm.mean(veg.data$lai00)
# LAI 2019
summary(veg.data$lai19)
gm.mean(veg.data$lai19)


#  ~~~~~~~~~~ Vegetation change metrics ~~~~~~~~~~~~~


source('~/R/startup.R')
#c:/Users/bryce/onedrive/documents/ngp/2019_Veg_change/

grass <- fread('data/Grass_change_data.csv')
n.grass <- nrow(grass)



# Tree cover change 
tcc <- grass%>% dplyr::select(tc) %>% na.exclude

n.tcc <- nrow(tcc) 
n.tcc/n.grass # 48.0

sum(tcc>0)/n.grass # 46.8
sum(tcc<=0)/n.grass # 1.2

gm.mean(tcc$tc[tcc$tc>0]) # 137.8
exp(sd(log(tcc$tc[tcc$tc>0])))
gm.mean(tcc$tc[tcc$tc<0]*-1) # -17.69
exp(sd(log(tcc$tc[tcc$tc<0]*-1)))


# peak NDVI change ~~~~~
peak <- grass %>% dplyr::select(ndvi) %>% na.exclude

n.peak <- nrow(peak) 
n.peak/n.grass # 63.0

sum(peak>0)/n.grass # 62.7
sum(peak<=0)/n.grass # 0.3

gm.mean(peak$ndvi[peak$ndvi>0]) # 9.87
exp(sd(log(peak$ndvi[peak$ndvi>0])))
gm.mean(peak$ndvi[peak$ndvi<=0]*-1) # -1.85
exp(sd(log(peak$ndvi[peak$ndvi<0]*-1)))



# mean NDVI change ~~~~~
mean <- grass %>% dplyr::select(ndvi_m) %>% na.exclude

n.mean <- nrow(mean) 
n.mean/n.grass # 58.9

sum(mean>0)/n.grass # 57.3
sum(mean<=0)/n.grass # 1.6

gm.mean(mean$ndvi_m[mean$ndvi_m>0]) # 4.18
exp(sd(log(mean$ndvi_m[mean$ndvi_m>0])))
gm.mean(mean$ndvi_m[mean$ndvi_m<=0]*-1) # -1.73
exp(sd(log(mean$ndvi_m[mean$ndvi_m<0]*-1)))


# lai change ~~~~~~
lai <- grass %>% dplyr::select(lai) %>% na.exclude

n.lai <- nrow(lai) 
n.lai/n.grass # 77.6

sum(lai>0)/n.grass # 76.0
sum(lai<=0)/n.grass # 1.6

gm.mean(lai$lai[lai$lai>0]) # 26.81
exp(sd(log(lai$lai[lai$lai>0])))
gm.mean(lai$lai[lai$lai<=0]*-1) # -3.89
exp(sd(log(lai$lai[lai$lai<0]*-1)))


# greening
gi <- grass %>%
        dplyr::select(ndvi, ndvi_m, lai)

gi$ndvi_m[gi$ndvi_m<0] <- NA
gi$ndvi[gi$ndvi<0] <- NA
gi$lai[gi$lai<0] <- NA

gi <- gi %>%
        mutate(ndvi_m = range01(ndvi_m), ndvi = range01(ndvi), lai = range01(lai)) %>%
        mutate(gi = ndvi_m + ndvi + lai) %>% 
        dplyr::select(gi) %>% 
        na.exclude()

n.gi <- nrow(gi) 
n.gi/n.grass # 77.6







# Grass/shrub -------------------------------------------------------------

shrub <- grass %>% filter(veg == 'Shrub') # 12.2% of graslands
grassgrass <- grass %>% filter(veg == 'Grass') # 87.8% of grasslands
n.grass <- nrow(grassgrass)
n.shrub <- nrow(shrub)




# ~~~~~~~~~ Tree cover change 
tcc.s <- shrub %>% dplyr::select(tc) %>% na.exclude
tcc.g <- grassgrass %>% dplyr::select(tc) %>% na.exclude

n.tcc.s <- nrow(tcc.s)
n.tcc.g <- nrow(tcc.g) 
n.tcc.s/n.shrub # 63.4
n.tcc.g/n.grass # 45.8

sum(tcc.s>0)/n.shrub # 61.6
sum(tcc.s<=0)/n.shrub # 1.8
sum(tcc.g>0)/n.grass # 44.7
sum(tcc.g<=0)/n.grass # 1.1

gm.mean(tcc.s$tc[tcc.s$tc>0]) #135.2
gm.mean(tcc.s$tc[tcc.s$tc<=0]*-1) #-30.5

gm.mean(tcc.g$tc[tcc.g$tc>0]) #138.3
gm.mean(tcc.g$tc[tcc.g$tc<=0]*-1) #-15.6


# ~~~~~~~ Peak NDVI
ndvi.s <- shrub %>% dplyr::select(ndvi) %>% na.exclude
ndvi.g <- grassgrass %>% dplyr::select(ndvi) %>% na.exclude

n.ndvi.s <- nrow(ndvi.s)
n.ndvi.g <- nrow(ndvi.g) 
n.ndvi.s/n.shrub # 78.6
n.ndvi.g/n.grass # 60.8

sum(ndvi.s>0)/n.shrub # 78.2
sum(ndvi.s<=0)/n.shrub # 0.4
sum(ndvi.g>0)/n.grass # 60.6
sum(ndvi.g<=0)/n.grass # 0.2

gm.mean(ndvi.s$ndvi[ndvi.s$ndvi>0]) # 10.1
gm.mean(ndvi.s$ndvi[ndvi.s$ndvi<=0]*-1) #-30.5

gm.mean(ndvi.g$ndvi[ndvi.g$ndvi>0]) # 9.83
gm.mean(ndvi.g$ndvi[ndvi.g$ndvi<=0]*-1) #-15.6


# ~~~~~~~~ Mean NDVI
ndvi.s <- shrub %>% dplyr::select(ndvi_m) %>% na.exclude
ndvi.g <- grassgrass %>% dplyr::select(ndvi_m) %>% na.exclude

n.ndvi.s <- nrow(ndvi.s)
n.ndvi.g <- nrow(ndvi.g) 
n.ndvi.s/n.shrub # 69.8
n.ndvi.g/n.grass # 57.3

sum(ndvi.s>0)/n.shrub # 67.5
sum(ndvi.s<=0)/n.shrub # 2.3
sum(ndvi.g>0)/n.grass # 55.8
sum(ndvi.g<=0)/n.grass # 1.5

gm.mean(ndvi.s$ndvi_m[ndvi.s$ndvi_m>0]) # 4.5
gm.mean(ndvi.s$ndvi_m[ndvi.s$ndvi_m<=0]*-1) #-2.4

gm.mean(ndvi.g$ndvi_m[ndvi.g$ndvi_m>0]) # 4.1
gm.mean(ndvi.g$ndvi_m[ndvi.g$ndvi_m<=0]*-1) #-1.6


# ~~~~~~~ LAI
lai.s <- shrub %>% dplyr::select(lai) %>% na.exclude
lai.g <- grassgrass %>% dplyr::select(lai) %>% na.exclude

n.lai.s <- nrow(lai.s)
n.lai.g <- nrow(lai.g) 
n.lai.s/n.shrub # 86.0
n.lai.g/n.grass # 76.4

sum(lai.s>0)/n.shrub # 84.1
sum(lai.s<=0)/n.shrub # 1.9
sum(lai.g>0)/n.grass # 74.9
sum(lai.g<=0)/n.grass # 1.5

gm.mean(lai.s$lai[lai.s$lai>0]) # 25.7
gm.mean(lai.s$lai[lai.s$lai<=0]*-1) #-5.4

gm.mean(lai.g$lai[lai.g$lai>0]) # 27.0
gm.mean(lai.g$lai[lai.g$lai<=0]*-1) #-3.7
