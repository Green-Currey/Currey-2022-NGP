# This script compiles all rasters into the data frame for the NGP random forest analysis


source('~/R/startup.R') 
#c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster

ngp <- readOGR(dsn = 'D:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')
dem <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/DEM_WGS_500.tif')

# Convert to WGS 500 (same as state) ---------------------------------------

# Climate -----------------------------------------------------------------




                      ### TEMPERATURE VARIABLES ###


MAT.daymet <- raster('NGP_MAT_00_19.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(MAT.daymet, 'Predictor_vars_500/WGS/MAT_daymet_WGS_500.tif', overwrite = T)

MAT.change <- raster('NGP_MAT_change_ttl.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(MAT.change, 'Predictor_vars_500/WGS/MAT_dif_WGS_500.tif', overwrite = T)

temp.summer <- raster('NGP_summerTemp_00_19.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(temp.summer, 'Predictor_vars_500/WGS/temp_summer_WGS_500.tif', overwrite = T)

temp.summer.change <- raster('NGP_summerTemp_change_ttl.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(temp.summer.change, 'Predictor_vars_500/WGS/temp_summer_dif_WGS_500.tif', overwrite = T)

temp.winter <- raster('NGP_winterTemp_00_19.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(temp.winter, 'Predictor_vars_500/WGS/temp_winter_WGS_500.tif', overwrite = T)

temp.winter.change <- raster('NGP_winterTemp_change_ttl.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(temp.winter.change, 'Predictor_vars_500/WGS/temp_winter_dif_WGS_500.tif', overwrite = T)



                                  ### PRECIPITATION VARIABLES ###


MAP.daymet <- raster('NGP_MAP_00_19.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(MAP.daymet, 'Predictor_vars_500/WGS/MAP_daymet_WGS_500.tif', overwrite = T)

MAP.change <- raster('NGP_MAP_change_ttl.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(MAP.change, 'Predictor_vars_500/WGS/MAP_dif_WGS_500.tif', overwrite = T)

pr.spring <- raster('NGP_SpringPr_00_19.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(pr.spring, 'Predictor_vars_500/WGS/pr_spring_WGS_500.tif', overwrite = T)

pr.spring.change <- raster('NGP_SpringPr_change_ttl.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(pr.spring.change, 'Predictor_vars_500/WGS/pr_spring_dif_WGS_500.tif', overwrite = T)

pr.summer <- raster('NGP_SummerPr_00_19.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(pr.summer, 'Predictor_vars_500/WGS/pr_summer_WGS_500.tif', overwrite = T)

pr.summer.change <- raster('NGP_SummerPr_change_ttl.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(pr.summer.change, 'Predictor_vars_500/WGS/pr_summer_dif_WGS_500.tif', overwrite = T)

pr.winter <- raster('NGP_WinterPr_00_18.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(pr.winter, 'Predictor_vars_500/WGS/pr_winter_WGS_500.tif', overwrite = T)

pr.winter.change <- raster('NGP_WinterPr_change_ttl.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(pr.winter.change, 'Predictor_vars_500/WGS/pr_winter_dif_WGS_500.tif', overwrite = T)

pr.cv <- raster('NGP_pr_CV.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(pr.cv, 'Predictor_vars_500/WGS/CV_WGS_500.tif', overwrite = T)



# Other Variables ---------------------------------------------------------


                              ### OTHER VARIABLES ###

# DEM
DEM <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/DEM_WGS_500.tif') %>%
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(DEM, 'Predictor_vars_500/WGS/DEM_WGS_500.tif', overwrite = T)

# roughness
rough <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/DEM_WGS_500.tif') %>%
  terrain(opt = 'roughness') %>%
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(rough, 'Predictor_vars_500/WGS/rough_WGS_500.tif', overwrite = T)

# ASP
ASP <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/ASP_WGS_500.tif') %>%
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(ASP, 'Predictor_vars_500/WGS/ASP_WGS_500.tif', overwrite = T)

# SLP
SLP <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/SLP_WGS_500.tif') %>%
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(SLP, 'Predictor_vars_500/WGS/SLP_WGS_500.tif', overwrite = T)

# Soils 
soils <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/NGP_soils_reclass.tif') %>%
  projectRaster(dem, method = 'ngb') %>%
  resample(dem, method = 'ngb') %>%
  mask(ngp)
writeRaster(soils, 'Predictor_vars_500/WGS/SOILS_WGS_500.tif', overwrite = T)


# ndep
ndep <- raster('d:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/NDEP_WGS_500.tif') %>%
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(ndep, 'Predictor_vars_500/WGS/NDEP_WGS_500.tif', overwrite = T)

# veg
veg <- raster('d:/GIS_data/NGP/Raster/Predictor_vars_500/WGS/VEG_WGS_500.tif') %>%
  projectRaster(dem, method = 'ngb') %>%
  resample(dem, method = 'ngb') %>%
  mask(ngp)
writeRaster(veg, 'Predictor_vars_500/WGS/VEG_WGS_500.tif', overwrite = T)

# Fire PA
burn <- raster('d:/GIS_data/NGP/Raster/Burn_data_MCD64A1/NGP_Burn.tif') %>%
  projectRaster(dem, method = 'ngb') %>%
  resample(dem, method = 'ngb') 
burn[is.na(burn)] <- 0
burn[burn>0] <- 1
burn <- burn %>%
  mask(ngp)
writeRaster(burn, 'Predictor_vars_500/WGS/BURN_WGS_500.tif', overwrite = T)


# Cattle from Gilbert et al
cow <- raster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/Cattle_aw_Gilbert_NGP.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(cow, 'Predictor_vars_500/WGS/COW_WGS_500.tif', overwrite = T)

# AGDD
agdd <- raster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_AGDD_00_18.tif') %>%
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(agdd, 'Predictor_vars_500/WGS/AGDD_WGS_500.tif', overwrite = T)

# AGDD dif
agdd.delta <- raster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_AGDD_change_ttl.tif') %>%
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(agdd.delta, 'Predictor_vars_500/WGS/AGDD_diff_WGS_500.tif', overwrite = T)



# response variables ------------------------------------------------------


# Tree cover change
tc <- raster('d:/GIS_data/NGP/Raster/TreeCover_MOD44B_V006/tc_NGP_ppdiff_sig.tif') %>% 
  projectRaster(dem, method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(tc, 'Predictor_vars_500/WGS/TC_WGS_500.tif', overwrite = T)

# tree cover year 2019
tc2019 <- read_csv(file = 'd:/GIS_data/NGP/Raster/TreeCover_MOD44B_V006/TC_2000_2019.csv') %>%
  dplyr::select(lon,lat, `2019`) %>% rasterFromXYZ(crs = crs(dem), method = 'ngb') %>%
  resample(dem, method = 'ngb') %>%
  mask(ngp)
writeRaster(tc2019, 'Predictor_vars_500/WGS/TC2019_WGS_500.tif', overwrite = T)

# tree cover year 2000
tc2000 <- read_csv(file = 'd:/GIS_data/NGP/Raster/TreeCover_MOD44B_V006/TC_2000_2019.csv') %>%
  dplyr::select(lon,lat, `2000`) %>% rasterFromXYZ(crs = crs(dem)) %>%
  resample(dem, method = 'ngb') %>%
  mask(ngp)
writeRaster(tc2000, 'Predictor_vars_500/WGS/TC2000_WGS_500.tif', overwrite = T)

# LAI
lai <- raster('d:/GIS_data/NGP/Raster/LAI/lai_NGP_ppdiff_sig.tif') %>%
  projectRaster(crs = crs(dem), method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(lai, 'Predictor_vars_500/WGS/LAI_WGS_500.tif', overwrite = T)

# max NDVI
ndvi.max <- raster('d:/GIS_data/NGP/Raster/NDVI/peak_ndvi_NGP_ppdiff_sig.tif') %>%
  projectRaster(crs = crs(dem), method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(ndvi.max, 'Predictor_vars_500/WGS/NDVI_WGS_500.tif', overwrite = T)

# mean NDVI
ndvi.mean <- raster('d:/GIS_data/NGP/Raster/NDVI/mean_ndvi_NGP_ppdiff_sig.tif') %>%
  projectRaster(crs = crs(dem), method = 'bilinear') %>%
  resample(dem) %>%
  mask(ngp)
writeRaster(ndvi.mean, 'Predictor_vars_500/WGS/mNDVI_WGS_500.tif', overwrite = T)




# WGS 500 Variables -------------------------------------------------------


source('~/R/startup.R') 
#c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/

# state <- raster('STATE_WGS_500.tif'); dim(state)                              #  1

mat.daymet <- raster('MAT_daymet_WGS_500.tif'); dim(mat.daymet)                 #  2
mat.dif <- raster('MAT_dif_WGS_500.tif') ; dim(mat.dif)                         #  3
temp.summer <- raster('temp_summer_WGS_500.tif'); dim(temp.summer)              #  4
temp.summer.dif <- raster('temp_summer_dif_WGS_500.tif'); dim(temp.summer.dif)  #  5
temp.winter <- raster('temp_winter_WGS_500.tif'); dim(temp.winter)              #  6
temp.winter.dif <- raster('temp_winter_dif_WGS_500.tif'); dim(temp.winter.dif)  #  7

map.daymet <- raster('MAP_daymet_WGS_500.tif'); dim(map.daymet)                 #  8
map.dif <- raster('MAP_dif_WGS_500.tif'); dim(map.dif)                          #  9
pr.spring <- raster('pr_spring_WGS_500.tif'); dim(pr.spring)                    # 10
pr.spring.dif <- raster('pr_spring_dif_WGS_500.tif'); dim(pr.spring.dif)        # 11
pr.summer <- raster('pr_summer_WGS_500.tif'); dim(pr.summer)                    # 12
pr.summer.dif <- raster('pr_summer_dif_WGS_500.tif'); dim(pr.summer.dif)        # 13
pr.winter <- raster('pr_winter_WGS_500.tif'); dim(pr.winter)                    # 14
pr.winter.dif <- raster('pr_winter_dif_WGS_500.tif'); dim(pr.winter.dif)        # 15

lai <- raster('LAI_WGS_500.tif'); dim(lai)                                      # 16
ndvi <- raster('NDVI_WGS_500.tif'); dim(ndvi)                                   # 17
ndvi.m <- raster('mNDVI_WGS_500.tif'); dim(ndvi.m)                              # 18
veg <- raster('VEG_WGS_500.tif'); dim(veg)                                      # 19
tc <- raster('TC_WGS_500.tif'); dim(tc)                                         # 20
tc19 <- raster('TC2019_WGS_500.tif'); dim(tc19)                                 # 21
tc00 <- raster('TC2000_WGS_500.tif'); dim(tc00)                                 # 22

asp <- raster('ASP_WGS_500.tif'); dim(asp)                                      # 23/24
rough <- raster('ROUGH_WGS_500.tif'); dim(rough)                                # 25
slp <- raster('SLP_WGS_500.tif'); dim(slp)                                      # 26
elv <- raster('DEM_WGS_500.tif'); dim(elv)                                      # 27
fire <- raster('BURN_WGS_500.tif'); dim(fire)                                   # 28
soil <- raster('SOILS_WGS_500.tif'); dim(soil)                                  # 29
cv <- raster('CV_WGS_500.tif'); dim(cv)                                         # 30
ndep <- raster('NDEP_WGS_500.tif'); dim(ndep)                                   # 31
agdd <- raster('AGDD_WGS_500.tif'); dim(agdd)                                   # 32
agdd.dif <- raster('AGDD_diff_WGS_500.tif'); dim(agdd.dif)                      # 33
cow <- raster('COW_WGS_500.tif'); dim(cow)                                      # 34


# aggregation into dataframe ----------------------------------------------

gc()

veg.change.data <- cbind(
  # as.data.frame(state, xy = T),
  as.data.frame(lai,xy = T),
  as.data.frame(ndvi),
  as.data.frame(ndvi.m),
  as.data.frame(tc),
  as.data.frame(tc19),
  as.data.frame(tc00),
  as.data.frame(sin(pi*asp/180)),
  as.data.frame(cos(pi*asp/180)),
  as.data.frame(slp),
  as.data.frame(elv),
  as.data.frame(rough),
  as.data.frame(soil),
  as.data.frame(veg),
  as.data.frame(fire),
  as.data.frame(pr.spring),
  as.data.frame(pr.spring.dif),
  as.data.frame(pr.summer),
  as.data.frame(pr.summer.dif),
  as.data.frame(pr.winter),
  as.data.frame(pr.winter.dif),
  as.data.frame(temp.summer),
  as.data.frame(temp.summer.dif),
  as.data.frame(temp.winter),
  as.data.frame(temp.winter.dif),
  as.data.frame(ndep),
  as.data.frame(agdd),
  as.data.frame(agdd.dif),
  as.data.frame(cow),
  as.data.frame(cv),
  as.data.frame(map.daymet),
  as.data.frame(mat.daymet),
  as.data.frame(map.dif),
  as.data.frame(mat.dif)
)

names(veg.change.data) <- c('x', 'y',
                  'lai', 'ndvi', 'ndvi_m', 'tc', 'tc19', 'tc00', 
                  'north', 'east', 'slp', 'elv', 'rough', 'soil',
                  'veg', 'fire', 'pr_sp', 'pr_sp_dif', 'pr_su', 'pr_su_dif', 'pr_w', 'pr_w_dif',
                  'tmp_su', 'tmp_su_dif', 'tmp_w', 'tmp_w_dif', 'ndep', 
                  'agdd', 'agdd_dif', 'cow', 'cv', 'map_dm', 'mat_dm', 'map_dif', 'mat_dif')

str(veg.change.data)


# altering variables ------------------------------------------------------

# factors

veg.change.data2 <- veg.change.data %>%
  mutate(veg = factor(veg,
                         labels = c('Needleleaf', 'Deciduous', 'Mixed', 'Shrub', 'Grass', 'Crop', 'Other'))) %>%
  mutate(fire = factor(fire>0, 
                          labels = c('UB', 'Burn'))) %>%
  mutate(soil = factor(round(soil),
                       labels = c('CH', 'SZ', 'RG', 'GL', 'VE', 'LU', 'BR')))
  
str(veg.change.data2)


write_csv(veg.change.data2, 'c:/Users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Data/Veg_change_data.csv')

grass.change.data <- veg.change.data2 %>% filter(veg == 'Grass' | veg == 'Shrub')
write_csv(grass.change.data, 'c:/Users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Data/Grass_change_data.csv')


