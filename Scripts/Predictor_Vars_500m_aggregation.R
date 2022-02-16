# This script compiles all rasters into the data frame for the NGP random forest analysis


source('~/R/startup.R') 
#z:/gis_data/NGP/raster/predictor_vars_500/wgs

ngp <- readOGR(dsn = 'c:/users/bryce/onedrive/documents/Current Projects/NGP/2019_Veg_change/Data/Shapefile/ngp_boundary_WGS.shp')
dem <- raster('DEM_WGS_500.tif')


# Climate -----------------------------------------------------------------




                      ### TEMPERATURE VARIABLES ###


mat <- raster('MAT_00_19_WGS.tif') 
mat.hist <- raster('MAT_hist_WGS.tif') 
mat.dif <- raster('NGP_MAT_diff_WGS.tif')

temp.summer <- raster('SummerTemp_00_19_WGS.tif')
temp.summer.hist <- raster('SummerTemp_hist_WGS.tif')
temp.summer.dif <- raster('NGP_SummerTemp_diff_WGS.tif')

temp.winter <- raster('WinterTemp_00_19_WGS.tif') 
temp.winter.hist <- raster('WinterTemp_hist_WGS.tif')
temp.winter.dif <- raster('NGP_WinterTemp_diff_WGS.tif') 

compareRaster(dem, mat, mat.dif, mat.hist,
              temp.summer, temp.summer.dif, temp.summer.hist,
              temp.winter, temp.winter.dif, temp.winter.hist)

                                  

                          ### PRECIPITATION VARIABLES ###

map <- raster('MAP_00_19_WGS.tif')
map.hist <- raster('MAP_hist_WGS.tif')
map.dif <- raster('NGP_MAP_diff_WGS.tif')

pr.spring <- raster('SpringPr_00_19_WGS.tif') 
pr.spring.hist <- raster('SpringPr_hist_WGS.tif')
pr.spring.dif <- raster('NGP_SpringPr_diff_WGS.tif') 

pr.summer <- raster('SummerPr_00_19_WGS.tif') 
pr.summer.hist <- raster('SummerPr_hist_WGS.tif')
pr.summer.dif <- raster('NGP_SummerPr_diff_WGS.tif') 

pr.winter <- raster('WinterPr_00_19_WGS.tif')
pr.winter.hist <- raster('WinterPr_hist_WGS.tif')
pr.winter.dif <- raster('NGP_WinterPr_diff_WGS.tif') 

pr.cv <- raster('CV_WGS_500.tif') 

compareRaster(dem, map, map.dif,
              pr.spring, pr.spring.dif, pr.spring.hist, 
              pr.summer, pr.summer.dif, pr.summer.hist, 
              pr.winter, pr.winter.dif, pr.winter.hist,
              pr.cv)



# Other Variables ---------------------------------------------------------

                              ### OTHER VARIABLES ###

# roughness
rough <- raster('rough_WGS_500.tif')
asp <- raster('ASP_WGS_500.tif')
slp <- raster('SLP_WGS_500.tif')
soils <- raster('SOILS_WGS_500.tif')
ndep <- raster('NDEP_WGS_500.tif') 
grass <- raster('GRASS_WGS_500.tif') 
burn <- raster('BURN_WGS_500.tif') 
cow <- raster('COW_WGS_500.tif')

agdd <- raster('AGDD_WGS_500.tif')
agdd.hist <- raster('AGDD_hist_WGS_500.tif') 
agdd.dif <- raster('AGDD_diff_WGS_500.tif')

compareRaster(dem, rough, asp, slp, soils, ndep, grass, burn, cow, 
              agdd, agdd.dif, agdd.hist)



# response variables ------------------------------------------------------


# Tree cover change
tc <- raster('TC_WGS_500.tif')
# tree cover year 2019
tc19 <- raster('TC2019_WGS_500.tif') 
# tree cover year 2000
tc00 <- raster('TC2000_WGS_500.tif') 

# LAI
lai <- raster('LAI_WGS_500.tif') 
# max NDVI
ndvi <- raster('NDVI_WGS_500.tif') 
# mean NDVI
mndvi <- raster('mNDVI_WGS_500.tif')

compareRaster(dem, tc, tc00, tc19, lai, ndvi, mndvi)



# aggregation into dataframe ----------------------------------------------

# ALL data

veg.change.data <- cbind(
  as.data.frame(lai, xy = T),
  as.data.frame(ndvi),
  as.data.frame(mndvi),
  as.data.frame(tc),
  as.data.frame(tc19),
  as.data.frame(tc00),
  as.data.frame(sin(pi*asp/180)),
  as.data.frame(cos(pi*asp/180)),
  as.data.frame(slp),
  as.data.frame(dem),
  as.data.frame(rough),
  as.data.frame(soils),
  as.data.frame(grass),
  as.data.frame(burn),
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
  as.data.frame(pr.cv),
  as.data.frame(map),
  as.data.frame(mat),
  as.data.frame(map.dif),
  as.data.frame(mat.dif)
)

names(veg.change.data) <- c('x', 'y',
                  'lai', 'ndvi', 'ndvi_m', 'tc', 'tc19', 'tc00', 
                  'north', 'east', 'slp', 'elv', 'rough', 'soil',
                  'grass', 'fire', 'pr_sp', 'pr_sp_dif', 'pr_su', 'pr_su_dif', 'pr_w', 'pr_w_dif',
                  'tmp_su', 'tmp_su_dif', 'tmp_w', 'tmp_w_dif', 'ndep', 
                  'agdd', 'agdd_dif', 'cow', 'cv', 'map_dm', 'mat_dm', 'map_dif', 'mat_dif')

str(veg.change.data)


# For TC2000

veg.change.data2 <- cbind(
  as.data.frame(tc00,xy=T),
  as.data.frame(sin(pi*asp/180)),
  as.data.frame(cos(pi*asp/180)),
  as.data.frame(slp),
  as.data.frame(dem),
  as.data.frame(rough),
  as.data.frame(soils),
  as.data.frame(grass),
  as.data.frame(burn),
  as.data.frame(pr.spring.hist),
  as.data.frame(pr.summer.hist),
  as.data.frame(pr.winter.hist),
  as.data.frame(temp.summer.hist),
  as.data.frame(temp.winter.hist),
  as.data.frame(ndep),
  as.data.frame(agdd.hist),
  as.data.frame(cow),
  as.data.frame(pr.cv),
  as.data.frame(map.hist),
  as.data.frame(mat.hist)
)

names(veg.change.data2) <- c('x', 'y',
                            'tc00', 
                            'north', 'east', 'slp', 'elv', 'rough', 'soil',
                            'grass', 'fire', 'pr_sp', 'pr_su', 'pr_w',
                            'tmp_su', 'tmp_w', 'ndep', 
                            'agdd', 'cow', 'cv', 'map', 'mat')

str(veg.change.data2)


# altering variables ------------------------------------------------------

# factors

veg.change.data.fin <- veg.change.data %>%
  mutate(grass = factor(grass,
                         labels = c('Other', 'Grass'))) %>%
  mutate(fire = factor(fire>0, 
                          labels = c('UB', 'Burn'))) %>%
  mutate(soil = factor(round(soil),
                       labels = c('CH', 'SZ', 'RG', 'GL', 'VE', 'LU', 'BR')))
  
str(veg.change.data.fin)


veg.change.data2.fin <- veg.change.data2 %>%
  mutate(grass = factor(grass,
                        labels = c('Other', 'Grass'))) %>%
  mutate(fire = factor(fire>0, 
                       labels = c('UB', 'Burn'))) %>%
  mutate(soil = factor(round(soil),
                       labels = c('CH', 'SZ', 'RG', 'GL', 'VE', 'LU', 'BR')))

str(veg.change.data2.fin)


fwrite(veg.change.data.fin, 'c:/Users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Veg_change_data.csv')
fwrite(veg.change.data2.fin, 'c:/Users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Veg_change_data2.csv')

grass.change.data <- veg.change.data.fin %>% filter(grass == 'Grass')
write_csv(grass.change.data, 'c:/Users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Grass_change_data.csv')

grass.change.data2 <- veg.change.data2.fin %>% filter(grass == 'Grass')
write_csv(grass.change.data2, 'c:/Users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Grass_change_data2.csv')


