# AGDD analsis

# Monthly climate analysis


source('~/R/startup.R') 
#z:/GIS_data/NGP/Raster/NEX_AGDD

ngp.wgs <- readOGR('D:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')
agdd.files <- list.files(pattern = '.tif$', )



hist <- seq(1980,1999)
for (j in hist) {
  
  agdd <- raster(agdd.files[grep(j, agdd.files)])
  
  if (j == 1980) {
    agdd.hist.stack <- agdd
    
  } else {
    agdd.hist.stack <- stack(agdd.hist.stack, agdd)
    
  }
}

agdd.hist <- mean(agdd.hist.stack) 
agdd.hist %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_AGDD_80_99.tif', overwrite = T)



# Calculate present-day precip climatology

present <- seq(2000,2018)
for (j in present) {
  
  agdd <- raster(agdd.files[grep(j, agdd.files)])
  
  if (j == 2000) {
    agdd.pres.stack <- agdd
    
  } else {
    agdd.pres.stack <- stack(agdd.pres.stack, agdd)
    
  }  
}

agdd.pres <- mean(agdd.pres.stack) 
agdd.pres %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_AGDD_00_18.tif', overwrite = T)

agdd.diff <- agdd.pres-agdd.hist
agdd.diff %>% writeRaster('c:/users/bryce/dropbox/NGP_veg_change_2019/Raster/NGP_AGDD_change_ttl.tif', overwrite = T)

