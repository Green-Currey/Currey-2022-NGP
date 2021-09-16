# NGP MODIS data change over time mann-kendall test for significance ##
# Creates change rasters

# obtaining yearly LAI ------------------------------------------------

# NDVI tif files 
source('~/R/startup.R')
# z:/GIS_data/NGP/Raster/LAI

ngp <- readOGR(dsn = 'z:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')
grass <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Grassland_layer.tif')
lai.files <- list.files(pattern = '.tif$')

yrs <- seq(2000,2019)

r1 <- raster(lai.files[1]) %>% resample(grass) %>% mask(ngp)
lai <- r1 %>% as.data.frame(xy = T)

pb <- txtProgressBar(min = 2, max = 19, style = 3)
for(i in 2:length(yrs)) {
  
  r <- raster(lai.files[i]) %>% resample(grass) %>% mask(ngp)
  
  lai <- cbind(lai, as.data.frame(r)[,1] )
  
  setTxtProgressBar(pb, i)    
}

names(lai)[1:2] <- c('lon','lat')
names(lai)[3:ncol(lai)] <- as.character(yrs)


write_csv(lai, "LAI_2000_2019.csv")



# obtaining yearly max NDVI ------------------------------------------------

# NDVI tif files 
source('~/R/startup.R')
# z:/GIS_data/NGP/Raster/NDVI/max/

ngp <- readOGR(dsn = 'z:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')
grass <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Grassland_layer.tif')
ndvi.files <- list.files(pattern = '.tif$')

yrs <- seq(2000,2019)

r1 <- raster(ndvi.files[1]) %>% resample(grass) %>% mask(ngp)
r1 <- r1/10000
peak.ndvi <- r1 %>% as.data.frame(xy = T)

pb <- txtProgressBar(min = 2, max = 19, style = 3)
for(i in 2:length(yrs)) {
  
  r <- raster(ndvi.files[i]) %>% resample(grass) %>% mask(ngp)
  r <- r/10000
  
  peak.ndvi <- cbind(peak.ndvi, as.data.frame(r)[,1] )
  
  setTxtProgressBar(pb, i)    
}

names(peak.ndvi)[1:2] <- c('lon','lat')
names(peak.ndvi)[3:ncol(peak.ndvi)] <- as.character(yrs)


write_csv(peak.ndvi, "Max_NDVI_2000_2019.csv")



# obtaining yearly mean NDVI ------------------------------------------------

# NDVI tif files 
source('~/R/startup.R')
# z:/GIS_data/NGP/Raster/NDVI/mean/

ngp <- readOGR(dsn = 'z:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')
grass <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Grassland_layer.tif')
ndvi.files <- list.files(pattern = '.tif$')

yrs <- seq(2000,2019)

r1 <- raster(ndvi.files[1]) %>% resample(grass) %>% mask(ngp)
r1 <- r1/10000
mean.ndvi <- r1 %>% as.data.frame(xy = T)

pb <- txtProgressBar(min = 2, max = 19, style = 3)
for(i in 2:length(yrs)) {
  
  r <- raster(ndvi.files[i]) %>% resample(grass) %>% mask(ngp)
  r <- r/10000
  
  mean.ndvi <- cbind(mean.ndvi, as.data.frame(r)[,1] )
  
  setTxtProgressBar(pb, i)    
}

names(mean.ndvi)[1:2] <- c('lon','lat')
names(mean.ndvi)[3:ncol(mean.ndvi)] <- as.character(yrs)


write_csv(mean.ndvi, "Mean_NDVI_2000_2019.csv")



# obtaining yearly TC data ------------------------------------------------

source('~/R/startup.R') 
#Z:\GIS_data\NGP\Raster\Veg_Cont_Fields_Yearly_250m_v6\Perc_TreeCov



ngp <- readOGR(dsn = 'z:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')
# dem <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Predictor_vars_500/WGS/DEM_WGS_500.tif')
grass <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Grassland_layer.tif')
grass[grass!=1] <- NA
tc.files <- list.files(pattern = '.tif$')

yrs <- seq(2000,2019)

r1 <- raster(tc.files[1]) %>% resample(grass, method = 'ngb') %>% mask(grass)
r1[r1>100] <- NA
tc <- r1 %>% as.data.frame(xy = T)

pb <- txtProgressBar(min = 2, max = 19, style = 3)
for(i in 2:length(yrs)) {
  
  r <- raster(tc.files[i]) %>% resample(r1, method = 'ngb') %>% mask(grass, maskvalue=1, inverse = T)
  r[r>100] <- NA
  
  tc <- cbind(tc, as.data.frame(r)[,1] )
  
  setTxtProgressBar(pb, i)    
}

names(tc)[1:2] <- c('lon','lat')
names(tc)[3:ncol(tc)] <- as.character(yrs)


# fwrite(tc, "../../TC_2000_2019.csv")
fwrite(tc, '../../../treeCover_data_2000_2020_ngp_grassland.csv')



tc2 <- apply(tc, 2, mean, na.rm = T) %>% as.data.frame
names(tc2) <- 'tc'
tc2$Year <- rownames(tc2)
tc2 <- tc2 %>% filter(tc<10) %>% filter(tc>-10)
ggplot(tc2) +
  geom_point(aes(Year, tc)) +
  geom_line(aes(Year, tc))



# Determining significance -------------------------------------------

source('~/R/startup.R')
#z:/GIS_data/NGP/Raster/

mk <- function(x) {
  mk <- apply(x, 1, MannKendall)
  df <- data.frame(matrix(unlist(mk), nrow=length(mk), byrow=T))
  df[df[,4]==0,2] <- NA
  return(df[,2])
}

pdiff.tc <- function(x) {
  x1p <- x+1
  delta <- (x1p[,ncol(x1p)] - x1p[,1])/x1p[,1]*100
  return(delta)
}

pdiff.ndvi <- function(x) {
  delta <- (x[,ncol(x)] - x[,1])/x[,1]*100
  return(delta)
}

# LAI Data
lai.data <- read_csv('LAI/LAI_2000_2019.csv')
xy <- lai.data[,1:2]
lai.data <- lai.data[,-c(1,2)]

# peak Data
peakndvi.data <- read_csv('NDVI/Max/Max_NDVI_2000_2019.csv')
peakndvi.data <- peakndvi.data[,-c(1,2)]

# mean Data
meanndvi.data <- read_csv('NDVI/Mean/Mean_NDVI_2000_2019.csv')
meanndvi.data <- meanndvi.data[,-c(1,2)]

# TC Data
tc.data <- read_csv('TreeCover_MOD44B_V006/TC_2000_2019.csv')
xy2 <- tc.data[,1:2]
tc.data <- tc.data[,-c(1,2)]



# LAI
lai.pval <- mk(lai.data)
lai.pdiff <- pdiff.ndvi(lai.data)


LAI <- cbind.data.frame(xy, lai.pdiff, lai.pval)
LAI <- LAI %>% 
  mutate(sig = as.numeric(lai.pval<=0.05))
LAI$sig[LAI$sig==0] <- NA


LAI <- LAI %>%
  mutate(pdif.sig = lai.pdiff*sig) 

LAI <- as.matrix(LAI)

summary(LAI)

fwrite(LAI, 'c:/users/bryce/onedrive/desktop/LAI_pdif_mk.csv')
rm(lai.data)
rm(LAI)
rm(lai.pval)
rm(lai.pdiff)


# peak NDVI
peakndvi.pval <- mk(peakndvi.data)
peakndvi.pdiff <- pdiff.ndvi(peakndvi.data)


peakndvi <- cbind.data.frame(xy, peakndvi.pdiff, peakndvi.pval)
peakndvi <- peakndvi %>% 
  mutate(sig = as.numeric(peakndvi.pval<=0.05))
peakndvi$sig[peakndvi$sig==0] <- NA


peakndvi <- peakndvi %>%
  mutate(pdif.sig = peakndvi.pdiff*sig) 

peakndvi <- as.matrix(peakndvi)

summary(peakndvi)

fwrite(peakndvi, 'c:/users/bryce/OneDrive/Desktop/peak_ndvi_pdif_mk.csv')
rm(peakndvi.data)
rm(peakndvi)
rm(peakndvi.pval)
rm(peakndvi.pdiff)



# mean NDVI
meanndvi.pval <- mk(meanndvi.data)
meanndvi.pdiff <- pdiff.ndvi(meanndvi.data)


meanndvi <- cbind.data.frame(xy, meanndvi.pdiff, meanndvi.pval)
meanndvi <- meanndvi %>% 
  mutate(sig = as.numeric(meanndvi.pval<=0.05))
meanndvi$sig[meanndvi$sig==0] <- NA


meanndvi <- meanndvi %>%
  mutate(pdif.sig = meanndvi.pdiff*sig) 

meanndvi <- as.matrix(meanndvi)

summary(meanndvi)

fwrite(meanndvi, 'c:/users/bryce/OneDrive/Desktop/mean_ndvi_pdif_mk.csv')
rm(meanndvi.data)
rm(meanndvi)
rm(meanndvi.pval)
rm(meanndvi.pdiff)


# tc NDVI
tc.pval <- mk(tc.data)
tc.pdiff <- pdiff.tc(tc.data)


tc <- cbind.data.frame(xy2, tc.pdiff, tc.pval)
tc <- tc %>% 
  mutate(sig = as.numeric(tc.pval<=0.05))
tc$sig[tc$sig==0] <- NA


tc <- tc %>%
  mutate(pdif.sig = tc.pdiff*sig) 


tc <- as.matrix(tc)

summary(tc)

fwrite(tc, 'c:/users/bryce/OneDrive/Desktop/tc_pdif_mk.csv')
rm(tc.data)
rm(tc)
rm(tc.pval)
rm(tc.pdiff)

# Rasters ------------------------------------------------------------------

source('~/R/startup.R') 
#z:/GIS_data/NGP/Raster/

# data
lai <- read_csv('LAI_pdif_mk.csv') %>%
  filter(pdif.sig > quantile(pdif.sig, 0.005, na.rm = T)) %>%
  filter(pdif.sig < quantile(pdif.sig, 0.995, na.rm = T))
peak <- read.csv('peak_ndvi_pdif_mk.csv') %>%
  filter(pdif.sig > quantile(pdif.sig, 0.005, na.rm = T)) %>%
  filter(pdif.sig < quantile(pdif.sig, 0.995, na.rm = T))
mean <- read.csv('mean_ndvi_pdif_mk.csv') %>%
  filter(pdif.sig > quantile(pdif.sig, 0.005, na.rm = T)) %>%
  filter(pdif.sig < quantile(pdif.sig, 0.995, na.rm = T))
TC <- read.csv('tc_pdif_mk.csv') %>%
  filter(pdif.sig > quantile(pdif.sig, 0.005, na.rm = T)) %>%
  filter(pdif.sig < quantile(pdif.sig, 0.995, na.rm = T))


ngp.boundary <- readOGR(dsn = 'z:/GIS_data/NGP/Shapefile/ngp_boundary_WGS.shp')
ext <- extent(ngp.boundary)
r.proj <- crs(ngp.boundary)





# ~~~~ LAI ~~~~~

# # % diff, all
# lai.pdif <- lai %>% dplyr::select(lon,lat, lai.pdiff) %>% 
#   rasterFromXYZ(crs = r.proj)
# extent(lai.pdif) <- ext
# lai.pdif.m <- mask(lai.pdif, ngp.boundary)
# plot(lai.pdif.m)
# writeRaster(lai.pdif.m, 'lai_ppdif_all.tif', overwrite = T)


# % diff, sig
lai.pdif.sig <- lai %>% dplyr::select(lon,lat,pdif.sig) %>%
  rasterFromXYZ(crs = r.proj)
extent(lai.pdif.sig) <- ext
lai.pdif.sig.m <- mask(lai.pdif.sig, ngp.boundary)
plot(lai.pdif.sig.m)
writeRaster(lai.pdif.sig.m, 'lai_NGP_ppdiff_sig.tif', overwrite = T)



# ~~~~ Peak NDVI ~~~~~

# # % diff, all
# peak.pdif <- peak %>% dplyr::select(lon,lat, peakndvi.pdiff) %>% 
#   rasterFromXYZ(crs = r.proj)
# extent(peak.pdif) <- ext
# peak.pdif.m <- mask(peak.pdif, ngp.boundary)
# plot(peak.pdif.m)
# writeRaster(peak.pdif.m, 'peak_ndvi_ppdif_all.tif', overwrite = T)


# % diff, sig
peak.pdif.sig <- peak %>% dplyr::select(lon,lat,pdif.sig) %>%
  rasterFromXYZ(crs = r.proj)
extent(peak.pdif.sig) <- ext
peak.pdif.sig.m <- mask(peak.pdif.sig, ngp.boundary)
plot(peak.pdif.sig.m)
writeRaster(peak.pdif.sig.m, 'peak_ndvi_NGP_ppdiff_sig.tif', overwrite = T)


# ~~~~ mean NDVI ~~~~~

# # % diff, all
# mean.pdif <- mean %>% dplyr::select(lon, lat, meanndvi.pdiff) %>% 
#   rasterFromXYZ(crs = r.proj)
# extent(mean.pdif) <- ext
# mean.pdif.m <- mask(mean.pdif, ngp.boundary)
# plot(mean.pdif.m)
# writeRaster(mean.pdif.m, 'mean_ndvi_ppdif_all.tif', overwrite = T)


# % diff, sig
mean.pdif.sig <- mean %>% dplyr::select(lon, lat, pdif.sig) %>%
  rasterFromXYZ(crs = r.proj)
extent(mean.pdif.sig) <- ext
mean.pdif.sig.m <- mask(mean.pdif.sig, ngp.boundary)
plot(mean.pdif.sig.m)
writeRaster(mean.pdif.sig.m, 'mean_ndvi_NGP_ppdiff_sig.tif', overwrite = T)


# ~~~~ tc ~~~~~

# # % diff, all
# tc.pdif <- TC %>% dplyr::select(lon,lat, tc.pdiff) %>% 
#   rasterFromXYZ(crs = r.proj)
# extent(tc.pdif) <- ext
# tc.pdif.m <- mask(tc.pdif, ngp.boundary)
# plot(tc.pdif.m)
# writeRaster(tc.pdif.m, 'tc_ppdif_all.tif', overwrite = T)


# % diff, sig
tc.pdif.sig <- TC %>% dplyr::select(lon,lat,pdif.sig) %>%
  rasterFromXYZ(crs = r.proj)
extent(tc.pdif.sig) <- ext
tc.pdif.sig.m <- mask(tc.pdif.sig, ngp.boundary)
plot(tc.pdif.sig.m)
writeRaster(tc.pdif.sig.m, 'tc_NGP_ppdiff_sig.tif', overwrite = T)
