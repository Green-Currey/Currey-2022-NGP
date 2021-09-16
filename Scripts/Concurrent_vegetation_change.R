# 1. Bivariate models and figures between greening variables and tree cover.
# 2. Creates a tif of the greening index
# 3. Examines coocurrence between greening metrics and tree cover (Figure 6 and S7)


source('~/R/startup.R') 
#c:/users/bryce/onedrive/documents/Current Projects/NGP/2019_Veg_change/Data



# Concurrent Models -------------------------------------------------------


## ---------- peak NDVI

peak <- fread('Grass_change_data.csv') %>%
  dplyr::select(c(ndvi,tc)) %>%
  na.exclude


summary(lm(ndvi ~ tc, data = peak)) #9.7% explained
confint(lm(ndvi ~ tc, data = peak))
# for a doubling in percent tree cover, peak NDVI decreases by 1.28% +/- 0.01%


xrange1 <- quantile(peak$tc, c(0.025, 0.975), na.rm = T) %>% as.numeric

peak <- peak %>% filter(tc < xrange1[2] & tc > xrange1[1])


figure(
  ggplot(peak) +
    aes(x = tc, y = ndvi) +
    geom_smooth(color = 'black', method = 'lm') +
    geom_smooth(color = 'red3', method = 'gam', formula = y ~ s(x, k = 6, bs = 'cr')) +
    # geom_vline(xintercept = 0) +
    theme_bw(base_size = 28) +
    # geom_rug(sides = c('bl'), alpha = 0.01) +
    # coord_cartesian(xlim = c(NA,420)) +
    labs(x = bquote('Tree Cover'~Delta~'(%)'), y = bquote('Peak NDVI'~Delta~'(%)')),
  path.name = 'c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Figures/NDVI_TC_relationship.tif',
  height = 6, width = 6,
  save = F
)



## ---------- mean NDVI

mean <- fread('Grass_change_data.csv') %>%
  dplyr::select(c(ndvi_m, tc)) %>%
  na.exclude

summary(lm(ndvi_m ~ tc, data = mean)) #19.7% explained
confint(lm(ndvi_m ~ tc, data = mean))
# where cooccurrence occurrs, for a doubling in percent tree cover, mean NDVI increases by 2.62% +/- 0.07%


xrange2 <- quantile(mean$tc, c(0.025, 0.975), na.rm = T) %>% as.numeric

mean <- mean %>% filter(tc < xrange2[2] & tc > xrange2[1])


figure(
  ggplot(mean) +
    aes(x = tc, y = ndvi_m) +
    geom_smooth(color = 'black', method = 'lm') +
    geom_smooth(color = 'red3', method = 'gam', formula = y ~ s(x, k = 6, bs = 'cr')) +
    # geom_hline(yintercept = 0) +
    # geom_vline(xintercept = 0) +
    theme_bw(base_size = 28) +
    # geom_rug(sides = c('bl'), alpha = 0.01) +
    # coord_cartesian(xlim = c(NA,420)) +
    # scale_x_continuous(breaks = c(0,5)) +
    labs(x = bquote('Tree Cover'~Delta~'(%)'), y = bquote('Mean NDVI'~Delta~'(%)')),
  path.name = 'c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Figures/meanNDVI_TC_relationship.tif',
  height = 6, width = 6,
  save = F
)


## ---------- LAI

lai <- fread('Grass_change_data.csv') %>%
  dplyr::select(c(lai, tc)) %>%
  na.exclude

summary(lm(lai ~ tc, data = lai)) #11.1% explained
confint(lm(lai ~ tc, data = lai))
# where cooccurrence occurrs, for a doubling in percent tree cover, LAI increases by 5.23% +/- 0.07%


xrange3 <- quantile(lai$tc, c(0.025, 0.975), na.rm = T) %>% as.numeric

lai <- lai %>% filter(tc < xrange3[2] & tc > xrange3[1])


figure(
  ggplot(lai) +
    aes(x = tc, y = lai) +
    geom_smooth(color = 'black', method = 'lm') +
    geom_smooth(color = 'red3', method = 'gam', formula = y ~ s(x, k = 6, bs = 'cr')) +
    # geom_hline(yintercept = 0) +
    # geom_vline(xintercept = 0) +
    theme_bw(base_size = 28) +
    # geom_rug(sides = c('bl'), alpha = 0.01) +
    # coord_cartesian(xlim = c(NA,420)) +
    # scale_x_continuous(breaks = c(0,5)) +
    labs(x = bquote('Tree Cover'~Delta~'(%)'), y = bquote('LAI'~Delta~'(%)')),
  path.name = 'c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Figures/LAI_TC_relationship.tif',
  height = 6, width = 6,
  save = F
)



# -------- GI 

veg.change <- fread('Grass_change_data.csv') %>%
  dplyr::select(ndvi_m, ndvi, lai, tc)

veg.change$ndvi_m[veg.change$ndvi_m<0] <- NA
veg.change$ndvi[veg.change$ndvi<0] <- NA
veg.change$lai[veg.change$lai<0] <- NA
veg.change$tc[veg.change$tc<0] <- NA

GI <- veg.change %>%
  dplyr::select(ndvi, ndvi_m, lai) %>%
  mutate(ndvi_m = range01(ndvi_m), ndvi = range01(ndvi), lai = range01(lai)) %>%
  mutate(GI = ndvi_m + ndvi + lai) %>%
  cbind(veg.change %>% dplyr::select(tc)) %>%
  dplyr::select(c(tc, GI))  %>%
  na.exclude





summary(lm(GI ~ tc, data = GI)) #17.01 % explained
confint(lm(GI ~ tc, data = GI))

xrange4 <- quantile(GI$tc, c(0.025, 0.975), na.rm = T) %>% as.numeric

GI <- GI %>% filter(tc < xrange4[2] & tc > xrange4[1])


gi.inc <- function(tc) {0.4731 + 0.0008339*tc}


(gi.inc(200)-gi.inc(100))/gi.inc(100)*100


figure(
  ggplot(GI) +
    aes(x = tc, y = GI) +
    geom_point(alpha = 0.01, color = '#6B4D7B') +
    geom_smooth(color = 'black', method = 'lm') +
    # geom_smooth(color = 'red3', method = 'gam', formula = y ~ s(x, k = 6, bs = 'cr')) +
    # geom_hline(yintercept = 0) +
    # geom_vline(xintercept = 0) +
    theme_bw(base_size = 28) +
    # geom_rug(sides = c('bl'), alpha = 0.01) +
    coord_cartesian(ylim = c(NA, 1.5)) +
    # scale_y_continuous(breaks = c(1.4,1.5,1.6,1.7)) +
    # scale_x_continuous(breaks = c(,1.5,1.6,1.7)) +
    labs(x = bquote('Tree Cover'~Delta~'(%)'), y = bquote('Greening Index')),
  path.name = 'c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Figures/GI_TC_relationship.tif',
  height = 6, width = 6,
  save = F
)


figure(
  ggplot(GI) +
    aes(x = GI) +
    geom_histogram(color = 'black', fill = '#55D515', binwidth = 0.05) +
    theme_bw(base_size = 28) +
    scale_x_continuous(limits = c(0,3)) +
    scale_y_continuous(limits = c(0,16000)) +
    labs(x = bquote('Greening Index'), y = 'Pixels'),
  path.name = 'c:/users/bryce/OneDrive/Documents/NGP/2019_Veg_change/Figures/GI_hist.tif',
  height = 6, width = 6,
  save = F
)



# Concurrent greening -----------------------------------------------------




source('~/R/startup.R') 
#c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/Data/

ngp <- readOGR(dsn = 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Shapefile/ngp_boundary_WGS.shp')
ext <- extent(ngp)
r.proj <- crs(ngp)

greening <- read_csv('Grass_change_data.csv') %>%
  dplyr::select(x,y, tc, ndvi_m, ndvi, lai, veg) %>%
  mutate(veg = factor(veg))


greening$ndvi_m[greening$ndvi_m<0] <- NA
greening$ndvi[greening$ndvi<0] <- NA
greening$lai[greening$lai<0] <- NA
greening$tc[greening$tc<0] <- NA


GI <- greening %>%
  mutate(ndvi_m = range01(ndvi_m), ndvi = range01(ndvi), lai = range01(lai)) %>%
  replace(., is.na(.), 0) %>%
  mutate(GI = ndvi_m + ndvi + lai) %>% 
  mutate(ndvi = as.numeric(ndvi>0)) %>%
  mutate(ndvi_m = as.numeric(ndvi_m>0)) %>%
  mutate(lai = as.numeric(lai>0))


# all greening
green <- GI %>%
  dplyr::select(x,y,GI) %>%
  rasterFromXYZ(crs = r.proj)
extent(green) <- ext
green <- mask(green, ngp)
plot(green)
writeRaster(green, 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Concurrent_grass_greening_index.tif', overwrite = T)


sum(GI$ndvi>0)/sum(GI$GI>0) # ndvi is increasing in 75.2% of greening
sum(GI$ndvi_m>0)/sum(GI$GI>0) # ndvi_m is increasing in 68.7% of greening pixels
sum(GI$lai>0)/sum(GI$GI>0) # lai is increasing in 91.2% of greening pixels



# Concurrent TC and greening ----------------------------------------------

concur <- greening %>%
  mutate(ndvi_m = range01(ndvi_m), ndvi = range01(ndvi), lai = range01(lai)) %>%
  mutate(GI = ndvi_m + ndvi + lai) %>% 
  mutate(ndvi = as.numeric(ndvi>0)+1) %>%
  mutate(ndvi_m = as.numeric(ndvi_m>0)+1) %>%
  mutate(lai = as.numeric(lai>0)+1) %>%
  mutate(tc = as.numeric(tc>0)) %>%
  mutate(GI2 = as.numeric(GI>0)+1) %>%
  replace(., is.na(.), 0) %>%
  mutate(concur1 = tc+ndvi) %>%
  mutate(concur2 = tc+ndvi_m) %>%
  mutate(concur3 = tc+lai) %>%
  mutate(concur4 = tc+GI2)


# TC and peak
peak <- concur %>%
  dplyr::select(x,y,concur1) %>%
  rasterFromXYZ(crs = r.proj)
extent(peak) <- ext
peak <- mask(peak, ngp)
plot(peak)
writeRaster(peak, 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Concurrent_grass_changes_peak.tif', overwrite = T)

sum(concur$concur1>0)/nrow(concur) # 75.4% of grasslands undergoing either tree cover or peak changes
sum(concur$concur1==1)/sum(concur$concur1>0) # 16.8% of those are tree cover only
sum(concur$concur1==2)/sum(concur$concur1>0) # 38.0% of those are ndvi only
sum(concur$concur1==3)/sum(concur$concur1>0) # 45.3% of those are concurrent




# TC and mean
mean <- concur %>%
  dplyr::select(x,y,concur2) %>%
  rasterFromXYZ(crs = r.proj)
extent(mean) <- ext
mean <- mask(mean, ngp)
plot(mean)
writeRaster(mean, 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Concurrent_grass_changes_mean.tif', overwrite = T)


sum(concur$concur2>0)/nrow(concur) # 69.8% of grasslands undergoing either tree cover or mean changes
sum(concur$concur2==1)/sum(concur$concur2>0) # 18.0% of those are tree cover only
sum(concur$concur2==2)/sum(concur$concur2>0) # 33.0% of those are ndvi only
sum(concur$concur2==3)/sum(concur$concur2>0) # 49.0% of those are concurrent




# TC and lai
lai <- concur %>%
  dplyr::select(x,y,concur3) %>%
  rasterFromXYZ(crs = r.proj)
extent(lai) <- ext
lai <- mask(lai, ngp)
plot(lai)
writeRaster(lai, 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Concurrent_grass_changes_lai.tif', overwrite = T)


sum(concur$concur3>0)/nrow(concur) # 82.3% of grasslands undergoing either tree cover or mean changes
sum(concur$concur3==1)/sum(concur$concur3>0) # 7.6% of those are tree cover only
sum(concur$concur3==2)/sum(concur$concur3>0) # 43.1% of those are lai only
sum(concur$concur3==3)/sum(concur$concur3>0) # 49.3% of those are concurrent



# TC and GI
GI <- concur %>%
  dplyr::select(x,y,concur4) %>%
  rasterFromXYZ(crs = r.proj)
extent(GI) <- ext
GI <- mask(GI, ngp)
plot(GI)
writeRaster(GI, 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/Concurrent_grass_changes_GI.tif', overwrite = T)

sum(concur$GI2>0)/nrow(concur) #44.1% of NGP undergoing all forms of greening
sum(concur$concur4==1)/sum(concur$concur4>0) # 30.5% of those are tree cover only
sum(concur$concur4==2)/sum(concur$concur4>0) # 26.4% of those are greening only
sum(concur$concur4==3)/sum(concur$concur4>0) # 43.1% of those are concurrent


sum(GI$GI>0)/nrow(GI) #44.1% of NGP undergoing all forms of greening
sum(concur$concur4==1)/sum(concur$concur4>0) # 30.5% of those are tree cover only
sum(concur$concur4==2)/sum(concur$concur4>0) # 26.4% of those are greening only
sum(concur$concur4==3)/sum(concur$concur4>0) # 43.1% of those are concurrent
