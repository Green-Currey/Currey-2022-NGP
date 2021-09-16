# soils

library(raster)
library(dplyr)

soils <- raster('c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/NGP_Soils.tif')


plot(soils == 10)
# 1 is CH
# 2 is SZ
# 3 is RG
# 4 is CA Water
# 5 is GL
# 6 is VE
# 7 is LU
# 8 is nothing
# 9 is BR
# 10 is US Water


# 8 remove
# 7 lump with 5
# remove 4 and 10

soils.df <- as.data.frame(soils, xy = T) %>%
  na.exclude() %>%
  filter(NGP_Soils != 4 &
           NGP_Soils != 8 &
           NGP_Soils != 10)



table(soils.df$NGP_Soils)
# c('CH', 'SZ', 'RG', 'GL', 'VE', 'LU, 'BR')


soils.new <- rasterFromXYZ(soils.df, crs = crs(soils))
writeRaster(soils.new, 'c:/users/bryce/Dropbox/NGP_veg_change_2019/Raster/NGP_soils_reclass.tif')

