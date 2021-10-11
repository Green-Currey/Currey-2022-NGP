
source('~/R/startup.R') 
#c:/users/bryce/onedrive/documents/Current Projects/NGP/2019_Veg_change/Data



# Concurrent Models -------------------------------------------------------
library(dplyr)
library(data.table)
library(SpatialPack)

# NDVI and TC00
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, ndvi, tc00)) 
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# NDVI and TC19
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, ndvi, tc19)) 
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# NDVI and TC
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, ndvi, tc))
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# MNDVI and TC00
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, ndvi_m, tc00)) 
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# MNDVI and TC19
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, ndvi_m, tc19)) 
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# MNDVI and TC
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, ndvi_m, tc)) 
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# LAI and TC00
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, lai, tc00)) 
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# LAI and TC19
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, lai, tc19)) 
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# LAI and TC
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, lai, tc))
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)



# Greening only -----------------------------------------------------------

# LAI and NDVI
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, lai, ndvi)) %>%
        na.exclude()
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# LAI and MNDVI
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, lai, ndvi_m)) %>%
        na.exclude()
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# NDVI and MNDVI
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, ndvi, ndvi_m)) %>%
        na.exclude()
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# TC only -----------------------------------------------------------------

# TC and TC00
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, tc, tc00)) 
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# TC and TC19
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, tc, tc19))
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)


# TC19 and TC00
df <- fread('Grass_change_data.csv') %>%
     dplyr::select(c(x, y, tc19, tc00))
coords <- matrix(c(df$x, df$y), ncol = 2)
x <- df[,3] %>% unlist %>% as.numeric
y <- df[,4] %>% unlist %>% as.numeric
cor.spatial(x, y, coords)
