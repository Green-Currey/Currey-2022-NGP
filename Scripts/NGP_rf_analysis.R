# This script runs the random forests for tree cover variables. 
# Output is the RDS file(s) of the random forest function.


# Script is run on hyalite super computer at Montana State
# SLURM script that runs this script is named:


## packages required ##

library(randomForest)
library(dplyr)
library(doParallel)
library(caret)
library(data.table)

cores <- detectCores()
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
print(getDoParWorkers())


# Set run variable to different value to run different analyses
# --------------------------------------------
# Tree Cover Change 	->	 run = 1
# 2000 Tree Cover 	->	 run = 2
# 2019 Tree Cover 	->	 run = 3
# All Tree Cover runs 	->	 run = 'tc'
# LAI Change 		->	 run = 4
# Peak NDVI Change 	->	 run = 5
# Mean NDVI Change 	->	 run = 6
# All Greening runs 	->	 run = 'green'
# All analyses 		->	 run = 'all
# --------------------------------------------

run <- 'green'
st <- Sys.time() # obtain start time


# ------------------#
## Reading in data ##
# ----------------- #



## Tree Cover ##

# tc change
if (run == 1 | run == 'tc' | run == 'all') {
        tc <- fread('c:/users/bryce/onedrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Grass_change_data.csv') %>%
                select(-c(x, y, lai, ndvi, ndvi_m, tc00, tc19, north, east, grass)) %>%
                mutate(soil = factor(soil), fire = factor(fire)) %>%
                na.exclude
        
        print('tc change data read')
        print(Sys.time()-st)
        
} 

# tc 2000
if (run == 2 | run == 'tc' | run == 'all') {
        tc00 <- fread('c:/users/bryce/onedrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Grass_change_data2.csv') %>%
                select(-c(x, y, north, east, grass)) %>%
                mutate(soil = factor(soil), fire = factor(fire)) %>%
                na.exclude
        
        print('tc 2000 data read')
        print(Sys.time()-st)
        
}

# tc 2019
if (run == 3 | run == 'tc' | run == 'all') {
        tc19 <- fread('c:/users/bryce/onedrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Grass_change_data.csv') %>%
                select(-c(x, y, lai, ndvi, ndvi_m, tc, tc00, north, east, grass)) %>%
                mutate(soil = factor(soil), fire = factor(fire)) %>%
                na.exclude
        
        print('tc 2019 data read')
        print(Sys.time()-st)
        
}


## Greening ##

# lai
if (run == 4 | run == 'green' | run == 'all') {
        lai <- fread('c:/users/bryce/onedrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Grass_change_data.csv') %>%
                select(-c(x, y, ndvi, ndvi_m, tc, tc00, tc19, north, east, grass)) %>%
                mutate(soil = factor(soil), fire = factor(fire)) %>%
                na.exclude
        
        print('LAI data read')
        print(Sys.time()-st)
        
}

# peak NDVI
if (run == 5 | run == 'green' | run == 'all'){
        pndvi <- fread('c:/users/bryce/onedrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Grass_change_data.csv') %>%
                select(-c(x, y, lai, ndvi_m, tc, tc00, tc19, north, east, grass)) %>%
                mutate(soil = factor(soil), fire = factor(fire)) %>%
                na.exclude
        
        print('Peak NDVI data read')
        print(Sys.time()-st) 
        
}

# mean NDVI
if (run == 6 | run == 'green' | run == 'all'){
        mndvi <- fread('c:/users/bryce/onedrive/Documents/Current Projects/NGP/2019_Veg_change/Data/Grass_change_data.csv') %>%
                select(-c(x, y, ndvi, lai, tc, tc00, tc19, north, east, grass)) %>%
                mutate(soil = factor(soil), fire = factor(fire)) %>%
                na.exclude
        
        print('Mean NDVI data read')
        print(Sys.time()-st)
        
}





# ------------------------ #
## Random forest modeling ##
# ------------------------ #


new.st <- Sys.time()


## Tree Cover ##
# tc change
if (run == 1 | run == 'tc' | run == 'all') {
        tc.rf <- randomForest(factor(tc > 0) ~ .,
                              data = tc,
                              importance = T,
                              ntree = 500)
        
        saveRDS(tc.rf, '~/Current Projects/NGP/2019_Veg_change/Data/RDS/tc_rf_grass_classification.rds')
        print('Random forest created and rds writen for tree cover change')
        print(Sys.time()-new.st)
        
}
samp <- sample(seq(nrow(tc)), 1000)
tc.train <- tc[samp,]
tc.cv1 <- rfcv(tc.train[,-1], factor(tc.train$tc>0), cv.fold = 5)
itc.cv2 <- caret::train(y = factor(tc.train$tc > 0),
                       x = tc.train[,-1],
                       metric = 'Accuracy')

# tc 2000
if (run == 2 | run == 'tc' | run == 'all') {
        tc00.rf <- randomForest(factor(tc00 > 0) ~ .,
                              data = tc00,
                              importance = T,
                              ntree = 500)
        
        saveRDS(tc00.rf, '~/Current Projects/NGP/2019_Veg_change/Data/RDS/tc00_rf_grass_classification.rds')
        print('Random forest created and rds writen for 2000 tree cover')
        print(Sys.time()-new.st)
        
}

# tc 2019
if (run == 3 | run == 'tc' | run == 'all') {
        tc19.rf <- randomForest(factor(tc19 > 0) ~ .,
                              data = tc19,
                              importance = T,
                              ntree = 500)
        
        saveRDS(tc19.rf, '~/Current Projects/NGP/2019_Veg_change/Data/RDS/tc19_rf_grass_classification.rds')
        print('Random forest created and rds writen for 2019 tree cover')
        print(Sys.time()-new.st)
        
}


## Greening ##
# LAI
if (run == 4 | run == 'green' | run == 'all') {
        lai.rf <- randomForest(factor(lai > 0) ~ .,
                               data = lai,
                               importance = T,
                               ntree = 500)
        
        saveRDS(lai.rf, '~/Current Projects/NGP/2019_Veg_change/Data/RDS/lai_rf_grass_classification.rds')
        print('Random forest created and rds writen for lai change')
        print(Sys.time()-new.st)
        
}

# peak NDVI
if (run == 5 | run == 'green' | run == 'all'){
        pndvi.rf <- randomForest(factor(ndvi > 0) ~ .,
                                 data = pndvi,
                                 importance = T,
                                 ntree = 500)
        
        saveRDS(pndvi.rf, '~/Current Projects/NGP/2019_Veg_change/Data/RDS/pndvi_rf_grass_classification.rds')
        print('Random forest created and rds writen for peak ndvi change')
        print(Sys.time()-new.st)
        
}


# mean NDVI
if (run == 6 | run == 'green' | run == 'all'){
        mndvi.rf <- randomForest(factor(ndvi_m > 0) ~ .,
                                 data = mndvi,
                                 importance = T,
                                 ntree = 500)
        
        saveRDS(mndvi.rf, '~/Current Projects/NGP/2019_Veg_change/Data/RDS/mndvi_rf_grass_classification.rds')
        print('Random forest created and rds writen for mean ndvi change')
        print(Sys.time()-new.st)
        
}


print('Analysis complete.')
print('Total run time: ')
print(Sys.time()-st)
stopCluster(cl)



