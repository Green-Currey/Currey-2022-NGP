# Boruta analysis 


source('~/R/startup.R')
# c:/users/bryce/onedrive/documents/NGP/2019_Veg_change/

library(Boruta)


tc <- read_csv('Data/Grass_change_data.csv') %>%
     mutate_at(vars(soil, fire), as.factor) %>%
     select(-c(x, y, state, veg, ndvi, ndvi_m, tc19, tc00, lai)) %>%
     na.exclude()
tc.samp <- sample(tc, nrow(tc)*0.01, replace = F)

lai <- read_csv('Data/Grass_change_data.csv') %>%
     mutate_at(vars(soil, fire), as.factor) %>%
     select(-c(x,y,state, veg, ndvi, ndvi_m, tc, tc00, tc19)) %>%
     na.exclude()
lai.samp <- sample(lai, nrow(lai)*0.01, replace = F)

ndvi <- read_csv('Data/Grass_change_data.csv') %>%
     mutate_at(vars(soil, fire), as.factor) %>%
     select(-c(x,y,state, veg, tc, ndvi_m, tc19, tc00, lai)) %>%
     na.exclude()
ndvi.samp <- sample(ndvi, nrow(ndvi)*0.01, replace = F)

mndvi <- read_csv('Data/Grass_change_data.csv') %>%
     mutate_at(vars(soil, fire), as.factor) %>%
     select(-c(x,y,state, veg, ndvi, tc, tc19, tc00, lai)) %>%
     na.exclude()
mndvi.samp <- sample(mndvi, nrow(mndvi)*0.01, replace = F)




tc.mod <- Boruta(tc ~ ., data = tc.samp,
                 doTrace = 2,
                 pValue = 0.2,
                 maxRuns = 15)

lai.mod <- Boruta(lai ~ ., data = lai.samp,
                 doTrace = 2,
                 pValue = 0.2,
                 maxRuns = 15)

ndvi.mod <- Boruta(ndvi ~ ., data = ndvi.samp,
                 doTrace = 2,
                 pValue = 0.2,
                 maxRuns = 15)

mndvi.mod <- Boruta(ndvi_m ~ ., data = mndvi.samp,
                 doTrace = 2,
                 pValue = 0.2,
                 maxRuns = 15)



tc.mod$finalDecision
tc17.mod$finalDecision
ndvi.mod$finalDecision
mndvi.mod$finalDecision
