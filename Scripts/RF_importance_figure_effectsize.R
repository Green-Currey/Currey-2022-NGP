# Creates importance plots with z-scored effect sizes (Figures 3, 5 and S6)



source('~/R/startup.R') 
#c:/users/bryce/onedrive/documents/current projects/NGP/2019_Veg_change/Data
library(plyr)





var.names <- c('agdd' = 'AGDD',
               'agdd_dif' = 'AGDD \u0394',
               'cow' = 'Cattle',
               'cv' = 'Precip CV',
               'elv' = 'Elevation',
               'fire' = 'Fire',
               'map_dif' = 'MAP \u0394',
               'map_dm' = 'MAP',
               'mat_dm' = 'MAT',
               'mat_dif' = 'MAT \u0394',
               'ndep' = 'N Deposition',
               'pr_sp' = 'Spring Pr',
               'pr_sp_dif' = 'Spring Pr \u0394',
               'pr_su' = 'Summer Pr',
               'pr_su_dif' = 'Summer Pr \u0394',
               'pr_w' = 'Winter Pr',
               'pr_w_dif' = 'Winter Pr \u0394',
               'rough' = 'Roughness',
               'slp' = 'Slope',
               'soil' = 'Soil',
               'tmp_su' = 'Summer Tmp',
               'tmp_su_dif' = 'Summer Tmp \u0394',
               'tmp_w' = 'Winter Tmp',
               'tmp_w_dif' = 'Winter Tmp \u0394')


var.names2 <- c('agdd' = 'AGDD',
               'cow' = 'Cattle',
               'cv' = 'Precip CV',
               'elv' = 'Elevation',
               'map' = 'MAP',
               'mat' = 'MAT',
               'ndep' = 'N Deposition',
               'pr_sp' = 'Spring Pr',
               'pr_su' = 'Summer Pr',
               'pr_w' = 'Winter Pr',
               'rough' = 'Roughness',
               'slp' = 'Slope',
               'soil' = 'Soil',
               'tmp_su' = 'Summer Tmp',
               'tmp_w' = 'Winter Tmp')




# TC 2000 ---------------------------------------------------------------------

tc00.rf <- readRDS('rds/tc00_rf_grass_classification.rds'); tc00.rf
tc00 <- read_csv('Grass_change_data2.csv') %>%
  dplyr::select(-c(x, y, north, east, grass)) %>%
  mutate(fire = factor(fire, labels = c(1,0))) %>%
  mutate(fire = as.numeric(as.character(fire))) %>%
  na.exclude


act <- as.numeric(tc00$tc00>0)
pred <- as.numeric(predict(tc00.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.55



tc00.rf.imp <- varImp(tc00.rf, type = 1, scale = T)
varImpPlot(tc00.rf, type = 1, scale = T)
tc00.rf.imp <- tc00.rf.imp %>% mutate(var = row.names(tc00.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        filter(var != 'fire') %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names2)) %>%
        cbind(y = rev(seq(15))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('c','t','d','t','c','d','t','t','c','c','c','c','c','c','c')
tc00.rf.imp$var_type <- factor(var.type)




# CV 
mod <- lm(scale(tc00) ~ scale(cv), data = tc00)
tc00.rf.imp$Est[1] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[1] <- confint(mod)[2,1]
tc00.rf.imp$Upr[1] <- confint(mod)[2,2]


# Roughness 
mod <- lm(scale(tc00) ~ scale(rough), data = tc00)
tc00.rf.imp$Est[2] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[2] <- confint(mod)[2,1]
tc00.rf.imp$Upr[2] <- confint(mod)[2,2]


# Cow
mod <- lm(scale(tc00) ~ scale(cow), data = tc00)
tc00.rf.imp$Est[3] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[3] <- confint(mod)[2,1]
tc00.rf.imp$Upr[3] <- confint(mod)[2,2]


# Slope  
mod <- lm(scale(tc00) ~ scale(slp), data = tc00)
tc00.rf.imp$Est[4] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[4] <- confint(mod)[2,1]
tc00.rf.imp$Upr[4] <- confint(mod)[2,2]


# Winter Pr  
mod <- lm(scale(tc00) ~ scale(pr_w), data = tc00)
tc00.rf.imp$Est[5] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[5] <- confint(mod)[2,1]
tc00.rf.imp$Upr[5] <- confint(mod)[2,2]



# N dep 
mod <- lm(scale(tc00) ~ scale(ndep), data = tc00)
tc00.rf.imp$Est[6] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[6] <- confint(mod)[2,1]
tc00.rf.imp$Upr[6] <- confint(mod)[2,2]


# Elv 
mod <- lm(scale(tc00) ~ scale(elv), data = tc00)
tc00.rf.imp$Est[7] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[7] <- confint(mod)[2,1]
tc00.rf.imp$Upr[7] <- confint(mod)[2,2]


# Soil 


# Summer Pr 
mod <- lm(scale(tc00) ~ scale(pr_su), data = tc00)
tc00.rf.imp$Est[9] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[9] <- confint(mod)[2,1]
tc00.rf.imp$Upr[9] <- confint(mod)[2,2]


# AGDD 
mod <- lm(scale(tc00) ~ scale(agdd), data = tc00)
tc00.rf.imp$Est[10] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[10] <- confint(mod)[2,1]
tc00.rf.imp$Upr[10] <- confint(mod)[2,2]


# Winter Tmp 
mod <- lm(scale(tc00) ~ scale(tmp_w), data = tc00)
tc00.rf.imp$Est[11] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[11] <- confint(mod)[2,1]
tc00.rf.imp$Upr[11] <- confint(mod)[2,2]


# MAT 
mod <- lm(scale(tc00) ~ scale(mat), data = tc00)
tc00.rf.imp$Est[12] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[12] <- confint(mod)[2,1]
tc00.rf.imp$Upr[12] <- confint(mod)[2,2]


# MAP 
mod <- lm(scale(tc00) ~ scale(map), data = tc00)
tc00.rf.imp$Est[13] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[13] <- confint(mod)[2,1]
tc00.rf.imp$Upr[13] <- confint(mod)[2,2]


# Summer Temp 
mod <- lm(scale(tc00) ~ scale(tmp_su), data = tc00)
tc00.rf.imp$Est[14] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[14] <- confint(mod)[2,1]
tc00.rf.imp$Upr[14] <- confint(mod)[2,2]


# Spring pr  
mod <- lm(scale(tc00) ~ scale(pr_sp), data = tc00)
tc00.rf.imp$Est[15] <- coefficients(mod)[2]
tc00.rf.imp$Lwr[15] <- confint(mod)[2,1]
tc00.rf.imp$Upr[15] <- confint(mod)[2,2]



# TC 2019 ---------------------------------------------------------------------

tc19.rf <- readRDS('rds/tc19_rf_grass_classification.rds'); tc19.rf

tc19 <- read_csv('Grass_change_data.csv') %>%
  dplyr::select(-c(x, y, lai, ndvi_m, ndvi, tc00, tc, north, east, grass)) %>%
  mutate(fire = factor(fire, labels = c(1,0))) %>%
  mutate(fire = as.numeric(as.character(fire))) %>%
  na.exclude


act <- as.numeric(tc19$tc19>0)
pred <- as.numeric(predict(tc19.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.39


tc19.rf.imp <- varImp(tc19.rf, type = 1, scale = T)
varImpPlot(tc19.rf, type = 1, scale = T)
tc19.rf.imp <- tc19.rf.imp %>% mutate(var = row.names(tc19.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('t','c','t','c','d','c','c','c','c','d','t','c','c','c','c','t','c','c','c','c','c','c','c','d')
tc19.rf.imp$var_type <- factor(var.type)




# Roughness 
mod <- lm(scale(tc19) ~ scale(rough), data = tc19)
tc19.rf.imp$Est[1] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[1] <- confint(mod)[2,1]
tc19.rf.imp$Upr[1] <- confint(mod)[2,2]


# CV 
mod <- lm(scale(tc19) ~ scale(cv), data = tc19)
tc19.rf.imp$Est[2] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[2] <- confint(mod)[2,1]
tc19.rf.imp$Upr[2] <- confint(mod)[2,2]


# Slope 
mod <- lm(scale(tc19) ~ scale(rough), data = tc19)
tc19.rf.imp$Est[3] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[3] <- confint(mod)[2,1]
tc19.rf.imp$Upr[3] <- confint(mod)[2,2]


# Summer temp dif 
mod <- lm(scale(tc19) ~ scale(tmp_su_dif), data = tc19)
tc19.rf.imp$Est[4] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[4] <- confint(mod)[2,1]
tc19.rf.imp$Upr[4] <- confint(mod)[2,2]


# Cow 
mod <- lm(scale(tc19) ~ scale(cow), data = tc19)
tc19.rf.imp$Est[5] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[5] <- confint(mod)[2,1]
tc19.rf.imp$Upr[5] <- confint(mod)[2,2]


# Summer pr dif 
mod <- lm(scale(tc19) ~ scale(pr_su_dif), data = tc19)
tc19.rf.imp$Est[6] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[6] <- confint(mod)[2,1]
tc19.rf.imp$Upr[6] <- confint(mod)[2,2]


# Winter Pr dif
mod <- lm(scale(tc19) ~ scale(pr_w_dif), data = tc19)
tc19.rf.imp$Est[7] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[7] <- confint(mod)[2,1]
tc19.rf.imp$Upr[7] <- confint(mod)[2,2]


# Spring Pr dif 
mod <- lm(scale(tc19) ~ scale(pr_sp_dif), data = tc19)
tc19.rf.imp$Est[8] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[8] <- confint(mod)[2,1]
tc19.rf.imp$Upr[8] <- confint(mod)[2,2]


# AGDD dif
mod <- lm(scale(tc19) ~ scale(agdd_dif), data = tc19)
tc19.rf.imp$Est[9] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[9] <- confint(mod)[2,1]
tc19.rf.imp$Upr[9] <- confint(mod)[2,2]


# N dep
mod <- lm(scale(tc19) ~ scale(ndep), data = tc19)
tc19.rf.imp$Est[10] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[10] <- confint(mod)[2,1]
tc19.rf.imp$Upr[10] <- confint(mod)[2,2]


# Elv
mod <- lm(scale(tc19) ~ scale(elv), data = tc19)
tc19.rf.imp$Est[11] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[11] <- confint(mod)[2,1]
tc19.rf.imp$Upr[11] <- confint(mod)[2,2]


# Winter Tmp
mod <- lm(scale(tc19) ~ scale(tmp_w), data = tc19)
tc19.rf.imp$Est[12] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[12] <- confint(mod)[2,1]
tc19.rf.imp$Upr[12] <- confint(mod)[2,2]


# MAP dif
mod <- lm(scale(tc19) ~ scale(map_dif), data = tc19)
tc19.rf.imp$Est[13] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[13] <- confint(mod)[2,1]
tc19.rf.imp$Upr[13] <- confint(mod)[2,2]


# Summer Temp
mod <- lm(scale(tc19) ~ scale(tmp_su), data = tc19)
tc19.rf.imp$Est[14] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[14] <- confint(mod)[2,1]
tc19.rf.imp$Upr[14] <- confint(mod)[2,2]


# Summer Pr
mod <- lm(scale(tc19) ~ scale(pr_su), data = tc19)
tc19.rf.imp$Est[15] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[15] <- confint(mod)[2,1]
tc19.rf.imp$Upr[15] <- confint(mod)[2,2]


# Soil


# Winter Pr
mod <- lm(scale(tc19) ~ scale(pr_w), data = tc19)
tc19.rf.imp$Est[17] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[17] <- confint(mod)[2,1]
tc19.rf.imp$Upr[17] <- confint(mod)[2,2]


# Winter Temp dif 
mod <- lm(scale(tc19) ~ scale(tmp_w_dif), data = tc19)
tc19.rf.imp$Est[18] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[18] <- confint(mod)[2,1]
tc19.rf.imp$Upr[18] <- confint(mod)[2,2]



# MAP 
mod <- lm(scale(tc19) ~ scale(map_dm), data = tc19)
tc19.rf.imp$Est[19] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[19] <- confint(mod)[2,1]
tc19.rf.imp$Upr[19] <- confint(mod)[2,2]


# MAT
mod <- lm(scale(tc19) ~ scale(mat_dm), data = tc19)
tc19.rf.imp$Est[20] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[20] <- confint(mod)[2,1]
tc19.rf.imp$Upr[20] <- confint(mod)[2,2]


# Spring pr
mod <- lm(scale(tc19) ~ scale(pr_sp), data = tc19)
tc19.rf.imp$Est[21] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[21] <- confint(mod)[2,1]
tc19.rf.imp$Upr[21] <- confint(mod)[2,2]


# MAT dif
mod <- lm(scale(tc19) ~ scale(mat_dif), data = tc19)
tc19.rf.imp$Est[22] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[22] <- confint(mod)[2,1]
tc19.rf.imp$Upr[22] <- confint(mod)[2,2]


# AGDD
mod <- lm(scale(tc19) ~ scale(agdd), data = tc19)
tc19.rf.imp$Est[23] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[23] <- confint(mod)[2,1]
tc19.rf.imp$Upr[23] <- confint(mod)[2,2]



# Fire
mod <- lm(scale(tc19) ~ scale(fire), data = tc19)
tc19.rf.imp$Est[24] <- coefficients(mod)[2]
tc19.rf.imp$Lwr[24] <- confint(mod)[2,1]
tc19.rf.imp$Upr[24] <- confint(mod)[2,2]





# TCC ---------------------------------------------------------------------


tcc.rf <- readRDS('rds/tc_rf_grass_classification.rds'); tcc.rf

tcc <- read_csv('Grass_change_data.csv') %>%
  dplyr::select(-c(x, y, lai, ndvi_m, ndvi, tc19, tc00, north, east, grass)) %>%
  mutate(fire = factor(fire, labels = c(1,0))) %>%
  mutate(fire = as.numeric(as.character(fire))) %>%
  na.exclude


act <- as.numeric(tcc$tc>0)
pred <- as.numeric(predict(tcc.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.33


tcc.rf.imp <- varImp(tcc.rf, type = 1, scale = T)
varImpPlot(tcc.rf, type = 1, scale = T)
tcc.rf.imp <- tcc.rf.imp %>% mutate(var = row.names(tcc.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('c','t','t','d','c','c','d','c','c','c','c','c','c','d','c','t','c','c','c','c','t','c','c','c')
tcc.rf.imp$var_type <- factor(var.type)





# CV  
mod <- lm(scale(tc) ~ scale(cv), data = tcc)
tcc.rf.imp$Est[1] <- coefficients(mod)[2]#-5.5
tcc.rf.imp$Lwr[1] <- confint(mod)[2,1]#-5.5
tcc.rf.imp$Upr[1] <- confint(mod)[2,2]#-5.5


# Roughness
mod <- lm(scale(tc) ~ scale(rough), data = tcc)
tcc.rf.imp$Est[2] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[2] <- confint(mod)[2,1]
tcc.rf.imp$Upr[2] <- confint(mod)[2,2]


# Slope
mod <- lm(scale(tc) ~ scale(slp), data = tcc)
tcc.rf.imp$Est[3] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[3] <- confint(mod)[2,1]
tcc.rf.imp$Upr[3] <- confint(mod)[2,2]

# Fire 
mod <- lm(scale(tc) ~ scale(fire), data = tcc)
tcc.rf.imp$Est[4] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[4] <- confint(mod)[2,1]
tcc.rf.imp$Upr[4] <- confint(mod)[2,2]

# Winter Pr dif 
mod <- lm(scale(tc) ~ scale(pr_w_dif), data = tcc)
tcc.rf.imp$Est[5] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[5] <- confint(mod)[2,1]
tcc.rf.imp$Upr[5] <- confint(mod)[2,2]


# Spring Pr dif
mod <- lm(scale(tc) ~ scale(pr_sp_dif), data = tcc)
tcc.rf.imp$Est[6] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[6] <- confint(mod)[2,1]
tcc.rf.imp$Upr[6] <- confint(mod)[2,2]

# N dep 
mod <- lm(scale(tc) ~ scale(ndep), data = tcc)
tcc.rf.imp$Est[7] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[7] <- confint(mod)[2,1]
tcc.rf.imp$Upr[7] <- confint(mod)[2,2]


# Summer temp dif 
mod <- lm(scale(tc) ~ scale(tmp_su_dif), data = tcc)
tcc.rf.imp$Est[8] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[8] <- confint(mod)[2,1]
tcc.rf.imp$Upr[8] <- confint(mod)[2,2]

# Summer pr dif 
mod <- lm(scale(tc) ~ scale(pr_su_dif), data = tcc)
tcc.rf.imp$Est[9] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[9] <- confint(mod)[2,1]
tcc.rf.imp$Upr[9] <- confint(mod)[2,2]


# Summer Temp 
mod <- lm(scale(tc) ~ scale(tmp_su), data = tcc)
tcc.rf.imp$Est[10] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[10] <- confint(mod)[2,1]
tcc.rf.imp$Upr[10] <- confint(mod)[2,2]


# MAP dif 
mod <- lm(scale(tc) ~ scale(map_diff), data = tcc)
tcc.rf.imp$Est[11] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[11] <- confint(mod)[2,1]
tcc.rf.imp$Upr[11] <- confint(mod)[2,2]


# Winter Tmp 
mod <- lm(scale(tc) ~ scale(tmp_w), data = tcc)
tcc.rf.imp$Est[12] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[12] <- confint(mod)[2,1]
tcc.rf.imp$Upr[12] <- confint(mod)[2,2]

# MAT
mod <- lm(scale(tc) ~ scale(mat_dm), data = tcc)
tcc.rf.imp$Est[13] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[13] <- confint(mod)[2,1]
tcc.rf.imp$Upr[13] <- confint(mod)[2,2]

# Cow 
mod <- lm(scale(tc) ~ scale(cow), data = tcc)
tcc.rf.imp$Est[14] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[14] <- confint(mod)[2,1]
tcc.rf.imp$Upr[14] <- confint(mod)[2,2]


# Winter Pr 
mod <- lm(scale(tc) ~ scale(pr_w), data = tcc)
tcc.rf.imp$Est[15] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[15] <- confint(mod)[2,1]
tcc.rf.imp$Upr[15] <- confint(mod)[2,2]


# Elv
mod <- lm(scale(tc) ~ scale(elv), data = tcc)
tcc.rf.imp$Est[16] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[16] <- confint(mod)[2,1]
tcc.rf.imp$Upr[16] <- confint(mod)[2,2]


# Summer Pr 
mod <- lm(scale(tc) ~ scale(pr_su), data = tcc)
tcc.rf.imp$Est[17] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[17] <- confint(mod)[2,1]
tcc.rf.imp$Upr[17] <- confint(mod)[2,2]


# MAT dif
mod <- lm(scale(tc) ~ scale(mat_dif), data = tcc)
tcc.rf.imp$Est[18] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[18] <- confint(mod)[2,1]
tcc.rf.imp$Upr[18] <- confint(mod)[2,2]


# AGDD dif
mod <- lm(scale(tc) ~ scale(agdd_dif), data = tcc)
tcc.rf.imp$Est[19] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[19] <- confint(mod)[2,1]
tcc.rf.imp$Upr[19] <- confint(mod)[2,2]

# Spring pr
mod <- lm(scale(tc) ~ scale(pr_sp), data = tcc)
tcc.rf.imp$Est[20] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[20] <- confint(mod)[2,1]
tcc.rf.imp$Upr[20] <- confint(mod)[2,2]

# Soil 


# AGDD 
mod <- lm(scale(tc) ~ scale(agdd), data = tcc)
tcc.rf.imp$Est[22] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[22] <- confint(mod)[2,1]
tcc.rf.imp$Upr[22] <- confint(mod)[2,2]


# MAP 
mod <- lm(scale(tc) ~ scale(map_dm), data = tcc)
tcc.rf.imp$Est[23] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[23] <- confint(mod)[2,1]
tcc.rf.imp$Upr[23] <- confint(mod)[2,2]


# Winter Temp dif
mod <- lm(scale(tc) ~ scale(tmp_w_dif), data = tcc)
tcc.rf.imp$Est[24] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[24] <- confint(mod)[2,1]
tcc.rf.imp$Upr[24] <- confint(mod)[2,2]









# Peak --------------------------------------------------------------------

ndvi.rf <- readRDS('rds/pndvi_rf_grass_classification.rds'); ndvi.rf

peak <- read_csv('Grass_change_data.csv') %>%
  dplyr::select(-c(x, y, lai, ndvi_m, tc, tc19, tc00, north, east, grass)) %>%
  mutate(fire = factor(fire, labels = c(1,0))) %>%
  mutate(fire = as.numeric(as.character(fire))) %>%
  na.exclude



act <- as.numeric(peak$ndvi>0)
pred <- as.numeric(predict(ndvi.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.24

ndvi.rf.imp <- varImp(ndvi.rf, type = 1, scale = T)
varImpPlot(ndvi.rf, type = 1, scale = T)
ndvi.rf.imp <- ndvi.rf.imp %>% mutate(var = row.names(ndvi.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))


var.type <- c('c','d','c','t','c','t','c','c','c','c','t','c','d','c','c','c','c','c','c','c','c','t','c','d')
ndvi.rf.imp$var_type <- factor(var.type)


# CV
mod <- lm(scale(ndvi) ~ scale(cv), data = peak)
ndvi.rf.imp$Est[1] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[1] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[1] <- confint(mod)[2,2]


# Cow
mod <- lm(scale(ndvi) ~ scale(cow), data = peak)
ndvi.rf.imp$Est[2] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[2] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[2] <- confint(mod)[2,2]


# Summer pr dif
mod <- lm(scale(ndvi) ~ scale(pr_su_dif), data = peak)
ndvi.rf.imp$Est[3] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[3] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[3] <- confint(mod)[2,2]


# Roughness
mod <- lm(scale(ndvi) ~ scale(rough), data = peak)
ndvi.rf.imp$Est[4] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[4] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[4] <- confint(mod)[2,2]


# MAP dif
mod <- lm(scale(ndvi) ~ scale(map_dif), data = peak)
ndvi.rf.imp$Est[5] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[5] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[5] <- confint(mod)[2,2]


# Slope 
mod <- lm(scale(ndvi) ~ scale(slp), data = peak)
ndvi.rf.imp$Est[6] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[6] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[6] <- confint(mod)[2,2]


# Winter Pr dif
mod <- lm(scale(ndvi) ~ scale(pr_w_dif), data = peak)
ndvi.rf.imp$Est[7] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[7] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[7] <- confint(mod)[2,2]


# Winter Temp dif 
mod <- lm(scale(ndvi) ~ scale(tmp_w_dif), data = peak)
ndvi.rf.imp$Est[8] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[8] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[8] <- confint(mod)[2,2]


# Spring Pr dif
mod <- lm(scale(ndvi) ~ scale(tmp_su_dif), data = peak)
ndvi.rf.imp$Est[9] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[9] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[9] <- confint(mod)[2,2]


# Summer temp dif
mod <- lm(scale(ndvi) ~ scale(tmp_su_dif), data = peak)
ndvi.rf.imp$Est[10] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[10] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[10] <- confint(mod)[2,2]


# Elv 
mod <- lm(scale(ndvi) ~ scale(elv), data = peak)
ndvi.rf.imp$Est[11] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[11] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[11] <- confint(mod)[2,2]


# Winter Pr
mod <- lm(scale(ndvi) ~ scale(pr_w), data = peak)
ndvi.rf.imp$Est[12] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[12] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[12] <- confint(mod)[2,2]


# N dep
mod <- lm(scale(ndvi) ~ scale(ndep), data = peak)
ndvi.rf.imp$Est[13] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[13] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[13] <- confint(mod)[2,2]


# MAT dif
mod <- lm(scale(ndvi) ~ scale(mat_dif), data = peak)
ndvi.rf.imp$Est[14] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[14] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[14] <- confint(mod)[2,2]


# Summer Temp
mod <- lm(scale(ndvi) ~ scale(tmp_su), data = peak)
ndvi.rf.imp$Est[15] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[15] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[15] <- confint(mod)[2,2]


# MAT
mod <- lm(scale(ndvi) ~ scale(mat_dm), data = peak)
ndvi.rf.imp$Est[16] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[16] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[16] <- confint(mod)[2,2]


# Summer Pr
mod <- lm(scale(ndvi) ~ scale(pr_su), data = peak)
ndvi.rf.imp$Est[17] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[17] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[17] <- confint(mod)[2,2]



# Winter Tmp
mod <- lm(scale(ndvi) ~ scale(tmp_w), data = peak)
ndvi.rf.imp$Est[18] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[18] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[18] <- confint(mod)[2,2]



# AGDD dif 
mod <- lm(scale(ndvi) ~ scale(agdd_dif), data = peak)
ndvi.rf.imp$Est[19] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[19] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[19] <- confint(mod)[2,2]

# MAP
mod <- lm(scale(ndvi) ~ scale(map_dm), data = peak)
ndvi.rf.imp$Est[20] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[20] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[20] <- confint(mod)[2,2]


# Spring pr 
mod <- lm(scale(ndvi) ~ scale(pr_sp), data = peak)
ndvi.rf.imp$Est[21] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[21] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[21] <- confint(mod)[2,2]


# Soil 


# AGDD 
mod <- lm(scale(ndvi) ~ scale(agdd), data = peak)
ndvi.rf.imp$Est[23] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[23] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[23] <- confint(mod)[2,2]


# Fire
mod <- lm(scale(ndvi) ~ scale(fire), data = peak)
ndvi.rf.imp$Est[24] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[24] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[24] <- confint(mod)[2,2]



# Mean --------------------------------------------------------------------

mndvi.rf <- readRDS('rds/mndvi_rf_grass_classification.rds'); mndvi.rf

mean <- read_csv('Grass_change_data.csv') %>%
  dplyr::select(-c(x, y, lai, ndvi, tc, tc19, tc00, north, east, grass)) %>%
  mutate(fire = factor(fire, labels = c(1,0))) %>%
  mutate(fire = as.numeric(as.character(fire))) %>%
  na.exclude


act <- as.numeric(mean$ndvi_m>0)
pred <- as.numeric(predict(mndvi.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.54


ndvi2.rf.imp <- varImp(mndvi.rf, type = 1, scale = T)
varImpPlot(mndvi.rf, type = 1, scale = T)

ndvi2.rf.imp <- ndvi2.rf.imp %>% mutate(var = row.names(ndvi2.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('d','c','t','c','c','t','t','c','c','c','c','d','d','c','c','c','c','c','c','c','c','c','c','t')
ndvi2.rf.imp$var_type <- factor(var.type)



# Fire
mod <- lm(scale(ndvi_m) ~ scale(fire), data = mean)
ndvi2.rf.imp$Est[1] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[1] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[1] <- confint(mod)[2,2]


# CV
mod <- lm(scale(ndvi_m) ~ scale(cv), data = mean)
ndvi2.rf.imp$Est[2] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[2] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[2] <- confint(mod)[2,2]


# Slope
mod <- lm(scale(ndvi_m) ~ scale(slp), data = mean)
ndvi2.rf.imp$Est[3] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[3] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[3] <- confint(mod)[2,2]


# Winter Pr dif
mod <- lm(scale(ndvi_m) ~ scale(pr_w_dif), data = mean)
ndvi2.rf.imp$Est[4] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[4] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[4] <- confint(mod)[2,2]


# Summer temp dif
mod <- lm(scale(ndvi_m) ~ scale(tmp_su_dif), data = mean)
ndvi2.rf.imp$Est[5] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[5] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[5] <- confint(mod)[2,2]


# Roughness
mod <- lm(scale(ndvi_m) ~ scale(rough), data = mean)
ndvi2.rf.imp$Est[6] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[6] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[6] <- confint(mod)[2,2]


# Elv
mod <- lm(scale(ndvi_m) ~ scale(elv), data = mean)
ndvi2.rf.imp$Est[7] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[7] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[7] <- confint(mod)[2,2]


# Spring Pr dif
mod <- lm(scale(ndvi_m) ~ scale(pr_sp_dif), data = mean)
ndvi2.rf.imp$Est[8] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[8] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[8] <- confint(mod)[2,2]


# Winter Tmp 
mod <- lm(scale(ndvi_m) ~ scale(tmp_w), data = mean)
ndvi2.rf.imp$Est[9] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[9] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[9] <- confint(mod)[2,2]



# Winter Pr 
mod <- lm(scale(ndvi_m) ~ scale(pr_w), data = mean)
ndvi2.rf.imp$Est[10] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[10] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[10] <- confint(mod)[2,2]


# Summer pr dif
mod <- lm(scale(ndvi_m) ~ scale(pr_su_dif), data = mean)
ndvi2.rf.imp$Est[11] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[11] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[11] <- confint(mod)[2,2]


# Cow 
mod <- lm(scale(ndvi_m) ~ scale(cow), data = mean)
ndvi2.rf.imp$Est[12] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[12] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[12] <- confint(mod)[2,2]


# N dep 
mod <- lm(scale(ndvi_m) ~ scale(ndep), data = mean)
ndvi2.rf.imp$Est[13] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[13] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[13] <- confint(mod)[2,2]


# MAP dif
mod <- lm(scale(ndvi_m) ~ scale(map_dif), data = mean)
ndvi2.rf.imp$Est[14] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[14] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[14] <- confint(mod)[2,2]


# MAT 
mod <- lm(scale(ndvi_m) ~ scale(mat_dm), data = mean)
ndvi2.rf.imp$Est[15] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[15] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[15] <- confint(mod)[2,2]


# Summer Pr
mod <- lm(scale(ndvi_m) ~ scale(pr_su), data = mean)
ndvi2.rf.imp$Est[16] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[16] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[16] <- confint(mod)[2,2]


# MAT dif
mod <- lm(scale(ndvi_m) ~ scale(mat_dif), data = mean)
ndvi2.rf.imp$Est[17] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[17] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[17] <- confint(mod)[2,2]


# AGDD dif 
mod <- lm(scale(ndvi_m) ~ scale(agdd_dif), data = mean)
ndvi2.rf.imp$Est[18] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[18] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[18] <- confint(mod)[2,2]


# Summer Temp
mod <- lm(scale(ndvi_m) ~ scale(tmp_su), data = mean)
ndvi2.rf.imp$Est[19] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[19] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[19] <- confint(mod)[2,2]


# Spring pr
mod <- lm(scale(ndvi_m) ~ scale(pr_sp), data = mean)
ndvi2.rf.imp$Est[20] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[20] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[20] <- confint(mod)[2,2]


# Winter Temp dif 
mod <- lm(scale(ndvi_m) ~ scale(tmp_w_dif), data = mean)
ndvi2.rf.imp$Est[21] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[21] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[21] <- confint(mod)[2,2]


# MAP
mod <- lm(scale(ndvi_m) ~ scale(map_dm), data = mean)
ndvi2.rf.imp$Est[22] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[22] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[22] <- confint(mod)[2,2]


# AGDD 
mod <- lm(scale(ndvi_m) ~ scale(agdd), data = mean)
ndvi2.rf.imp$Est[23] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[23] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[23] <- confint(mod)[2,2]


# Soil 





# LAI ---------------------------------------------------------------------

lai.rf <- readRDS('rds/lai_rf_grass_classification.rds'); lai.rf

lai <- read_csv('Grass_change_data.csv') %>%
  dplyr::select(-c(x, y, ndvi_m, ndvi, tc, tc19, tc00, north, east, grass)) %>%
  mutate(fire = factor(fire, labels = c(1,0))) %>%
  mutate(fire = as.numeric(as.character(fire))) %>%
  na.exclude


act <- as.numeric(lai$lai>0)
pred <- as.numeric(predict(lai.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.33


lai.rf.imp <- varImp(lai.rf, type = 1, scale = T)
varImpPlot(lai.rf, type = 1, scale = T)
lai.rf.imp <- lai.rf.imp %>% mutate(var = row.names(lai.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('t','c','t','c','t','d','c','c','c','d','c','c','c','c','c','d','c','c','c','c','c','c','c','t')
lai.rf.imp$var_type <- factor(var.type)


# Roughness
mod <- lm(scale(lai) ~ scale(rough), data = lai)
lai.rf.imp$Est[1] <- coefficients(mod)[2]
lai.rf.imp$Lwr[1] <- confint(mod)[2,1]
lai.rf.imp$Upr[1] <- confint(mod)[2,2]


# CV
mod <- lm(scale(lai) ~ scale(cv), data = lai)
lai.rf.imp$Est[2] <- coefficients(mod)[2]
lai.rf.imp$Lwr[2] <- confint(mod)[2,1]
lai.rf.imp$Upr[2] <- confint(mod)[2,2]


# Slope
mod <- lm(scale(lai) ~ scale(slp), data = lai)
lai.rf.imp$Est[3] <- coefficients(mod)[2]
lai.rf.imp$Lwr[3] <- confint(mod)[2,1]
lai.rf.imp$Upr[3] <- confint(mod)[2,2]


# Spring Pr dif
mod <- lm(scale(lai) ~ scale(pr_sp_dif), data = lai)
lai.rf.imp$Est[4] <- coefficients(mod)[2]
lai.rf.imp$Lwr[4] <- confint(mod)[2,1]
lai.rf.imp$Upr[4] <- confint(mod)[2,2]


# Elv
mod <- lm(scale(lai) ~ scale(elv), data = lai)
lai.rf.imp$Est[5] <- coefficients(mod)[2]
lai.rf.imp$Lwr[5] <- confint(mod)[2,1]
lai.rf.imp$Upr[5] <- confint(mod)[2,2]


# Fire
mod <- lm(scale(lai) ~ scale(fire), data = lai)
lai.rf.imp$Est[6] <- coefficients(mod)[2]
lai.rf.imp$Lwr[6] <- confint(mod)[2,1]
lai.rf.imp$Upr[6] <- confint(mod)[2,2]


# Summer temp dif
mod <- lm(scale(lai) ~ scale(tmp_su_dif), data = lai)
lai.rf.imp$Est[7] <- coefficients(mod)[2]
lai.rf.imp$Lwr[7] <- confint(mod)[2,1]
lai.rf.imp$Upr[7] <- confint(mod)[2,2]


# MAP dif
mod <- lm(scale(lai) ~ scale(map_dif), data = lai)
lai.rf.imp$Est[8] <- coefficients(mod)[2]
lai.rf.imp$Lwr[8] <- confint(mod)[2,1]
lai.rf.imp$Upr[8] <- confint(mod)[2,2]


# Winter Pr dif
mod <- lm(scale(lai) ~ scale(pr_w_dif), data = lai)
lai.rf.imp$Est[9] <- coefficients(mod)[2]
lai.rf.imp$Lwr[9] <- confint(mod)[2,1]
lai.rf.imp$Upr[9] <- confint(mod)[2,2]


# Cow
mod <- lm(scale(lai) ~ scale(cow), data = lai)
lai.rf.imp$Est[10] <- coefficients(mod)[2]
lai.rf.imp$Lwr[10] <- confint(mod)[2,1]
lai.rf.imp$Upr[10] <- confint(mod)[2,2]


# Summer pr dif
mod <- lm(scale(lai) ~ scale(pr_su_dif), data = lai)
lai.rf.imp$Est[11] <- coefficients(mod)[2]
lai.rf.imp$Lwr[11] <- confint(mod)[2,1]
lai.rf.imp$Upr[11] <- confint(mod)[2,2]


# Winter Temp dif
mod <- lm(scale(lai) ~ scale(tmp_w_dif), data = lai)
lai.rf.imp$Est[12] <- coefficients(mod)[2]
lai.rf.imp$Lwr[12] <- confint(mod)[2,1]
lai.rf.imp$Upr[12] <- confint(mod)[2,2]


# Winter Pr
mod <- lm(scale(lai) ~ scale(pr_w), data = lai)
lai.rf.imp$Est[13] <- coefficients(mod)[2]
lai.rf.imp$Lwr[13] <- confint(mod)[2,1]
lai.rf.imp$Upr[13] <- confint(mod)[2,2]


# AGDD dif
mod <- lm(scale(lai) ~ scale(agdd_dif), data = lai)
lai.rf.imp$Est[14] <- coefficients(mod)[2]
lai.rf.imp$Lwr[14] <- confint(mod)[2,1]
lai.rf.imp$Upr[14] <- confint(mod)[2,2]


# Winter Tmp 
mod <- lm(scale(lai) ~ scale(tmp_w), data = lai)
lai.rf.imp$Est[15] <- coefficients(mod)[2]
lai.rf.imp$Lwr[15] <- confint(mod)[2,1]
lai.rf.imp$Upr[15] <- confint(mod)[2,2]


# N dep 
mod <- lm(scale(lai) ~ scale(ndep), data = lai)
lai.rf.imp$Est[16] <- coefficients(mod)[2]
lai.rf.imp$Lwr[16] <- confint(mod)[2,1]
lai.rf.imp$Upr[16] <- confint(mod)[2,2]


# MAT dif
mod <- lm(scale(lai) ~ scale(mat_dif), data = lai)
lai.rf.imp$Est[17] <- coefficients(mod)[2]
lai.rf.imp$Lwr[17] <- confint(mod)[2,1]
lai.rf.imp$Upr[17] <- confint(mod)[2,2]


# Summer Temp 
mod <- lm(scale(lai) ~ scale(tmp_su), data = lai)
lai.rf.imp$Est[18] <- coefficients(mod)[2]
lai.rf.imp$Lwr[18] <- confint(mod)[2,1]
lai.rf.imp$Upr[18] <- confint(mod)[2,2]


# Spring pr
mod <- lm(scale(lai) ~ scale(pr_sp), data = lai)
lai.rf.imp$Est[19] <- coefficients(mod)[2]
lai.rf.imp$Lwr[19] <- confint(mod)[2,1]
lai.rf.imp$Upr[19] <- confint(mod)[2,2]


# MAT
mod <- lm(scale(lai) ~ scale(mat_dm), data = lai)
lai.rf.imp$Est[20] <- coefficients(mod)[2]
lai.rf.imp$Lwr[20] <- confint(mod)[2,1]
lai.rf.imp$Upr[20] <- confint(mod)[2,2]


# Summer Pr 
mod <- lm(scale(lai) ~ scale(pr_su), data = lai)
lai.rf.imp$Est[21] <- coefficients(mod)[2]
lai.rf.imp$Lwr[21] <- confint(mod)[2,1]
lai.rf.imp$Upr[21] <- confint(mod)[2,2]


# MAP
mod <- lm(scale(lai) ~ scale(map_dm), data = lai)
lai.rf.imp$Est[22] <- coefficients(mod)[2]
lai.rf.imp$Lwr[22] <- confint(mod)[2,1]
lai.rf.imp$Upr[22] <- confint(mod)[2,2]


# AGDD
mod <- lm(scale(lai) ~ scale(agdd), data = lai)
lai.rf.imp$Est[23] <- coefficients(mod)[2]
lai.rf.imp$Lwr[23] <- confint(mod)[2,1]
lai.rf.imp$Upr[23] <- confint(mod)[2,2]


# Soil


# Plots --------------------------------------------------------------------

save = F

figure(
        ggplot(tc00.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(tc00.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = tc00.rf.imp$y, labels = tc00.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.4,0.4)) +
                theme_bw() +
                theme(panel.background = element_blank(),
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 18),
                      axis.title.y = element_text(size = 18),
                      axis.title.x = element_blank(),
                      plot.title = element_text(size = 18),
                      axis.ticks.x = element_blank(),
                      legend.position = 'none') +
                labs(y = '', x = ''),
        path.name = 'c:/users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Figures/RFE/TC_imp_grass.tif', 
        height = 6, width = 6, 
        save = save
)



figure(
        ggplot(tc19.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(tc19.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = tc19.rf.imp$y, labels = tc19.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.4,0.4)) +
                theme_bw() +
                theme(panel.background = element_blank(),
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 18),
                      axis.title.y = element_text(size = 18),
                      axis.title.x = element_blank(),
                      plot.title = element_text(size = 18),
                      axis.ticks.x = element_blank(),
                      legend.position = 'none') +
                labs(y = '', x = ''),
        path.name = 'c:/users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Figures/RFE/TC19_imp_grass.tif', 
        height = 6, width = 6, 
        save = save
)



figure(
        ggplot(tcc.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(tcc.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = tcc.rf.imp$y, labels = tcc.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.4,0.4)) +
                theme_bw() +
                theme(panel.background = element_blank(),
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 18),
                      axis.title.y = element_text(size = 18),
                      axis.title.x = element_blank(),
                      plot.title = element_text(size = 18),
                      axis.ticks.x = element_blank(),
                      legend.position = 'none') +
                labs(y = '', x = ''),
        path.name = 'c:/users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Figures/RFE/TCC_imp_grass.tif', 
        height = 6, width = 6, 
        save = save
)


figure(
        ggplot(ndvi.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(ndvi.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(x = Est, y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = ndvi.rf.imp$y, labels = ndvi.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.4,0.4)) +
                theme_bw() +
                theme(panel.background = element_blank(),
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 18),
                      axis.title.y = element_text(size = 18),
                      axis.title.x = element_blank(),
                      plot.title = element_text(size = 18),
                      axis.ticks.x = element_blank(),
                      legend.position = 'none') +
                labs(y = '', x = ''),
        path.name = 'c:/users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Figures/RFE/max_NDVI_imp_grass.tif', 
        height = 6, width = 6, 
        save = save
)

figure(
        ggplot(ndvi2.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(ndvi2.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(x = Est, y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = ndvi2.rf.imp$y, labels = ndvi2.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.4,0.4)) +
                theme_bw() +
                theme(panel.background = element_blank(),
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 18),
                      axis.title.y = element_text(size = 18),
                      axis.title.x = element_blank(),
                      plot.title = element_text(size = 18),
                      axis.ticks.x = element_blank(),
                      legend.position = 'none') +
                labs(y = '', x = ''),
        path.name = 'c:/users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Figures/RFE/mean_NDVI_imp_grass.tif', 
        height = 6, width = 6, 
        save = save
)

figure(
        ggplot(lai.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(lai.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(x = Est, y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = lai.rf.imp$y, labels = lai.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.4,0.4)) +
                theme_bw() +
                theme(panel.background = element_blank(),
                      panel.grid = element_blank(),
                      axis.text = element_text(size = 18),
                      axis.title.y = element_text(size = 18),
                      axis.title.x = element_blank(),
                      plot.title = element_text(size = 18),
                      axis.ticks.x = element_blank(),
                      legend.position = 'none') +
                labs(y = '', x = ''),
        path.name = 'c:/users/bryce/OneDrive/Documents/Current Projects/NGP/2019_Veg_change/Figures/RFE/LAI_imp_grass.tif', 
        height = 6, width = 6, 
        save = save
)



# Correlation -------------------------------------------------------------

library(labdsv)

importance <- rbind(
        cbind.data.frame(Group = rep('TreeCover00'), Variable = tc00.rf.imp$var, Overall = tc00.rf.imp$Est),
        cbind.data.frame(Group = rep('TreeCover19'), Variable = tc19.rf.imp$var, Overall = tc19.rf.imp$Est),
        cbind.data.frame(Group = rep('TreeCoverChange'), Variable = tcc.rf.imp$var, Overall = tcc.rf.imp$Est),
        cbind.data.frame(Group = rep('PeakNDVI'), Variable = ndvi.rf.imp$var, Overall = ndvi.rf.imp$Est),
        cbind.data.frame(Group = rep('MeanNDVI'), Variable = ndvi2.rf.imp$var, Overall = ndvi2.rf.imp$Est),
        cbind.data.frame(Group = rep('LAI'), Variable = lai.rf.imp$var, Overall = lai.rf.imp$Est)
)

import.mat <- matrify(importance) %>% t %>% as.data.frame()

importance2 <- rbind(
        cbind.data.frame(Group = rep('TreeCover00'), Variable = tc00.rf.imp$var, Overall = tc00.rf.imp$Est),
        cbind.data.frame(Group = rep('TreeCover19'), Variable = tc19.rf.imp$var, Overall = tc19.rf.imp$Est),
        cbind.data.frame(Group = rep('TreeCoverChange'), Variable = tcc.rf.imp$var, Overall = tcc.rf.imp$Est)
)

import.mat2 <- matrify(importance2) %>% t %>% as.data.frame() %>% filter(TreeCover00>0)
import.mat3 <- matrify(importance) %>% t %>% as.data.frame() %>% filter(TreeCover00>0)

cor(import.mat, method = c("spearman"))[,-4]
cor(import.mat2, method = c("spearman"))
cor(import.mat3, method = c('spearman'))[,4]
