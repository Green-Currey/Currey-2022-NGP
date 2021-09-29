# Creates importance plots with z-scored effect sizes (Figures 3, 5 and S6)



source('~/R/startup.R') 
#c:/users/bryce/onedrive/documents/current projects/NGP/2019_Veg_change/Data

library(plyr)


tc.rf <- readRDS('rds/tc00_rf_grass_classification.rds'); tc.rf
tc2.rf <- readRDS('rds/tc19_rf_grass2_classification.rds'); tc2.rf
tcc.rf <- readRDS('rds/tc_rf_grass_classification.rds'); tcc.rf
ndvi.rf <- readRDS('rds/ndvi_rf_grass_classification.rds') 
mndvi.rf <- readRDS('rds/ndvi_m_rf_grass_classification.rds') 
lai.rf <- readRDS('rds/lai_rf_grass_classification.rds') 

peak <- read_csv('Grass_change_data.csv') %>%
        dplyr::select(-c(x, y, lai, ndvi_m, tc, tc19, tc00, north, east, veg)) %>%
        mutate(fire = factor(fire, labels = c(1,0))) %>%
        mutate(fire = as.numeric(as.character(fire))) %>%
        na.exclude

mean <- read_csv('Grass_change_data.csv') %>%
        dplyr::select(-c(x, y, lai, ndvi, tc, tc19, tc00, north, east, veg)) %>%
        mutate(fire = factor(fire, labels = c(1,0))) %>%
        mutate(fire = as.numeric(as.character(fire))) %>%
        na.exclude

lai <- read_csv('Grass_change_data.csv') %>%
        dplyr::select(-c(x, y, ndvi_m, ndvi, tc, tc19, tc00, north, east, veg)) %>%
        mutate(fire = factor(fire, labels = c(1,0))) %>%
        mutate(fire = as.numeric(as.character(fire))) %>%
        na.exclude

tcc <- read_csv('Grass_change_data.csv') %>%
        dplyr::select(-c(x, y, lai, ndvi_m, ndvi, tc19, tc00, north, east, veg)) %>%
        mutate(fire = factor(fire, labels = c(1,0))) %>%
        mutate(fire = as.numeric(as.character(fire))) %>%
        na.exclude

tc <- read_csv('Grass_change_data2.csv') %>%
        dplyr::select(-c(x, y, north, east, veg)) %>%
        mutate(fire = factor(fire, labels = c(1,0))) %>%
        mutate(fire = as.numeric(as.character(fire))) %>%
        na.exclude

tc2 <- read_csv('Grass_change_data.csv') %>%
        dplyr::select(-c(x, y, lai, ndvi_m, ndvi, tc00, tc, north, east, veg)) %>%
        mutate(fire = factor(fire, labels = c(1,0))) %>%
        mutate(fire = as.numeric(as.character(fire))) %>%
        na.exclude

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


act <- as.numeric(tc$tc00>0)
pred <- as.numeric(predict(tc.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.449



tc.rf.imp <- varImp(tc.rf, type = 1, scale = T)
tc.rf.imp <- tc.rf.imp %>% mutate(var = row.names(tc.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        filter(var != 'fire') %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names2)) %>%
        cbind(y = rev(seq(15))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('c','t','t','c','d','d','t','c','c','t','c','c','c','c', 'c')
tc.rf.imp$var_type <- factor(var.type)





# CV 1  
mod <- lm(scale(tc00) ~ scale(cv), data = tc)
tc.rf.imp$Est[1] <- coefficients(mod)[2]
tc.rf.imp$Lwr[1] <- confint(mod)[2,1]
tc.rf.imp$Upr[1] <- confint(mod)[2,2]


# Roughness 2 
mod <- lm(scale(tc00) ~ scale(rough), data = tc)
tc.rf.imp$Est[2] <- coefficients(mod)[2]
tc.rf.imp$Lwr[2] <- confint(mod)[2,1]
tc.rf.imp$Upr[2] <- confint(mod)[2,2]


# Slope 3 
mod <- lm(scale(tc00) ~ scale(slp), data = tc)
tc.rf.imp$Est[3] <- coefficients(mod)[2]
tc.rf.imp$Lwr[3] <- confint(mod)[2,1]
tc.rf.imp$Upr[3] <- confint(mod)[2,2]


# Winter Pr 4 
mod <- lm(scale(tc00) ~ scale(pr_w), data = tc)
tc.rf.imp$Est[4] <- coefficients(mod)[2]
tc.rf.imp$Lwr[4] <- confint(mod)[2,1]
tc.rf.imp$Upr[4] <- confint(mod)[2,2]


# Cow 5
mod <- lm(scale(tc00) ~ scale(cow), data = tc)
tc.rf.imp$Est[5] <- coefficients(mod)[2]
tc.rf.imp$Lwr[5] <- confint(mod)[2,1]
tc.rf.imp$Upr[5] <- confint(mod)[2,2]


# N dep 6
mod <- lm(scale(tc00) ~ scale(ndep), data = tc)
tc.rf.imp$Est[6] <- coefficients(mod)[2]
tc.rf.imp$Lwr[6] <- confint(mod)[2,1]
tc.rf.imp$Upr[6] <- confint(mod)[2,2]


# Elv 7
mod <- lm(scale(tc00) ~ scale(elv), data = tc)
tc.rf.imp$Est[7] <- coefficients(mod)[2]
tc.rf.imp$Lwr[7] <- confint(mod)[2,1]
tc.rf.imp$Upr[7] <- confint(mod)[2,2]


# Winter Tmp 8
mod <- lm(scale(tc00) ~ scale(tmp_w), data = tc)
tc.rf.imp$Est[8] <- coefficients(mod)[2]
tc.rf.imp$Lwr[8] <- confint(mod)[2,1]
tc.rf.imp$Upr[8] <- confint(mod)[2,2]


# AGDD 9
mod <- lm(scale(tc00) ~ scale(agdd), data = tc)
tc.rf.imp$Est[9] <- coefficients(mod)[2]
tc.rf.imp$Lwr[9] <- confint(mod)[2,1]
tc.rf.imp$Upr[9] <- confint(mod)[2,2]


# Soil 10 


# Summer Pr 11
mod <- lm(scale(tc00) ~ scale(pr_su), data = tc)
tc.rf.imp$Est[11] <- coefficients(mod)[2]
tc.rf.imp$Lwr[11] <- confint(mod)[2,1]
tc.rf.imp$Upr[11] <- confint(mod)[2,2]


# Summer Temp 12
mod <- lm(scale(tc00) ~ scale(tmp_su), data = tc)
tc.rf.imp$Est[12] <- coefficients(mod)[2]
tc.rf.imp$Lwr[12] <- confint(mod)[2,1]
tc.rf.imp$Upr[12] <- confint(mod)[2,2]


# MAP 13
mod <- lm(scale(tc00) ~ scale(map), data = tc)
tc.rf.imp$Est[13] <- coefficients(mod)[2]
tc.rf.imp$Lwr[13] <- confint(mod)[2,1]
tc.rf.imp$Upr[13] <- confint(mod)[2,2]


# Spring pr 14 
mod <- lm(scale(tc00) ~ scale(pr_sp), data = tc)
tc.rf.imp$Est[14] <- coefficients(mod)[2]
tc.rf.imp$Lwr[14] <- confint(mod)[2,1]
tc.rf.imp$Upr[14] <- confint(mod)[2,2]


# MAT 15
mod <- lm(scale(tc00) ~ scale(mat), data = tc)
tc.rf.imp$Est[15] <- coefficients(mod)[2]
tc.rf.imp$Lwr[15] <- confint(mod)[2,1]
tc.rf.imp$Upr[15] <- confint(mod)[2,2]





# TC 2019 ---------------------------------------------------------------------

act <- as.numeric(tc2$tc19>0)
pred <- as.numeric(predict(tc2.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.382


tc2.rf.imp <- varImp(tc2.rf, type = 1, scale = T)
tc2.rf.imp <- tc2.rf.imp %>% mutate(var = row.names(tc2.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('c','t','t','c','c','c','c','c','t','d','c','c','t','d','c','c','c','c','c','c','c','c','c','d')
tc2.rf.imp$var_type <- factor(var.type)





# CV 1 (2)
mod <- lm(scale(tc19) ~ scale(cv), data = tc2)
tc2.rf.imp$Est[1] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[1] <- confint(mod)[2,1]
tc2.rf.imp$Upr[1] <- confint(mod)[2,2]



# Slope 2 (5)
mod <- lm(scale(tc19) ~ scale(rough), data = tc2)
tc2.rf.imp$Est[2] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[2] <- confint(mod)[2,1]
tc2.rf.imp$Upr[2] <- confint(mod)[2,2]



# Roughness 3 (1)
mod <- lm(scale(tc19) ~ scale(rough), data = tc2)
tc2.rf.imp$Est[3] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[3] <- confint(mod)[2,1]
tc2.rf.imp$Upr[3] <- confint(mod)[2,2]



# Summer temp dif 4 (10)
mod <- lm(scale(tc19) ~ scale(tmp_su_dif), data = tc2)
tc2.rf.imp$Est[4] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[4] <- confint(mod)[2,1]
tc2.rf.imp$Upr[4] <- confint(mod)[2,2]



# Winter Pr dif 5 (3)
mod <- lm(scale(tc19) ~ scale(pr_w_dif), data = tc2)
tc2.rf.imp$Est[5] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[5] <- confint(mod)[2,1]
tc2.rf.imp$Upr[5] <- confint(mod)[2,2]



# Summer pr dif 6 (7)
mod <- lm(scale(tc19) ~ scale(pr_su_dif), data = tc2)
tc2.rf.imp$Est[6] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[6] <- confint(mod)[2,1]
tc2.rf.imp$Upr[6] <- confint(mod)[2,2]



# Winter Pr 7 (9)
mod <- lm(scale(tc19) ~ scale(pr_w), data = tc2)
tc2.rf.imp$Est[7] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[7] <- confint(mod)[2,1]
tc2.rf.imp$Upr[7] <- confint(mod)[2,2]



# MAP dif 8 (12)
mod <- lm(scale(tc19) ~ scale(map_dif), data = tc2)
tc2.rf.imp$Est[8] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[8] <- confint(mod)[2,1]
tc2.rf.imp$Upr[8] <- confint(mod)[2,2]



# Soil 10 (16) 



# Cow 9 (11)
mod <- lm(scale(tc19) ~ scale(cow), data = tc2)
tc2.rf.imp$Est[10] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[10] <- confint(mod)[2,1]
tc2.rf.imp$Upr[10] <- confint(mod)[2,2]





# Spring Pr dif 11 (8)
mod <- lm(scale(tc19) ~ scale(pr_sp_dif), data = tc2)
tc2.rf.imp$Est[11] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[11] <- confint(mod)[2,1]
tc2.rf.imp$Upr[11] <- confint(mod)[2,2]



# Winter Temp dif 12 (4)
mod <- lm(scale(tc19) ~ scale(tmp_w_dif), data = tc2)
tc2.rf.imp$Est[12] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[12] <- confint(mod)[2,1]
tc2.rf.imp$Upr[12] <- confint(mod)[2,2]



# Elv 13 (14)
mod <- lm(scale(tc19) ~ scale(elv), data = tc2)
tc2.rf.imp$Est[13] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[13] <- confint(mod)[2,1]
tc2.rf.imp$Upr[13] <- confint(mod)[2,2]



# N dep 14 (13) 
mod <- lm(scale(tc19) ~ scale(ndep), data = tc2)
tc2.rf.imp$Est[14] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[14] <- confint(mod)[2,1]
tc2.rf.imp$Upr[14] <- confint(mod)[2,2]



# AGDD dif 15 (20)
mod <- lm(scale(tc19) ~ scale(agdd_dif), data = tc2)
tc2.rf.imp$Est[15] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[15] <- confint(mod)[2,1]
tc2.rf.imp$Upr[15] <- confint(mod)[2,2]



# Summer Pr 16 (17)
mod <- lm(scale(tc19) ~ scale(pr_su), data = tc2)
tc2.rf.imp$Est[16] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[16] <- confint(mod)[2,1]
tc2.rf.imp$Upr[16] <- confint(mod)[2,2]



# MAT dif 17 (12)
mod <- lm(scale(tc19) ~ scale(mat_dif), data = tc2)
tc2.rf.imp$Est[17] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[17] <- confint(mod)[2,1]
tc2.rf.imp$Upr[17] <- confint(mod)[2,2]



# Winter Tmp 18 
mod <- lm(scale(tc19) ~ scale(tmp_w), data = tc2)
tc2.rf.imp$Est[18] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[18] <- confint(mod)[2,1]
tc2.rf.imp$Upr[18] <- confint(mod)[2,2]



# Spring pr 19 (23) 
mod <- lm(scale(tc19) ~ scale(pr_sp), data = tc2)
tc2.rf.imp$Est[19] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[19] <- confint(mod)[2,1]
tc2.rf.imp$Upr[19] <- confint(mod)[2,2]



# MAP 20 (19) 
mod <- lm(scale(tc19) ~ scale(map_dm), data = tc2)
tc2.rf.imp$Est[20] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[20] <- confint(mod)[2,1]
tc2.rf.imp$Upr[20] <- confint(mod)[2,2]



# MAT 21 (22)
mod <- lm(scale(tc19) ~ scale(mat_dm), data = tc2)
tc2.rf.imp$Est[21] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[21] <- confint(mod)[2,1]
tc2.rf.imp$Upr[21] <- confint(mod)[2,2]



# Summer Temp 22 (15)
mod <- lm(scale(tc19) ~ scale(tmp_su), data = tc2)
tc2.rf.imp$Est[22] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[22] <- confint(mod)[2,1]
tc2.rf.imp$Upr[22] <- confint(mod)[2,2]



# AGDD 23 (21)
mod <- lm(scale(tc19) ~ scale(agdd), data = tc2)
tc2.rf.imp$Est[23] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[23] <- confint(mod)[2,1]
tc2.rf.imp$Upr[23] <- confint(mod)[2,2]



# Fire 24 
mod <- lm(scale(tc19) ~ scale(fire), data = tc2)
tc2.rf.imp$Est[24] <- coefficients(mod)[2]
tc2.rf.imp$Lwr[24] <- confint(mod)[2,1]
tc2.rf.imp$Upr[24] <- confint(mod)[2,2]





# TCC ---------------------------------------------------------------------

act <- as.numeric(tcc$tc>0)
pred <- as.numeric(predict(tcc.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.309


tcc.rf.imp <- varImp(tcc.rf, type = 1, scale = T)
tcc.rf.imp <- tcc.rf.imp %>% mutate(var = row.names(tcc.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('d','c','t','c','t','c','c','c','c','d','c','c','c','c','d','c','t','c','c','c','c','c','c','t')
tcc.rf.imp$var_type <- factor(var.type)


# Fire 1 
mod <- lm(scale(tc) ~ scale(fire), data = tcc)
tcc.rf.imp$Est[1] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[1] <- confint(mod)[2,1]
tcc.rf.imp$Upr[1] <- confint(mod)[2,2]


# CV 2  
mod <- lm(scale(tc) ~ scale(cv), data = tcc)
tcc.rf.imp$Est[2] <- coefficients(mod)[2]#-5.5
tcc.rf.imp$Lwr[2] <- confint(mod)[2,1]#-5.5
tcc.rf.imp$Upr[2] <- confint(mod)[2,2]#-5.5



# Roughness 3 
mod <- lm(scale(tc) ~ scale(rough), data = tcc)
tcc.rf.imp$Est[3] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[3] <- confint(mod)[2,1]
tcc.rf.imp$Upr[3] <- confint(mod)[2,2]



# Winter Pr dif 4 
mod <- lm(scale(tc) ~ scale(pr_w_dif), data = tcc)
tcc.rf.imp$Est[4] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[4] <- confint(mod)[2,1]
tcc.rf.imp$Upr[4] <- confint(mod)[2,2]


# Slope 5 
mod <- lm(scale(tc) ~ scale(slp), data = tcc)
tcc.rf.imp$Est[5] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[5] <- confint(mod)[2,1]
tcc.rf.imp$Upr[5] <- confint(mod)[2,2]

# Summer pr dif 6 
mod <- lm(scale(tc) ~ scale(pr_su_dif), data = tcc)
tcc.rf.imp$Est[6] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[6] <- confint(mod)[2,1]
tcc.rf.imp$Upr[6] <- confint(mod)[2,2]


# Summer Temp 7 
mod <- lm(scale(tc) ~ scale(tmp_su), data = tcc)
tcc.rf.imp$Est[7] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[7] <- confint(mod)[2,1]
tcc.rf.imp$Upr[7] <- confint(mod)[2,2]

# MAP dif 8 
mod <- lm(scale(tc) ~ scale(map_diff), data = tcc)
tcc.rf.imp$Est[8] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[8] <- confint(mod)[2,1]
tcc.rf.imp$Upr[8] <- confint(mod)[2,2]


# MAT 9 
mod <- lm(scale(tc) ~ scale(mat_dm), data = tcc)
tcc.rf.imp$Est[9] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[9] <- confint(mod)[2,1]
tcc.rf.imp$Upr[9] <- confint(mod)[2,2]


# Cow 10 
mod <- lm(scale(tc) ~ scale(cow), data = tcc)
tcc.rf.imp$Est[10] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[10] <- confint(mod)[2,1]
tcc.rf.imp$Upr[10] <- confint(mod)[2,2]


# Spring Pr dif 11 
mod <- lm(scale(tc) ~ scale(pr_sp_dif), data = tcc)
tcc.rf.imp$Est[11] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[11] <- confint(mod)[2,1]
tcc.rf.imp$Upr[11] <- confint(mod)[2,2]

# Winter Pr 12 
mod <- lm(scale(tc) ~ scale(pr_w), data = tcc)
tcc.rf.imp$Est[12] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[12] <- confint(mod)[2,1]
tcc.rf.imp$Upr[12] <- confint(mod)[2,2]


# Summer temp dif 13 
mod <- lm(scale(tc) ~ scale(tmp_su_dif), data = tcc)
tcc.rf.imp$Est[13] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[13] <- confint(mod)[2,1]
tcc.rf.imp$Upr[13] <- confint(mod)[2,2]


# Winter Tmp 14 
mod <- lm(scale(tc) ~ scale(tmp_w), data = tcc)
tcc.rf.imp$Est[14] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[14] <- confint(mod)[2,1]
tcc.rf.imp$Upr[14] <- confint(mod)[2,2]


# N dep 15 
mod <- lm(scale(tc) ~ scale(ndep), data = tcc)
tcc.rf.imp$Est[15] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[15] <- confint(mod)[2,1]
tcc.rf.imp$Upr[15] <- confint(mod)[2,2]


# Spring pr 16 
mod <- lm(scale(tc) ~ scale(pr_sp), data = tcc)
tcc.rf.imp$Est[16] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[16] <- confint(mod)[2,1]
tcc.rf.imp$Upr[16] <- confint(mod)[2,2]


# Elv 17 
mod <- lm(scale(tc) ~ scale(elv), data = tcc)
tcc.rf.imp$Est[17] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[17] <- confint(mod)[2,1]
tcc.rf.imp$Upr[17] <- confint(mod)[2,2]


# Summer Pr 18 -
mod <- lm(scale(tc) ~ scale(pr_su), data = tcc)
tcc.rf.imp$Est[18] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[18] <- confint(mod)[2,1]
tcc.rf.imp$Upr[18] <- confint(mod)[2,2]



# MAT dif 19 
mod <- lm(scale(tc) ~ scale(mat_dif), data = tcc)
tcc.rf.imp$Est[19] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[19] <- confint(mod)[2,1]
tcc.rf.imp$Upr[19] <- confint(mod)[2,2]


# AGDD 20 
mod <- lm(scale(tc) ~ scale(agdd), data = tcc)
tcc.rf.imp$Est[20] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[20] <- confint(mod)[2,1]
tcc.rf.imp$Upr[20] <- confint(mod)[2,2]


# AGDD dif 21 
mod <- lm(scale(tc) ~ scale(agdd_dif), data = tcc)
tcc.rf.imp$Est[21] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[21] <- confint(mod)[2,1]
tcc.rf.imp$Upr[21] <- confint(mod)[2,2]


# Winter Temp dif 22 
mod <- lm(scale(tc) ~ scale(tmp_w_dif), data = tcc)
tcc.rf.imp$Est[22] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[22] <- confint(mod)[2,1]
tcc.rf.imp$Upr[22] <- confint(mod)[2,2]


# MAP 23 
mod <- lm(scale(tc) ~ scale(map_dm), data = tcc)
tcc.rf.imp$Est[23] <- coefficients(mod)[2]
tcc.rf.imp$Lwr[23] <- confint(mod)[2,1]
tcc.rf.imp$Upr[23] <- confint(mod)[2,2]


# Soil 24 




# Peak --------------------------------------------------------------------

act <- as.numeric(peak$ndvi>0)
pred <- as.numeric(predict(ndvi.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.174

ndvi.rf.imp <- varImp(ndvi.rf, type = 1, scale = T)
ndvi.rf.imp <- ndvi.rf.imp %>% mutate(var = row.names(ndvi.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))


var.type <- c('c','c','c','t','d','c','t','d','c','c','c','c','t','c','c','c','c','c','t','d','c','c','c','c')
ndvi.rf.imp$var_type <- factor(var.type)


# CV 1  
mod <- lm(scale(ndvi) ~ scale(cv), data = peak)
ndvi.rf.imp$Est[1] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[1] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[1] <- confint(mod)[2,2]



# Spring Pr dif 2 
mod <- lm(scale(ndvi) ~ scale(pr_sp_dif), data = peak)
ndvi.rf.imp$Est[2] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[2] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[2] <- confint(mod)[2,2]



# Summer pr dif 3 
mod <- lm(scale(ndvi) ~ scale(pr_su_dif), data = peak)
ndvi.rf.imp$Est[3] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[3] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[3] <- confint(mod)[2,2]



# Slope 4 
mod <- lm(scale(ndvi) ~ scale(slp), data = peak)
ndvi.rf.imp$Est[4] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[4] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[4] <- confint(mod)[2,2]



# Cow 5 
mod <- lm(scale(ndvi) ~ scale(cow), data = peak)
ndvi.rf.imp$Est[5] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[5] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[5] <- confint(mod)[2,2]



# Winter Temp dif 6 
mod <- lm(scale(ndvi) ~ scale(tmp_w_dif), data = peak)
ndvi.rf.imp$Est[6] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[6] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[6] <- confint(mod)[2,2]



# Roughness 7 
mod <- lm(scale(ndvi) ~ scale(rough), data = peak)
ndvi.rf.imp$Est[7] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[7] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[7] <- confint(mod)[2,2]



# Fire 8 
mod <- lm(scale(ndvi) ~ scale(fire), data = peak)
ndvi.rf.imp$Est[8] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[8] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[8] <- confint(mod)[2,2]



# Summer temp dif 9 
mod <- lm(scale(ndvi) ~ scale(tmp_su_dif), data = peak)
ndvi.rf.imp$Est[9] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[9] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[9] <- confint(mod)[2,2]



# Winter Pr dif 10 
mod <- lm(scale(ndvi) ~ scale(pr_w_dif), data = peak)
ndvi.rf.imp$Est[10] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[10] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[10] <- confint(mod)[2,2]



# MAP dif 11 
mod <- lm(scale(ndvi) ~ scale(map_dif), data = peak)
ndvi.rf.imp$Est[11] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[11] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[11] <- confint(mod)[2,2]



# Winter Pr 12 
mod <- lm(scale(ndvi) ~ scale(pr_w), data = peak)
ndvi.rf.imp$Est[12] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[12] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[12] <- confint(mod)[2,2]



# Elv 13 
mod <- lm(scale(ndvi) ~ scale(elv), data = peak)
ndvi.rf.imp$Est[13] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[13] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[13] <- confint(mod)[2,2]



# MAT 14 
mod <- lm(scale(ndvi) ~ scale(mat_dm), data = peak)
ndvi.rf.imp$Est[14] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[14] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[14] <- confint(mod)[2,2]



# MAT dif 15 
mod <- lm(scale(ndvi) ~ scale(mat_dif), data = peak)
ndvi.rf.imp$Est[15] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[15] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[15] <- confint(mod)[2,2]



# Winter Tmp 16 
mod <- lm(scale(ndvi) ~ scale(tmp_w), data = peak)
ndvi.rf.imp$Est[16] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[16] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[16] <- confint(mod)[2,2]



# Summer Temp 17 
mod <- lm(scale(ndvi) ~ scale(tmp_su), data = peak)
ndvi.rf.imp$Est[17] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[17] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[17] <- confint(mod)[2,2]



# AGDD dif 18 
mod <- lm(scale(ndvi) ~ scale(agdd_dif), data = peak)
ndvi.rf.imp$Est[18] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[18] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[18] <- confint(mod)[2,2]


# Soil 19 


# N dep 20 
mod <- lm(scale(ndvi) ~ scale(ndep), data = peak)
ndvi.rf.imp$Est[20] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[20] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[20] <- confint(mod)[2,2]



# AGDD 21 
mod <- lm(scale(ndvi) ~ scale(agdd), data = peak)
ndvi.rf.imp$Est[21] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[21] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[21] <- confint(mod)[2,2]



# Summer Pr 22 
mod <- lm(scale(ndvi) ~ scale(pr_su), data = peak)
ndvi.rf.imp$Est[22] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[22] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[22] <- confint(mod)[2,2]



# MAP 23 
mod <- lm(scale(ndvi) ~ scale(map_dm), data = peak)
ndvi.rf.imp$Est[23] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[23] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[23] <- confint(mod)[2,2]



# Spring pr 24 
mod <- lm(scale(ndvi) ~ scale(pr_sp), data = peak)
ndvi.rf.imp$Est[24] <- coefficients(mod)[2]
ndvi.rf.imp$Lwr[24] <- confint(mod)[2,1]
ndvi.rf.imp$Upr[24] <- confint(mod)[2,2]




# Mean --------------------------------------------------------------------

act <- as.numeric(mean$ndvi_m>0)
pred <- as.numeric(predict(mndvi.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.507


ndvi2.rf.imp <- varImp(mndvi.rf, type = 1, scale = T)
ndvi2.rf.imp <- ndvi2.rf.imp %>% mutate(var = row.names(ndvi2.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('d','t','c','t','c','t','c','c','c','c','c','d','c','d','c','c','c','c','c','c','c','c','c','t')
ndvi2.rf.imp$var_type <- factor(var.type)



# Fire 1 
mod <- lm(scale(ndvi_m) ~ scale(fire), data = mean)
ndvi2.rf.imp$Est[1] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[1] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[1] <- confint(mod)[2,2]



# Slope 2 
mod <- lm(scale(ndvi_m) ~ scale(slp), data = mean)
ndvi2.rf.imp$Est[2] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[2] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[2] <- confint(mod)[2,2]



# CV 3  
mod <- lm(scale(ndvi_m) ~ scale(cv), data = mean)
ndvi2.rf.imp$Est[3] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[3] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[3] <- confint(mod)[2,2]



# Roughness 4 
mod <- lm(scale(ndvi_m) ~ scale(rough), data = mean)
ndvi2.rf.imp$Est[4] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[4] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[4] <- confint(mod)[2,2]



# Winter Pr dif 5 
mod <- lm(scale(ndvi_m) ~ scale(pr_w_dif), data = mean)
ndvi2.rf.imp$Est[5] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[5] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[5] <- confint(mod)[2,2]



# Elv 6
mod <- lm(scale(ndvi_m) ~ scale(elv), data = mean)
ndvi2.rf.imp$Est[6] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[6] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[6] <- confint(mod)[2,2]



# MAP dif 7
mod <- lm(scale(ndvi_m) ~ scale(map_dif), data = mean)
ndvi2.rf.imp$Est[7] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[7] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[7] <- confint(mod)[2,2]



# Spring Pr dif 8 
mod <- lm(scale(ndvi_m) ~ scale(pr_sp_dif), data = mean)
ndvi2.rf.imp$Est[8] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[8] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[8] <- confint(mod)[2,2]



# Summer temp dif 9 
mod <- lm(scale(ndvi_m) ~ scale(tmp_su_dif), data = mean)
ndvi2.rf.imp$Est[9] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[9] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[9] <- confint(mod)[2,2]



# Winter Pr 10 
mod <- lm(scale(ndvi_m) ~ scale(pr_w), data = mean)
ndvi2.rf.imp$Est[10] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[10] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[10] <- confint(mod)[2,2]



# Winter Tmp 11 
mod <- lm(scale(ndvi_m) ~ scale(tmp_w), data = mean)
ndvi2.rf.imp$Est[11] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[11] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[11] <- confint(mod)[2,2]



# Cow 12 
mod <- lm(scale(ndvi_m) ~ scale(cow), data = mean)
ndvi2.rf.imp$Est[12] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[12] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[12] <- confint(mod)[2,2]



# MAT 13 
mod <- lm(scale(ndvi_m) ~ scale(mat_dm), data = mean)
ndvi2.rf.imp$Est[13] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[13] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[13] <- confint(mod)[2,2]



# N dep 14 
mod <- lm(scale(ndvi_m) ~ scale(ndep), data = mean)
ndvi2.rf.imp$Est[14] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[14] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[14] <- confint(mod)[2,2]



# Summer pr dif 15 
mod <- lm(scale(ndvi_m) ~ scale(pr_su_dif), data = mean)
ndvi2.rf.imp$Est[15] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[15] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[15] <- confint(mod)[2,2]



# AGDD dif 16 
mod <- lm(scale(ndvi_m) ~ scale(agdd_dif), data = mean)
ndvi2.rf.imp$Est[16] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[16] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[16] <- confint(mod)[2,2]



# Summer Temp 17 
mod <- lm(scale(ndvi_m) ~ scale(tmp_su), data = mean)
ndvi2.rf.imp$Est[17] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[17] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[17] <- confint(mod)[2,2]



# Summer Pr 18 
mod <- lm(scale(ndvi_m) ~ scale(pr_su), data = mean)
ndvi2.rf.imp$Est[18] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[18] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[18] <- confint(mod)[2,2]



# Spring pr 19 
mod <- lm(scale(ndvi_m) ~ scale(pr_sp), data = mean)
ndvi2.rf.imp$Est[19] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[19] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[19] <- confint(mod)[2,2]



# AGDD 20 -
mod <- lm(scale(ndvi_m) ~ scale(agdd), data = mean)
ndvi2.rf.imp$Est[20] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[20] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[20] <- confint(mod)[2,2]



# MAP 21 
mod <- lm(scale(ndvi_m) ~ scale(map_dm), data = mean)
ndvi2.rf.imp$Est[21] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[21] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[21] <- confint(mod)[2,2]



# MAT dif 22 
mod <- lm(scale(ndvi_m) ~ scale(mat_dif), data = mean)
ndvi2.rf.imp$Est[22] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[22] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[22] <- confint(mod)[2,2]



# Winter Temp dif 23 
mod <- lm(scale(ndvi_m) ~ scale(tmp_w_dif), data = mean)
ndvi2.rf.imp$Est[23] <- coefficients(mod)[2]
ndvi2.rf.imp$Lwr[23] <- confint(mod)[2,1]
ndvi2.rf.imp$Upr[23] <- confint(mod)[2,2]



# Soil 24 





# LAI ---------------------------------------------------------------------

act <- as.numeric(lai$lai>0)
pred <- as.numeric(predict(lai.rf))-1

R2 <- 1 - (sum((act-pred)^2)/sum((act-mean(act))^2)); R2 #0.309


lai.rf.imp <- varImp(lai.rf, type = 1, scale = T)
lai.rf.imp <- lai.rf.imp %>% mutate(var = row.names(lai.rf.imp)) %>%
        arrange(desc(Overall)) %>%
        mutate(var = factor(var, levels = var)) %>%
        mutate(var = revalue(var, var.names)) %>%
        cbind(y = rev(seq(24))) %>%
        mutate(Overall = range01(Overall)) %>%
        mutate(Est = rep(0), Upr = rep(0), Lwr = rep(0))

var.type <- c('t','t','c','d','c','c','c','c','c','c','d','t','c','c','d','c','c','c','c','c','c','c','c','t')
lai.rf.imp$var_type <- factor(var.type)


# Roughness 1 
mod <- lm(scale(lai) ~ scale(rough), data = lai)
lai.rf.imp$Est[1] <- coefficients(mod)[2]
lai.rf.imp$Lwr[1] <- confint(mod)[2,1]
lai.rf.imp$Upr[1] <- confint(mod)[2,2]



# Slope 2 
mod <- lm(scale(lai) ~ scale(slp), data = lai)
lai.rf.imp$Est[2] <- coefficients(mod)[2]
lai.rf.imp$Lwr[2] <- confint(mod)[2,1]
lai.rf.imp$Upr[2] <- confint(mod)[2,2]



# CV 3  
mod <- lm(scale(lai) ~ scale(cv), data = lai)
lai.rf.imp$Est[3] <- coefficients(mod)[2]
lai.rf.imp$Lwr[3] <- confint(mod)[2,1]
lai.rf.imp$Upr[3] <- confint(mod)[2,2]



# Fire 4 
mod <- lm(scale(lai) ~ scale(fire), data = lai)
lai.rf.imp$Est[4] <- coefficients(mod)[2]
lai.rf.imp$Lwr[4] <- confint(mod)[2,1]
lai.rf.imp$Upr[4] <- confint(mod)[2,2]



# Spring Pr dif 5
mod <- lm(scale(lai) ~ scale(pr_sp_dif), data = lai)
lai.rf.imp$Est[5] <- coefficients(mod)[2]
lai.rf.imp$Lwr[5] <- confint(mod)[2,1]
lai.rf.imp$Upr[5] <- confint(mod)[2,2]



# Winter Pr dif 6 
mod <- lm(scale(lai) ~ scale(pr_w_dif), data = lai)
lai.rf.imp$Est[6] <- coefficients(mod)[2]
lai.rf.imp$Lwr[6] <- confint(mod)[2,1]
lai.rf.imp$Upr[6] <- confint(mod)[2,2]



# Winter Pr 7 
mod <- lm(scale(lai) ~ scale(pr_w), data = lai)
lai.rf.imp$Est[7] <- coefficients(mod)[2]
lai.rf.imp$Lwr[7] <- confint(mod)[2,1]
lai.rf.imp$Upr[7] <- confint(mod)[2,2]



# Summer temp dif 8 
mod <- lm(scale(lai) ~ scale(tmp_su_dif), data = lai)
lai.rf.imp$Est[8] <- coefficients(mod)[2]
lai.rf.imp$Lwr[8] <- confint(mod)[2,1]
lai.rf.imp$Upr[8] <- confint(mod)[2,2]



# Summer pr dif 9 
mod <- lm(scale(lai) ~ scale(pr_su_dif), data = lai)
lai.rf.imp$Est[9] <- coefficients(mod)[2]
lai.rf.imp$Lwr[9] <- confint(mod)[2,1]
lai.rf.imp$Upr[9] <- confint(mod)[2,2]



# MAP dif 10 
mod <- lm(scale(lai) ~ scale(map_dif), data = lai)
lai.rf.imp$Est[10] <- coefficients(mod)[2]
lai.rf.imp$Lwr[10] <- confint(mod)[2,1]
lai.rf.imp$Upr[10] <- confint(mod)[2,2]



# Cow 11
mod <- lm(scale(lai) ~ scale(cow), data = lai)
lai.rf.imp$Est[11] <- coefficients(mod)[2]
lai.rf.imp$Lwr[11] <- confint(mod)[2,1]
lai.rf.imp$Upr[11] <- confint(mod)[2,2]



# Elv 12 
mod <- lm(scale(lai) ~ scale(elv), data = lai)
lai.rf.imp$Est[12] <- coefficients(mod)[2]
lai.rf.imp$Lwr[12] <- confint(mod)[2,1]
lai.rf.imp$Upr[12] <- confint(mod)[2,2]



# Winter Temp dif 13 
mod <- lm(scale(lai) ~ scale(tmp_w_dif), data = lai)
lai.rf.imp$Est[13] <- coefficients(mod)[2]
lai.rf.imp$Lwr[13] <- confint(mod)[2,1]
lai.rf.imp$Upr[13] <- confint(mod)[2,2]



# AGDD dif 14 
mod <- lm(scale(lai) ~ scale(agdd_dif), data = lai)
lai.rf.imp$Est[14] <- coefficients(mod)[2]
lai.rf.imp$Lwr[14] <- confint(mod)[2,1]
lai.rf.imp$Upr[14] <- confint(mod)[2,2]



# N dep 15 
mod <- lm(scale(lai) ~ scale(ndep), data = lai)
lai.rf.imp$Est[15] <- coefficients(mod)[2]
lai.rf.imp$Lwr[15] <- confint(mod)[2,1]
lai.rf.imp$Upr[15] <- confint(mod)[2,2]



# Winter Tmp 16 
mod <- lm(scale(lai) ~ scale(tmp_w), data = lai)
lai.rf.imp$Est[16] <- coefficients(mod)[2]
lai.rf.imp$Lwr[16] <- confint(mod)[2,1]
lai.rf.imp$Upr[16] <- confint(mod)[2,2]



# MAT dif 17 
mod <- lm(scale(lai) ~ scale(mat_dif), data = lai)
lai.rf.imp$Est[17] <- coefficients(mod)[2]
lai.rf.imp$Lwr[17] <- confint(mod)[2,1]
lai.rf.imp$Upr[17] <- confint(mod)[2,2]



# Summer Temp 18 
mod <- lm(scale(lai) ~ scale(tmp_su), data = lai)
lai.rf.imp$Est[18] <- coefficients(mod)[2]
lai.rf.imp$Lwr[18] <- confint(mod)[2,1]
lai.rf.imp$Upr[18] <- confint(mod)[2,2]



# MAT 19 
mod <- lm(scale(lai) ~ scale(mat_dm), data = lai)
lai.rf.imp$Est[19] <- coefficients(mod)[2]
lai.rf.imp$Lwr[19] <- confint(mod)[2,1]
lai.rf.imp$Upr[19] <- confint(mod)[2,2]



# MAP 20 
mod <- lm(scale(lai) ~ scale(map_dm), data = lai)
lai.rf.imp$Est[20] <- coefficients(mod)[2]
lai.rf.imp$Lwr[20] <- confint(mod)[2,1]
lai.rf.imp$Upr[20] <- confint(mod)[2,2]



# Spring pr 21 
mod <- lm(scale(lai) ~ scale(pr_sp), data = lai)
lai.rf.imp$Est[21] <- coefficients(mod)[2]
lai.rf.imp$Lwr[21] <- confint(mod)[2,1]
lai.rf.imp$Upr[21] <- confint(mod)[2,2]



# Summer Pr 22 
mod <- lm(scale(lai) ~ scale(pr_su), data = lai)
lai.rf.imp$Est[22] <- coefficients(mod)[2]
lai.rf.imp$Lwr[22] <- confint(mod)[2,1]
lai.rf.imp$Upr[22] <- confint(mod)[2,2]



# AGDD 23 
mod <- lm(scale(lai) ~ scale(agdd), data = lai)
lai.rf.imp$Est[23] <- coefficients(mod)[2]
lai.rf.imp$Lwr[23] <- confint(mod)[2,1]
lai.rf.imp$Upr[23] <- confint(mod)[2,2]


# Soil 24


# Plots --------------------------------------------------------------------



figure(
        ggplot(tc.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(tc.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = tc.rf.imp$y, labels = tc.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.5,0.5)) +
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
        save = T
)



figure(
        ggplot(tc2.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(tc2.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = tc2.rf.imp$y, labels = tc2.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.5,0.5)) +
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
        save = T
)



figure(
        ggplot(tcc.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(tcc.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = tcc.rf.imp$y, labels = tcc.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.5,0.5)) +
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
        save = T
)


figure(
        ggplot(ndvi.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(ndvi.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(x = Est, y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = ndvi.rf.imp$y, labels = ndvi.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.5,0.5)) +
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
        save = T
)

figure(
        ggplot(ndvi2.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(ndvi2.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(x = Est, y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = ndvi2.rf.imp$y, labels = ndvi2.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.5,0.5)) +
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
        save = T
)

figure(
        ggplot(lai.rf.imp) +
                geom_vline(xintercept = 0, color = 'grey60') +
                geom_hline(yintercept = rev(seq(nrow(lai.rf.imp))), linetype = 'dashed', alpha = 0.3) +
                geom_point(aes(x = Est, y = y, color = var_type), size = 6, shape = 16) +
                # geom_errorbarh(aes(x = Est, y = y, xmin = Lwr, xmax = Upr, color = var_type)) +
                scale_y_continuous(breaks = lai.rf.imp$y, labels = lai.rf.imp$var) +
                scale_color_manual(values = c('blue4',  'gold3', 'grey60')) +
                scale_x_continuous(limits = c(-0.5,0.5)) +
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
        save = T
)



# Correlation -------------------------------------------------------------


importance <- rbind(
        cbind.data.frame(Group = rep('TreeCover00'), Variable = tc.rf.imp$var, Overall = tc.rf.imp$Est),
        cbind.data.frame(Group = rep('TreeCover19'), Variable = tc2.rf.imp$var, Overall = tc2.rf.imp$Est),
        cbind.data.frame(Group = rep('TreeCoverChange'), Variable = tcc.rf.imp$var, Overall = tcc.rf.imp$Est),
        cbind.data.frame(Group = rep('PeakNDVI'), Variable = ndvi.rf.imp$var, Overall = ndvi.rf.imp$Est),
        cbind.data.frame(Group = rep('MeanNDVI'), Variable = ndvi2.rf.imp$var, Overall = ndvi2.rf.imp$Est),
        cbind.data.frame(Group = rep('LAI'), Variable = lai.rf.imp$var, Overall = lai.rf.imp$Est)
)

import.mat <- matrify(importance) %>% t %>% as.data.frame()

importance2 <- rbind(
        cbind.data.frame(Group = rep('TreeCover00'), Variable = tc.rf.imp$var, Overall = tc.rf.imp$Est),
        cbind.data.frame(Group = rep('TreeCover19'), Variable = tc2.rf.imp$var, Overall = tc2.rf.imp$Est),
        cbind.data.frame(Group = rep('TreeCoverChange'), Variable = tcc.rf.imp$var, Overall = tcc.rf.imp$Est)
)

import.mat2 <- matrify(importance2) %>% t %>% as.data.frame() %>% filter(TreeCover00>0)
import.mat3 <- matrify(importance) %>% t %>% as.data.frame() %>% filter(TreeCover00>0)

cor(import.mat, method = c("spearman"))
cor(import.mat2, method = c("spearman"))
cor(import.mat3, method = c('spearman'))
