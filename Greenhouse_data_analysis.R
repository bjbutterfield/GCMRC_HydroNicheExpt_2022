setwd('C:/Users/bjbut/OneDrive/Documents/GitHub/GCMRC_HydroNicheExpt_2022')
gs_dat <- read.csv('Stomatal_Conductance_Data.csv')#reading in stomatal conductance data

#setting gs values of dead plants (NAs) to the minimum measured conductance
gs_dat$Conductance[is.na(gs_dat$Conductance)] <- min(gs_dat$Conductance,na.rm=T)

#converting time data to numeric, where the first measurement is time zero, and subsequent measurements are seconds after zero
dt <- paste(gs_dat$Date, gs_dat$Time, sep = ' ')
dt <- as.numeric(as.POSIXct(dt))
dt <- dt-min(dt)
gs_dat$seconds_elapsed <- dt

#creating a new treatment variable
Rx <- rep(NA,nrow(gs_dat))
Rx[gs_dat$Bin == 'A' | gs_dat$Bin == 'D' | gs_dat$Bin == 'G' | gs_dat$Bin == 'J' | gs_dat$Bin == 'M'] <- 'Drought'
Rx[gs_dat$Bin == 'B' | gs_dat$Bin == 'E' | gs_dat$Bin == 'H' | gs_dat$Bin == 'K' | gs_dat$Bin == 'N'] <- 'Control'
Rx[gs_dat$Bin == 'C' | gs_dat$Bin == 'F' | gs_dat$Bin == 'I' | gs_dat$Bin == 'L' | gs_dat$Bin == 'O'] <- 'Flood'
gs_dat$Rx <- Rx

#creating a unique ID for each plant
plant_ID <- paste(gs_dat$Bin,gs_dat$Species,sep='')
gs_dat$plant_ID <- plant_ID

#quick visualizations
boxplot(Conductance ~ Species, data = gs_dat)
boxplot(Conductance ~ Species*Rx, data = gs_dat)

# to do:
# interpolate leaf temp values for dead plants
# get leaf temperature data for CERE on day two
# 


#mixed effects models to account for repeated measures (plant_ID), nested design (bin), and leaf temperature as a covariate
library(nlme)
gs_mod <- lme(Conductance ~ Species*Rx*seconds_elapsed + Leaf.Temp + (1|Bin) + (1|plant_ID), data = gs_dat)
