# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
# Clutch size analysis

bio <- read.csv("4.LSP_clutch_size.csv")
clim <- read.csv("climate_data_13_23.csv",)

library(climwin)
library(lme4)
library(MuMIn)

bio$maleplumage <- as.factor(bio$maleplumage)
bio$season <- as.factor(bio$season)

# Baseline 
baseline <- lmer(clutchsize ~ maleplumage + Laying_Day + (1|season),
                 data = bio, REML = F)

summary(baseline)
AICc(baseline)
r.squaredGLMM(baseline)

#### Relative Analysis
relative_clutchsize <- slidingwin(baseline = 
                                    lmer(clutchsize ~ maleplumage + Laying_Day + (1|season),
                                         data = bio, REML = F),
                              xvar = list(avrTemp = clim$avr_temp, maxTemp = clim$max_temp,
                                          minTemp = clim$min_temp, Rain = clim$tot_prec,
                                          rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                              type = "relative", 
                              range = c(90, 0),
                              stat = c("mean"), 
                              func = c("lin", "quad"),
                              cinterval = "day",
                              cdate = clim$date, bdate = bio$layingdate)

relative_clutchsize$combos

