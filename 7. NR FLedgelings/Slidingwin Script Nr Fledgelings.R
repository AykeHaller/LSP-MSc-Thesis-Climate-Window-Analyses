# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
## Number of Fledgelings analysis

bio <- read.csv("7.LSP_nr_fledgelings.csv")
clim <- read.csv("climate_data_13_23.csv",)

library(climwin)
library(lme4)
library(MuMIn)

# Baseline
bio$maleplumage <- as.factor(bio$maleplumage)
bio$ring_female <- as.factor(bio$ring_female)
bio$season <- as.factor(bio$season)

baseline <- lmer(fledge ~ maleplumage + hatchday + 
                   (1|ring_female), data = bio, REML = F)

summary(baseline)
AICc(baseline)
r.squaredGLMM(baseline)

#### Relative Analysis
relative_nrFledge <- slidingwin(baseline = 
                                  lmer(fledge ~ maleplumage + hatchday  +
                                         (1|ring_female), data = bio, REML = F),
                                  xvar = list(avrTemp = clim$avr_temp, maxTemp = clim$max_temp,
                                              minTemp = clim$min_temp, Rain = clim$tot_prec,
                                              rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                                  type = "relative", 
                                  range = c(90, 0),
                                  stat = c("mean"), 
                                  func = c("quad", "lin"),
                                  cinterval = "day",
                                  cdate = clim$date, bdate = bio$hatch_date)

relative_nrFledge$combos

