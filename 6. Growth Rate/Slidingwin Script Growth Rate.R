# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
# Growth Rate analysis

bio <- read.csv("6.LSP_Growth_rate.csv")
clim <- read.csv("climate_data_13_23.csv",)

library(climwin)
library(lme4)
library(MuMIn)

bio$maleplumage <- as.factor(bio$maleplumage)
bio$Season <- as.factor(bio$Season)
bio$nest_id <- as.factor(bio$nest_id)

# Baseline 
baseline <- lmer(rgr ~ maleplumage + hatchdate  + I(hatchdate^2) +
                   (1|Season) + (1|nest_id), data = bio, REML = F)

summary(baseline)
AICc(baseline)
r.squaredGLMM(baseline)

#### Relative Analysis
relative_growthrate <- slidingwin(baseline = 
                                    lmer(rgr ~ maleplumage + hatchdate  + I(hatchdate^2) +
                                           (1|Season) + (1|nest_id), data = bio, REML = F),
                                  xvar = list(avrTemp = clim$avr_temp, maxTemp = clim$max_temp,
                                              minTemp = clim$min_temp, Rain = clim$tot_prec,
                                              rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                                  type = "relative", 
                                  range = c(90, 0),
                                  stat = c("mean"), 
                                  func = c("lin", "quad"),
                                  cinterval = "day",
                                  cdate = clim$date, bdate = bio$Hatch_Date)

relative_growthrate$combos



## Best Relative Models Randomised
relative_growthrate_rand <- randwin(repeats = 100, baseline = 
                                      lmer(rgr ~ maleplumage + hatchdate  + I(hatchdate^2) +
                                             (1|Season) + (1|nest_id), data = bio, REML = F),
                                 xvar = list(maxTemp = clim$max_temp,
                                             Rain = clim$tot_prec
                                 ),
                                 type = "relative", 
                                 range = c(90, 0),
                                 stat = c("mean"), 
                                 func = c("lin", "quad"),
                                 cinterval = "day",
                                 cdate = clim$date, bdate = bio$Hatch_Date)

pvalue(datasetrand = relative_growthrate_rand[[4]],
       dataset = relative_growthrate[[10]]$Dataset,
       metric = "AIC", sample.size = 186)
