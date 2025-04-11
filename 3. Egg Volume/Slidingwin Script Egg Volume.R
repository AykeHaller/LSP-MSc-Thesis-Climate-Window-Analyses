# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
# Egg Volume analysis


bio <- read.csv("3.LSP_egg_volume.csv")
clim <- read.csv("climate_data_13_23.csv",)
library(climwin)
library(lme4)
library(MuMIn)

# Egg Volume	Laying Date		Male Plumage	Female	Season	Nest id
bio$maleplumage<- as.factor(bio$maleplumage)
bio$nest_id <- as.factor(bio$nest_id)
bio$ring_female <- as.factor(bio$ring_female)
bio$season <- as.factor(bio$season)

# Baseline
baseline <- lmer(egg_vol ~ Laying_Day + maleplumage + (1|ring_female) +
                   (1|season) + (1|nest_id), data = bio, REML = F)
AICc(baseline)
r.squaredGLMM(baseline)

#### Relative Analysis
relative_eggvol <- slidingwin(baseline = 
                                    lmer(egg_vol ~ Laying_Day + maleplumage + (1|ring_female) +
                                            (1|season) + (1|nest_id), data = bio, REML = F),
                                  xvar = list(avrTemp = clim$avr_temp, maxTemp = clim$max_temp,
                                              minTemp = clim$min_temp, Rain = clim$tot_prec,
                                              rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                                  type = "relative", 
                                  range = c(90, 0),
                                  stat = c("mean"), 
                                  func = c("lin", "quad"),
                                  cinterval = "day",
                                  cdate = clim$date, bdate = bio$laydate)

relative_eggvol$combos

