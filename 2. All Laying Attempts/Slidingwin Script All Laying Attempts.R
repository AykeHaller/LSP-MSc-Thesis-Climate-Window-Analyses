# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
## All Laying Attempts

# Packages
library(climwin)
library(lme4)
library(MuMIn)

## Import Datasets
bio <- read.csv("2.LSP_all_laying_attempts.csv")
clim <- read.csv("climate_data_13_23.csv",)

bio$maleplumage <- as.factor(bio$maleplumage)
bio$season <- as.factor(bio$season)

# Baseline
baseline <- lmer(lay_day ~ maleplumage + (1|season), data = bio, REML = F)
AICc(baseline)
r.squaredGLMM(baseline)

#### Relative Analysis
relative_all_layday <- slidingwin(baseline = 
                                    lmer(lay_day ~ maleplumage + (1|season), data = bio, REML = F),
                                    xvar = list(avrTemp = clim$avr_temp, maxTemp = clim$max_temp,
                                                minTemp = clim$min_temp, Rain = clim$tot_prec,
                                                rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                                    type = "relative", 
                                    range = c(90, 0),
                                    stat = c("mean"), 
                                    func = c("lin", "quad"),
                                    cinterval = "day",
                                    cdate = clim$date, bdate = bio$laying_date)

relative_all_layday$combos
plotall(datasetrand = NULL,
        bestmodel = relative_all_layday[[6]]$BestModel,
        bestmodeldata = relative_all_layday[[6]]$BestModelData,
        dataset = relative_all_layday[[6]]$Dataset,
        arrow = T)