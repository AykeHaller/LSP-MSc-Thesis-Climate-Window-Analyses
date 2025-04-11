# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
# hatch succes analysis


bio <- read.csv("8.LSP_Hatch_success.csv")
clim <- read.csv("def_florestalclimate_13_22.csv")
library(climwin)
library(lme4)
library(MuMIn)

bio$maleplumage <- as.factor(bio$maleplumage)
bio$season <- as.factor(bio$season)
bio$ring_number_female <- as.factor(bio$ring_number_female)

baseline <- lmer(hatch_succes ~ Incubation_day + I(Incubation_day^2) + maleplumage +
                (1|season) + (1|ring_number_female), data = bio, REML = F)

AICc(baseline)
r.squaredGLMM(baseline)

rel_result <- slidingwin(baseline = 
                           lmer(hatch_succes ~ Incubation_day + I(Incubation_day^2) + maleplumage +
                                  (1|season) + (1|ring_number_female), data = bio, REML = F),
                         xvar = list(Temp = clim$avr_temp, Rain = clim$tot_prec,
                                     MinTemp = clim$min_temp, MaxTemp = clim$max_temp,
                                     rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                         type = "relative", 
                         range = c(90, 0),
                         stat = c("mean"), 
                         func = c("lin", "quad"),
                         cinterval = "day",
                         cdate = clim$date, bdate = bio$incubation_date)
