# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
# Renesting Probability analysis


bio <- read.csv("10.LSP_renesting_probability.csv")
clim <- read.csv("climate_data_13_23.csv")
library(lme4)
library(MuMIn)
library(climwin)


# Renesting Probability	Last Active Date	Nest Success	Male Plumage	Female	Season
bio$renesting <- as.factor(bio$renesting)
bio$maleplumage <- as.factor(bio$maleplumage)
bio$nest_succes <- as.factor(bio$nest_succes)
bio$season <- as.factor(bio$season)
bio$ring_female <- as.factor(bio$ring_female)


baseline <- glmer(renesting ~ last_active_day + maleplumage + nest_succes + 
                    (1|season) + (1|ring_female), data = bio, family = binomial(link = "logit"),
                  control = glmerControl(optimizer = "bobyqa")) 

summary(baseline)
AICc(baseline)
r.squaredGLMM(baseline)

## Relative Analysis
relative_renesting_lin <- slidingwin(baseline = 
                                   glmer(renesting ~ last_active_day + maleplumage + nest_succes + 
                                           (1|season) + (1|ring_female), data = bio, family = binomial(link = "logit"),
                                         control = glmerControl(optimizer = "bobyqa")),
                                     xvar = list(avrTemp = clim$avr_temp, maxTemp = clim$max_temp,
                                                 minTemp = clim$min_temp, Rain = clim$tot_prec,
                                                 rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                                     type = "relative", 
                                     range = c(90, 0),
                                     stat = c("mean"), 
                                     func = c("lin"),
                                     cinterval = "day",
                                     cdate = clim$date, bdate = bio$Laydate)

relative_renesting_quad <- slidingwin(baseline = 
                                       glmer(renesting ~ last_active_day + maleplumage + nest_succes + 
                                               (1|season) + (1|ring_female), data = bio, family = binomial(link = "logit"),
                                             control = glmerControl(optimizer = "bobyqa")),
                                     xvar = list(avrTemp = clim$avr_temp_c, maxTemp = clim$max_temp_c,
                                                 minTemp = clim$min_temp_c, Rain = clim$tot_prec_c,
                                                 rH = clim$avr_rH_c, Wind = clim$avr_wind_vel_c),
                                     type = "relative", 
                                     range = c(90, 0),
                                     stat = c("mean"), 
                                     func = c("quad"),
                                     cinterval = "day",
                                     cdate = clim$date, bdate = bio$Laydate)