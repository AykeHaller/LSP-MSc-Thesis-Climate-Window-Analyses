# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
# Nest Success analysis
 
bio <- read.csv("5.LSP_Nest_success.csv")
clim <- read.csv("climate_data_13_23.csv")

library(climwin)
library(lme4)
library(MuMIn)

# Nest Success	Laying Date		Male Plumage		Season
bio$plumage <- as.factor(bio$plumage)
bio$season <- as.factor(bio$season)
bio$success <- as.factor(bio$success)

# Centre an Scale Continuous Laying Day Variable for better model fit
bio$Laying_Day_c <- scale(bio$Laying_Day, center = T, scale = T)

# baseline 
baseline <- glmer(success ~ Laying_Day_c + plumage + (1|season), data = bio, 
                  family = binomial(link = "logit"), 
                  control = glmerControl(optimizer = "bobyqa"))

AICc(baseline)
r.squaredGLMM(baseline)
summary(baseline)

relative_nestsuccess  <- slidingwin(baseline= 
                                           glmer(success ~ Laying_Day + plumage + (1|season), data = bio, 
                                                 family = binomial(link = "logit"),
                                       control = glmerControl(optimizer = "bobyqa")),
                               xvar = list(Temp = clim$avr_temp, Rain = clim$tot_prec,
                                           MaxTemp = clim$max_temp, MinTemp = clim$min_temp,
                                           rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                               type = "relative",
                               range = c(90, 0),
                               stat = c("mean"),
                               func = c("lin"),
                               cinterval = "day",
                               cdate = clim$date, bdate = bio$fail_fledge_date)




best_nestsuccess_rand  <- randwin(repeats = 100,
                                       baseline= 
                                           glmer(success ~ Laying_Day + plumage + (1|season), data = bio, 
                                                 family = binomial(link = "logit"),
                                                 control = glmerControl(optimizer = "bobyqa")),
                                         xvar = list(MinTemp = clim$min_temp
                                                     ),
                                         type = "relative",
                                         range = c(90, 0),
                                         stat = c("mean"),
                                         func = c("lin"),
                                         cinterval = "day",
                                         cdate = clim$date, bdate = bio$fail_fledge_date)
