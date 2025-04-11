# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
# Hatch Probability analysis

bio <- read.csv("9.LSP_hatch_probability.csv")
clim <- read.csv("climate_data_13_23.csv",)

library(climwin)
library(lme4)
library(MuMIn)


# Baseline
bio$maleplumage <- as.factor(bio$maleplumage)
bio$season <- as.factor(bio$season)
bio$odd_hatch <- as.factor(bio$odd_hatch)
bio$clutch_size <- as.factor(bio$clutch_size)


baseline <- glmer(odd_hatch ~ maleplumage + Incube_day + clutch_size +
                    (1|season), data = bio, family = binomial(link = "logit"),
                  control = glmerControl(optimizer = "bobyqa"))

AICc(baseline)
r.squaredGLMM(baseline)
summary(baseline)

relative_hatch_prob <- slidingwin(baseline= 
                                          glmer(odd_hatch ~ maleplumage + 
                                                  Incube_day + clutch_size + (1|season), data = bio, 
                                                family = binomial(link = "logit"),
                                                control = glmerControl(optimizer = "bobyqa")),
                                        xvar = list(
                                                    MaxTemp = clim$max_temp 
                                                    ),
                                        type = "relative",
                                        range = c(90, 0),
                                        stat = c("mean"),
                                        func = c("quad"),
                                        cinterval = "day",
                                        cdate = clim$date, bdate = bio$incube_date)

relative_hatch_prob$combos

candidate_hatch_prob_random <- randwin(repeats = 100,
                            baseline= 
                              glmer(odd_hatch ~ maleplumage + Incube_day + clutch_size +
                                      (1|season), data = bio, family = binomial(link = "logit"),
                                    control = glmerControl(optimizer = "bobyqa")),
                            xvar = list(MaxTemp = clim$max_temp_c),
                            type = "relative",
                            range = c(90, 0),
                            stat = c("mean"),
                            func = c("quad"),
                            cinterval = "day",
                            cdate = clim$date, bdate = bio$incube_date)
