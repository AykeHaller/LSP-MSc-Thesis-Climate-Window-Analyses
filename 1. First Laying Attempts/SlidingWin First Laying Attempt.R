# Paper: Weather Conditions Influence Breeding Initiation of an Intra-Tropical Migrant Bird
# First Laying attempt analysis

# Packages
library(climwin)
library(lme4)
library(MuMIn)

# Import breeding data
df <- read.csv("1.LSP_first_laying_attempt.csv")

df$maleplumage <- as.factor(df$maleplumage)
df$season <- as.factor(df$season)

# Import climate data
clim <- read.csv("climate_data_13_23.csv")

# Baseline model
baseline <- lmer(layingday ~ maleplumage + (1|season), data = df, REML = F)
AICc(baseline)
r.squaredGLMM(baseline)


# Sliding window analysis
firstlay_relative <- slidingwin(baseline = 
                                  lmer(layingday ~ maleplumage + (1|season), data = df, REML = F),
                                xvar = list(avrTemp = clim$avr_temp, maxTemp = clim$max_temp,
                                            minTemp = clim$min_temp, Rain = clim$tot_prec,
                                            rH = clim$avr_rH, Wind = clim$avr_wind_vel),
                                type = "relative", 
                                range = c(90, 0),
                                stat = c("mean"), 
                                func = c("lin", "quad"),
                                cinterval = "day",
                                cdate = clim$date, bdate = df$laying_date)

# plot results
plotall(datasetrand = NULL,
        bestmodel = firstlay_relative[[4]]$BestModel,
        bestmodeldata = firstlay_relative[[4]]$BestModelData,
        dataset = firstlay_relative[[4]]$Dataset,
        arrow = T)

plotall(datasetrand = NULL,
        bestmodel = firstlay_relative[[6]]$BestModel,
        bestmodeldata = firstlay_relative[[6]]$BestModelData,
        dataset = firstlay_relative[[6]]$Dataset,
        arrow = T)


# Randomised test
firstlay_relrand_best <- randwin(repeats = 100, baseline = 
                                  lmer(layingday ~ maleplumage + (1|season), data = df, REML = F),
                                xvar = list(maxTemp = clim$max_temp,
                                             Rain = clim$tot_prec
                                            ),
                                type = "relative", 
                                range = c(90, 0),
                                stat = c("mean"), 
                                func = c("lin", "quad"),
                                cinterval = "day",
                                cdate = clim$date, bdate = df$laying_date)



pvalue(dataset= firstlay_relative[[10]]$Dataset,
       datasetrand = firstlay_relrand_best[[4]],
       metric = "AIC", sample.size = 87)

plotall(datasetrand = firstlay_relrand_best[[3]],
        bestmodel = firstlay_relative[[8]]$BestModel,
        bestmodeldata = firstlay_relative[[2]]$BestModelData,
        dataset = firstlay_relative[[2]]$Dataset,
        arrow = T)
