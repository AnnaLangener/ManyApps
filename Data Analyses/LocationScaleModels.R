#### ManyApps ####
#### Locataion-Scale Models ####
#### \TE 06_26


library(tidyverse)
library(ggplot2)
library(viridis)
library(ggpubr)
library(sjPlot)
library(lme4)
library(lmerTest)
library(glmmTMB)

dat <- read_csv("../Data/processed_data/Final_noapp_overview.csv")
dat$id <- dat$unique_participant_number
dat$id_numeric <- as.numeric(factor(dat$id))
dat$gender_numeric <- as.numeric(factor(dat$gender, levels = c("male","female","other"))) # 1 = male, 2 = female, 3 = other
#table(dat$gender,dat$gender_numeric) #check 
dat$weekend <- timeDate::isWeekend(dat$day)*1

# Aggregating to the daily level
dat_daily <- dat %>%
  group_by(
    id,
    id_numeric, 
    day, 
    weekend,
    Dataset, 
    age, 
    gender, 
    gender_numeric, 
    PHQ, 
    country, 
    PANAS_POS, 
    PANAS_NEG
  ) %>%
  summarise(
    # Summing the hourly columns as requested
    total_daily_usage_raw = sum(hourly_usage_raw, na.rm = TRUE),
    total_daily_app_usage = sum(total_hourly_app_usage, na.rm = TRUE),
    unique_apps_day = mean(unique_apps_day, na.rm = T),
    .groups = "drop"
  ) %>%
  mutate(daily_usage_hours = total_daily_app_usage / 3600) %>%
  filter(!is.na(gender), !is.na(age), !is.na(weekend), !is.na(daily_usage_hours)) %>%
  # Ensure character variables are treated as factors for model matrix generation
  mutate(
    Dataset = factor(Dataset, levels = c(
      "Aidan_APPUSAGE", "Behapp_PRISM", "Chow_APPUSAGE", "MoodyLife", 
      "Spain 1", "Spain 2", "Study_Smart_W1", "Study_Smart_W2", 
      "Study_Smart_W3", "WHALE", "Yannik", "disconnect", 
      "eMotion", "klingelhoefer_disconnection"
    )),
    gender = factor(gender, levels = c("male", "female", "other"))
  )

# 2. Build the numeric design matrix for categorical predictors
# This automatically creates K-1 dummies (omitting reference levels)
dummies <- model.matrix(~ gender + Dataset, data = dat_daily)[, -1]

# 3. Bind the numeric dummies back to your main dataset
dat_daily <- bind_cols(dat_daily, as.data.frame(dummies))

# Quick verification: Check the clean numeric column names generated
colnames(dummies)


# daily_numeric_withoutWB <- dat_daily[,c("id_numeric","age","gender_numeric","weekend","total_daily_usage_raw","total_daily_app_usage")]
# daily_numeric_withAffect <- dat_daily[,c("id_numeric","age","gender_numeric","weekend","PANAS_POS","PANAS_NEG","total_daily_usage_raw","total_daily_app_usage")]
# daily_numeric_withPHQ <- dat_daily[,c("id_numeric","age","gender_numeric","weekend","PHQ","total_daily_usage_raw","total_daily_app_usage")]

#write.csv(daily_numeric_withoutWB[complete.cases(daily_numeric_withoutWB),],"../Data/processed_data/daily_numeric_completeCases_withoutWB.csv")
#write.csv(daily_numeric_withAffect[complete.cases(daily_numeric_withAffect),],"../Data/processed_data/daily_numeric_completeCases_withAffect.csv")
#write.csv(daily_numeric_withPHQ[complete.cases(daily_numeric_withPHQ),],"../Data/processed_data/daily_numeric_completeCases_withAffect.csv")


### some descriptives to get to know the data ####

table(dat$gender, useNA = "always")
table(dat$age, useNA = "always")

##RQ 3: (a) How do daily total usage time and app diversity differ by age, gender, well-being, 
##time of day (with hourly usage time), and weekday vs. weekend? (b) 
## How does within-person variability in daily total usage time and app diversity differ across age, gender, well-being, and sample?


## We then estimated location-scale mixed models (Hedeker & Mermelstein, 2007), 
# which simultaneously model: (i) the average level (location) of daily smartphone use or app diversity (addressing RQ3a), 
# and (ii) the within-person variability (scale) in these measures (addressing RQ3b) as a function of person-level predictors. 
# The primary outcomes were daily total smartphone use, operationalized as the total number of minutes the smartphone was used 
# in the foreground per day, and daily app diversity, operationalized as the number of distinct applications used per day. 
# A weekend indicator (0 = weekday, 1 = weekend) was included as a level 1 predictor. Age, gender, well-being, and sample 
# (categorical dataset indicator) were included as level 2 predictors. For RQ3a, the location level-2 predictors are focal.
# For RQ3b, the scale level-2 predictors are focal. 

# To examine time-of-day effects, we estimated separate location-only mixed 
# models using hourly observations as the unit of analysis. In these models, the outcome variables were hourly smartphone use and 
# hourly app diversity, and hour of day was included as a categorical predictor, while retaining the same set of Level-2 predictors. 

### RQ3


library(LMMELSM)
# fit <- lmmelsm(
#   formula = list(
#     observed ~ daily_usage_hours,       # <-- 'observed' must be on the LHS, outcome on the RHS
#     location ~ weekend,    # Predictor for the mean
#     scale ~ weekend        # Predictor for the within-person variance
#   ),
#   group = id,            
#   data = dat_daily, 
#   cores = 2,
#   iter = 1000,
#   warmup = 500
# )
# 
# summary(fit)

# m1 <- lmmelsm(
#   formula = list(
#     observed ~ daily_usage_hours,
#     
#     # Location predictors (including all 13 dataset dummies and 2 gender dummies)
#     location ~ age + genderfemale + genderother + weekend + 
#       DatasetBehapp_PRISM + DatasetChow_APPUSAGE + DatasetMoodyLife + 
#       `DatasetSpain 1` + `DatasetSpain 2` + DatasetStudy_Smart_W1 + 
#       DatasetStudy_Smart_W2 + DatasetStudy_Smart_W3 + DatasetWHALE + 
#       DatasetYannik + Datasetdisconnect + DataseteMotion + 
#       Datasetklingelhoefer_disconnection,
#     
#     # Scale predictors 
#     scale ~ age + genderfemale + genderother + weekend
#   ),
#   group = id,            
#   data = dat_daily,  
#   cores = 2#,
#   #iter = 1000,   
#   #warmup = 500
# )
# 
# summary(m1)
# save(m1, file = paste0("fits/lmmeslm_fit_m1",Sys.Date(),".RData"))
# system("say done")


# equivalent glmmTMB model?

model_test <- glmmTMB(
  daily_usage_hours ~ weekend + (1 | id), # location 
  dispformula = ~ weekend + (1 | id),  # scale?
  data = dat_daily,
  family = gaussian()
)
summary(model_test)

#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### models with all data and no well-being score (aka base) ####

# Model for Daily Total Usage Time
# Note: dispersion formula models the 'scale' (within-person variability; see Nakagawa et al., 2025 for details and a tutorial)
m_duration_base <- glmmTMB(
  daily_usage_hours ~ age + gender + weekend + Dataset + (1 | id),
  dispformula = ~ age + gender + weekend  + Dataset + (1 | id), 
  data = dat_daily,
  family = gaussian()
)
summary(m_duration_base)

# center age!!

# Model for Daily App Diversity

##########################################################################
####### CHEAP WORKAROUND until inconsistency is worked out with count data ###############
##########################################################################

dat_daily$unique_apps_day_rounded <- round(dat_daily$unique_apps_day)  
m_apps_base <- glmmTMB(
  unique_apps_day_rounded ~ age + gender + weekend + Dataset + (1 | id),
  dispformula = ~ age + gender + weekend  + Dataset+  (1 | id),
  data = dat_daily,
  family = gaussian() ################## should be poission or nbinom2, but both do not work !!!!
  # family = nbinom2() # dispormula is not supported for poission (count) distribution
)
summary(m_apps_base)

# Convert hour to factor to create dummy variables automatically
dat <- dat %>% mutate(hour_fact = factor(hour(hour_start)))

# m_duration_hourly_base <- glmmTMB(
#   total_hourly_app_usage ~ hour_fact + age + gender + Dataset + (1 | id),
#   data = dat,
#   family = gaussian()
# )

m_duration_hourly_base <- lmer(
  total_hourly_app_usage ~ hour_fact + age + gender + Dataset + (1 | id),
  data = dat)

m_apps_hourly_base <- lmer(
  unique_apps_hour ~ hour_fact + age + gender + Dataset + (1 | id),
  data = dat, family = possion())


summary(m_duration_base)
summary(m_apps_base)
summary(m_duration_hourly_base)
summary(m_apps_hourly_base)

#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### models with affect ####

# Model for Daily Total Usage Time
# Note: dispersion formula models the 'scale' (within-person variability; see Nakagawa et al., 2025 for details and a tutorial)
m_duration_affect <- glmmTMB(
  daily_usage_hours ~ age + gender + weekend +Dataset+ PANAS_NEG + PANAS_POS  + (1 | id),
  dispformula = ~ age + gender + weekend +Dataset + PANAS_NEG + PANAS_POS + (1 | id), 
  data = dat_daily,
  family = gaussian() 
)
summary(m_duration_affect)

# Model for Daily App Diversity
m_apps_affect<- glmmTMB(
  unique_apps_day_rounded ~ age + gender + weekend +Dataset + PANAS_NEG + PANAS_POS + (1 | id),
  dispformula = ~ age + gender + weekend +Dataset + PANAS_NEG + PANAS_POS + (1 | id),
  data = dat_daily,
  family = gaussian() ################## should be poission or nbinom2, but both do not work !!!!
  #family = nbinom2()  #poisson() # Use poisson for count data (number of apps)
)
summary(m_apps_affect)

summary(m_duration_affect)
summary(m_apps_affect)

#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### models with PHQ ####

# Model for Daily Total Usage Time
# Note: dispersion formula models the 'scale' (within-person variability; see Nakagawa et al., 2025 for details and a tutorial)
m_duration_phq <- glmmTMB(
  daily_usage_hours ~ age + gender + weekend +Dataset + PHQ + (1 | id),
  dispformula = ~ age + gender + weekend + Dataset+ PHQ + (1 | id), 
  data = dat_daily,
  family = gaussian()
)
summary(m_duration_phq)

# Model for Daily App Diversity
m_apps_phq<- glmmTMB(
  unique_apps_day_rounded ~ age + gender + weekend + Dataset + PHQ +(1 | id),
  dispformula = ~ age + gender + weekend + Dataset+ PHQ + (1 | id),
  data = dat_daily,
  family = gaussian() ################## should be poission or nbinom2, but both do not work !!!!
  #family = nbinom2()  #poisson() # Use poisson for count data (number of apps)
)
summary(m_apps_phq)

summary(m_duration_phq)
summary(m_apps_phq)

tab_model(mget(ls()[grep("m_duration", ls())]), file = paste0("tables/LS_models_duration_", Sys.Date(), ".html"))
tab_model(mget(ls()[grep("m_apps", ls())]), file = paste0("tables/LS_models_apps_", Sys.Date(), ".html"))