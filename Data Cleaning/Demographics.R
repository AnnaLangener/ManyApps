##################################
### Data Cleaning Demographics ###
##################################

library(dplyr)
library(dplyr)
library(readxl)

############# Mariek ERC ############
# Gender: M = Man, N = Non binary, O = Other, V = Vrouw
erc <- read.csv("/Users/f007qrc/projects/ManyApps_Data/ERC/manyApps_rawdata_disconnect.csv", stringsAsFactors = FALSE)
length(unique(erc$participant_number))

phq_cols <- paste0("PHQ_", 1:8)
erc$PHQ <- rowMeans(erc[, phq_cols], na.rm = TRUE)

erc_dem <- erc %>%
  transmute(
    PHQ = PHQ,
    participant_number = as.character(participant_number),
    age = suppressWarnings(as.numeric(age)),
    gender_raw = toupper(trimws(as.character(gender))),
    Dataset = as.character(Dataset)
  ) %>%
  distinct(participant_number, Dataset, .keep_all = TRUE) %>%
  mutate(
    gender = case_when(
      gender_raw == "M" ~ "male",
      gender_raw == "V" ~ "female",
      TRUE ~ "other"
    ),
    unique_participant_number = paste0(substr(Dataset, 1, 3), "_", participant_number)
  ) %>%
  select(participant_number, age, gender, PHQ, Dataset, unique_participant_number)


erc_apps <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/erc_apps.csv", stringsAsFactors = FALSE)[-1]
length(unique(erc_apps$unique_participant_number))

erc_apps_with_dem <- erc_apps %>%
  mutate(unique_participant_number = as.character(unique_participant_number)) %>%
  inner_join(
    erc_dem %>% select(unique_participant_number, PHQ, age, gender),
    by = "unique_participant_number"
  )

erc_apps_with_dem$country <- NA
erc_apps_with_dem$PANAS_NEG <- NA
erc_apps_with_dem$PANAS_POS <- NA
erc_apps_with_dem$STRESS <- NA
erc_apps_with_dem$SWLS <- NA


# optional save
write.csv(
  erc_apps_with_dem,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/erc_apps_with_dem.csv",
  row.names = FALSE
)


length(unique(erc_apps_with_dem$unique_participant_number))
length(unique(erc_apps_with_dem$unique_participant_number[!is.na(erc_apps_with_dem$PHQ)]))
length(unique(erc$participant_number[!is.na(erc$PHQ_1)]))

colnames(erc_apps_with_dem)

############# Study Smart ############
# Gender: 1: male, 2: female, 3: divers

library(readxl)

study_smart <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/study_smart.csv", stringsAsFactors = FALSE)[-1]
length(unique(study_smart$participant_number))


study_smart <- study_smart %>%
  mutate(
    participant_number = as.character(participant_number),
    unique_participant_number = as.character(unique_participant_number)
  )

# demographics raw
study_smart_dem_raw <- read_excel(
  "/Users/f007qrc/projects/ManyApps_Data/Study_Smart/Studysmart_WBMeasures_withtime_new.xlsx"
)

study_smart_dem_raw = study_smart_dem_raw[study_smart_dem_raw$ExpCond == 0,]

length(unique(study_smart_dem_raw$ID))


if (names(study_smart_dem_raw)[1] == "") {
  study_smart_dem_raw <- study_smart_dem_raw[, -1, drop = FALSE]
}

# id bridge
id_map <- read_excel("/Users/f007qrc/projects/ManyApps_Data/Study_Smart/participant_stats__Theda Studies_anonym.xlsx") %>%
  transmute(
    APP_CODE = as.character(APP_CODE),
    participant_number = as.character(`User ID`)
  ) %>%
  filter(!is.na(APP_CODE), nzchar(APP_CODE), !is.na(participant_number), nzchar(participant_number)) %>%
  distinct(APP_CODE, .keep_all = TRUE)

# build demographics in app-ID space (User ID)
study_smart_dem <- study_smart_dem_raw %>%
  transmute(
    APP_CODE = as.character(APP_CODE),
    age = suppressWarnings(as.numeric(Age)),
    gender_raw = suppressWarnings(as.numeric(Gender)),
    PANAS_NEG = NEGAF_T1,
    PANAS_POS = POSAF_T1,
    STRESS = PSS_T1,
  ) %>%
  inner_join(id_map, by = "APP_CODE") %>%
  mutate(
    gender = case_when(
      gender_raw == 1 ~ "man",
      gender_raw == 2 ~ "woman",
      TRUE ~ "other"
    ),
    unique_participant_number = paste0("Stu_", participant_number)
  ) %>%
  select(participant_number, age, gender,PANAS_NEG,PANAS_POS, STRESS, unique_participant_number) %>%
  distinct(participant_number, .keep_all = TRUE)

# keep only participants present in both
study_smart_with_dem <- study_smart %>%
  inner_join(
    study_smart_dem %>% select(participant_number,PANAS_NEG,PANAS_POS, STRESS, age, gender),
    by = "participant_number"
  )
study_smart_with_dem$country <- "Germany"
study_smart_with_dem$SWLS <- NA
study_smart_with_dem$PHQ <- NA

study_smart_with_dem

# optional save
write.csv(
  study_smart_with_dem,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/study_smart_with_dem.csv",
  row.names = FALSE
)

colnames(study_smart_with_dem)

length(unique(study_smart_with_dem$participant_number))
length(unique(study_smart_with_dem$participant_number[!is.na(study_smart_with_dem$STRESS)]))

colnames(study_smart_with_dem)

############# Aurelio #############
# Spain 1/2: Female, Male
aur1_env <- new.env()
aur2_env <- new.env()
load("/Users/f007qrc/projects/ManyApps_Data/Aurelio/New/Demographics_Spain1.RData", envir = aur1_env)
load("/Users/f007qrc/projects/ManyApps_Data/Aurelio/New/Demographics_Spain2.RData", envir = aur2_env)

#  swl1 swl2 swl3 swl4 swl5 (SWLS)
# PA: awb1, awb3, awb5
# NA: awb2, awb4, awb6, awb 7
aurelio_dem_1 <- aur1_env$base_SP1 

cols_to_numeric <- c(paste0("awb", 1:7), paste0("swl", 1:5))
aurelio_dem_1[cols_to_numeric] <- lapply(aurelio_dem_1[cols_to_numeric], as.numeric)
aurelio_dem_1$PANAS_POS <- rowMeans(aurelio_dem_1[, c("awb1","awb3","awb5")], na.rm = TRUE)
aurelio_dem_1$PANAS_NEG <- rowMeans(aurelio_dem_1[, c("awb2","awb4","awb6","awb7")], na.rm = TRUE)
aurelio_dem_1$SWLS <- rowMeans(aurelio_dem_1[, c("swl1","swl2","swl3","swl4","swl5")], na.rm = TRUE)


aurelio_dem_2 <- aur2_env$base_SP2 

cols_to_numeric <- c(paste0("awb", 1:7), paste0("swl", 1:5))
aurelio_dem_2[cols_to_numeric] <- lapply(aurelio_dem_2[cols_to_numeric], as.numeric)
aurelio_dem_2$PANAS_POS <- rowMeans(aurelio_dem_2[, c("awb1","awb3","awb5")], na.rm = TRUE)
aurelio_dem_2$PANAS_NEG <- rowMeans(aurelio_dem_2[, c("awb2","awb4","awb6","awb7")], na.rm = TRUE)
aurelio_dem_2$SWLS <- rowMeans(aurelio_dem_2[, c("swl1","swl2","swl3","swl4","swl5")], na.rm = TRUE)


aurelio_dem_1 <- aurelio_dem_1 %>%
  mutate(
    participant_number = as.character(participant_number),
    age = suppressWarnings(as.numeric(as.character(age))),
    gender_raw = tolower(trimws(as.character(gender))),
    Dataset = as.character(Dataset)
  ) %>%
  distinct(participant_number, Dataset, .keep_all = TRUE) %>%
  mutate(
    gender = case_when(
      gender_raw %in% c("Male") ~ "male",
      gender_raw %in% c("Female") ~ "female",
      TRUE ~ "other"
    ),
    unique_participant_number = paste0(substr(Dataset, 1, 3), "_", participant_number)
  ) %>%
  select(participant_number, age, gender,PANAS_POS,PANAS_NEG, SWLS, Dataset, unique_participant_number)



aurelio_dem_2 <- aurelio_dem_2 %>%
  mutate(
    participant_number = as.character(participant_number),
    age = suppressWarnings(as.numeric(as.character(age))),
    gender_raw = tolower(trimws(as.character(gender))),
    Dataset = as.character(Dataset)
  ) %>%
  distinct(participant_number, Dataset, .keep_all = TRUE) %>%
  mutate(
    gender = case_when(
      gender_raw %in% c("Male") ~ "male",
      gender_raw %in% c("Female") ~ "female",
      TRUE ~ "other"
    ),
    unique_participant_number = paste0(substr(Dataset, 1, 3), "_", participant_number)
  ) %>%
  select(participant_number, age, gender,PANAS_POS,PANAS_NEG, SWLS, Dataset, unique_participant_number)


aurelio_dem <- bind_rows(aurelio_dem_1, aurelio_dem_2)

aurelio_apps <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/aurelio_apps.csv", stringsAsFactors = FALSE)[-1]

length(unique(aurelio_apps$participant_number[aurelio_apps$Dataset == "Spain 1"]))
length(unique(aurelio_apps$participant_number[aurelio_apps$Dataset == "Spain 2"]))


aurelio_dem <- aurelio_apps %>%
  mutate(unique_participant_number = as.character(unique_participant_number)) %>%
  inner_join(
    aurelio_dem %>% select(unique_participant_number, age, gender, PANAS_POS,PANAS_NEG, SWLS,),
    by = "unique_participant_number"
  )

aurelio_dem$country <- "Spain"
aurelio_dem$STRESS <- NA
aurelio_dem$PHQ <- NA

length(unique(aurelio_dem$participant_number[aurelio_dem$Dataset == "Spain 1" & !is.na(aurelio_dem$SWLS)]))


# optional save
write.csv(
  aurelio_dem,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/aurelio_apps_with_dem.csv")

colnames(aurelio_dem)

############# Corona Health ############# 
# Gender: 0 = F, 1 = M, 2 = transgender
library(dplyr)

# --- Load CSVs ---
stress_b <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_stress_baseline_090226.csv",
                     sep = ",", stringsAsFactors = FALSE)
stress_f <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_stress_followUp_090226.csv",
                     sep = ",", stringsAsFactors = FALSE)

heart_b <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_heart_baseline_090226.csv",
                    sep = ";", stringsAsFactors = FALSE)

parent_b <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_parent_baseline_090226.csv",
                     sep = ";", stringsAsFactors = FALSE)

parent_b <- parent_b[!is.na(parent_b$alter),]

parent_f <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_parent_followUp_090226.csv",
                     sep = ";", stringsAsFactors = FALSE)


parent_apps_daily <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/parent_apps_daily.csv",
                              stringsAsFactors = FALSE)[-1]

stress_apps_daily <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/stress_apps_daily.csv",
                              stringsAsFactors = FALSE)[-1]

heart_apps_daily <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_apps_daily.csv",
                             stringsAsFactors = FALSE)[-1]

# --- Participant counts ---
length(unique(parent_b$user_id))
length(unique(c(parent_b$user_id, parent_f$user_id)))
length(unique(parent_apps_daily$unique_participant_number))

length(unique(stress_b$user_id))
length(unique(c(stress_b$user_id, stress_f$user_id)))
length(unique(stress_apps_daily$unique_participant_number))

length(unique(heart_b$user_id))
length(unique(heart_apps_daily$unique_participant_number))


pss_cols <- paste0("pss", 1:10)

# Compute row-wise mean stress score
stress_b$STRESS <- rowMeans(stress_b[pss_cols], na.rm = TRUE)

# --- Clean and rename stress baseline data ---
stress_b <- stress_b %>%
  select(user_id, alter, geschlecht, country, STRESS) %>%
  rename(
    participant_number = user_id,
    age = alter,
    gender = geschlecht
  ) %>%
  mutate(
    gender = case_when(
      gender == 0 ~ "female",
      gender == 1 ~ "male",
      gender == 2 ~ "other",
      TRUE ~ NA_character_
    )
  )

# --- Merge stress baseline demographics with app data ---
stress_dem_apps <- stress_apps_daily %>%
  inner_join(
    stress_b %>% select(participant_number, age, gender,country, STRESS),
    by = "participant_number"
  )
length(unique(stress_dem_apps$participant_number))

length(unique(stress_dem_apps$participant_number[!is.na(stress_dem_apps$STRESS)]))

stress_dem_apps$PANAS_POS <- NA
stress_dem_apps$PANAS_NEG <- NA
stress_dem_apps$PHQ <- NA
stress_dem_apps$SWLS <- NA

write.csv(
  stress_dem_apps,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/stress_dem_apps.csv")

colnames(stress_dem_apps)

# # --- Clean and rename stress baseline data ---
# heart_b <- heart_b %>%
#   select(user_id, alter, geschlecht, country) %>%
#   rename(
#     participant_number = user_id,
#     age = age,
#     gender = geschlecht
#   ) %>%
#   mutate(
#     gender = case_when(
#       gender == 0 ~ "female",
#       gender == 1 ~ "male",
#       gender == 2 ~ "other",
#       TRUE ~ NA_character_
#     )
#   )
# 
# # --- Merge stress baseline demographics with app data ---
# heart_dem_apps <- heart_apps_daily %>%
#   inner_join(
#     heart_b %>% select(participant_number, age, gender),
#     by = "participant_number"
#   )
# length(unique(heart_dem_apps$participant_number))

# Define PHQ columns
phq_cols <- c("phq9_a", "phq9_b", "phq9_c", "phq9_d",
              "phq9_e", "phq9_f", "phq9_g", "phq9_h")

# Clean and add mean PHQ
parent_b <- parent_b %>%
  select(user_id, alter, geschlecht, region, all_of(phq_cols)) %>%
  rename(
    participant_number = user_id,
    age = alter,
    gender = geschlecht,
    country = region
  ) %>%
  mutate(
    gender = case_when(
      gender == 0 ~ "female",
      gender == 1 ~ "male",
      gender == 2 ~ "other",
      TRUE ~ NA_character_
    ),
    PHQ = rowMeans(across(all_of(phq_cols)), na.rm = TRUE)
  )
table(parent_b$region)

parent_b <- parent_b[!duplicated(parent_b$participant_number), ]

# --- Merge stress baseline demographics with app data ---
parent_dem_apps <- parent_apps_daily %>%
  inner_join(
    parent_b %>% select(participant_number, age, gender, country, PHQ),
    by = "participant_number"
  )

length(unique(parent_dem_apps$participant_number))

length(unique(parent_dem_apps$participant_number[!is.na(parent_dem_apps$PHQ)]))

parent_dem_apps$PANAS_POS <- NA
parent_dem_apps$PANAS_NEG <- NA
parent_dem_apps$STRESS <- NA
parent_dem_apps$SWLS <- NA

write.csv(
  parent_dem_apps,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/parent_apps_with_dem.csv")

colnames(parent_dem_apps)

############# Klingelhoefer ############
# 1 männlich, 2 weiblich, 3 nichtbinär und 4 ist custom
kling <- read.csv("/Users/f007qrc/projects/ManyApps_Data/klingelhoefer/data_klingelhoefer_disconnection.csv", stringsAsFactors = FALSE)
length(unique(kling$participant_number))

colnames(kling)

swls_cols <- c("swl_01_pre", "swl_02_pre", "swl_03_pre", "swl_04_pre", "swl_05_pre")

# Compute row-wise mean SWLS score
kling$SWLS <- rowMeans(kling[swls_cols], na.rm = TRUE)

kling_dem <- kling %>%
  transmute(
    participant_number = as.character(participant_number),
    age = suppressWarnings(as.numeric(age)),
    gender_raw = suppressWarnings(as.numeric(gender)),
    Dataset = as.character(Dataset),
    SWLS = SWLS
  ) %>%
  distinct(participant_number, Dataset, .keep_all = TRUE) %>%
  mutate(
    gender = case_when(
      gender_raw == 1 ~ "male",
      gender_raw == 2 ~ "female",
      TRUE ~ "other"
    ),
    unique_participant_number = paste0(substr(Dataset, 1, 3), "_", participant_number)
  ) %>%
  select(participant_number, age, gender,SWLS, Dataset, unique_participant_number)



klingelhoefer_apps <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/klingelhoefer_apps.csv",
                             stringsAsFactors = FALSE)[-1]

length(unique(klingelhoefer_apps$participant_number))

# keep only participants present in both
kling_dem <- klingelhoefer_apps %>%
  inner_join(
    kling_dem %>% select(unique_participant_number, age, gender, SWLS),
    by = "unique_participant_number"
  )

kling_dem$country <- NA
kling_dem$PANAS_POS <- NA
kling_dem$PANAS_NEG <- NA
kling_dem$STRESS <- NA
kling_dem$PHQ <- NA

length(unique(kling_dem$participant_number[!is.na(kling_dem$SWLS)]))


write.csv(
  kling_dem,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/klingelhoefer_with_dem.csv"
)

colnames(kling_dem)


############# Emotion #############
# 1: "female"; 2: "male"; 3: "others".
emotion_dem_raw <- read.csv("/Users/f007qrc/projects/ManyApps_Data/eMotion/ManyApps_eMotion_Demographics.csv", stringsAsFactors = FALSE)
length(unique(emotion_dem_raw$participant_number))

emotion_dem_raw$SWLS

emotion_dem <- emotion_dem_raw %>%
  transmute(
    participant_number = as.character(participant_number),
    age = suppressWarnings(as.numeric(Age)),
    gender_raw = suppressWarnings(as.numeric(Gender)),
    Dataset = as.character(Dataset),
    STRESS = PSS,
    SWLS = SWLS
    
  ) %>%
  distinct(participant_number, Dataset, .keep_all = TRUE) %>%
  mutate(
    gender = case_when(
      gender_raw == 2 ~ "male",
      gender_raw == 1 ~ "female",
      TRUE ~ "other"
    ),
    unique_participant_number = paste0(substr(Dataset, 1, 3), "_", participant_number)
  ) %>%
  select(participant_number, age, gender,STRESS, SWLS, Dataset, unique_participant_number)



emotion_apps <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/emotion_apps.csv",
                               stringsAsFactors = FALSE)[-1]

length(unique(emotion_apps$participant_number))

# keep only participants present in both
emotion_dem <- emotion_apps %>%
  inner_join(
    emotion_dem %>% select(unique_participant_number, age, gender,STRESS, SWLS,),
    by = "unique_participant_number"
  )
emotion_dem$country <- NA
emotion_dem$PANAS_POS <- NA
emotion_dem$PANAS_NEG <- NA
emotion_dem$PHQ <- NA

length(unique(emotion_dem$participant_number[!is.na(emotion_dem$STRESS)]))

write.csv(
  emotion_dem,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/emotion_with_dem.csv"
)

colnames(emotion_dem)

############# PRISM / Behapp #############
prism_dem_raw <- read.csv("/Users/f007qrc/projects/ManyApps_Data/PRISM/PRISM2_demographics.csv", stringsAsFactors = FALSE)
prism_dem <- prism_dem_raw %>%
  transmute(
    participant_number = as.character(SUBJECT_ID),
    age = suppressWarnings(as.numeric(AGE)),
    gender_raw = toupper(trimws(as.character(SEX))),
    Dataset = "PRISM2"
  ) %>%
  distinct(participant_number, Dataset, .keep_all = TRUE) %>%
  mutate(
    gender = case_when(
      gender_raw == "M" ~ "man",
      gender_raw == "F" ~ "woman",
      TRUE ~ "other"
    ),
    unique_participant_number = paste0(substr(Dataset, 1, 3), "_", participant_number)
  ) %>%
  select(participant_number, age, gender, Dataset, unique_participant_number)

############# Chow  #############
# Gender: 1: male, 2: female, 3: other

chow_dem <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Chow/BUCS_traitdata_cleaned_11-21-2024.csv")

remove_ids <- c(4, 20, 70, 94, 214, 253, 283, 494, 153)
chow_dem <- chow_dem[!chow_dem$Record_ID %in% remove_ids, ]

# Check how many unique participants remain
length(unique(chow_dem$Record_ID))

chow_apps <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/chow_apps.csv")

length(unique(chow_apps$participant_number))


chow_dem$PHQ <- chow_dem$PHQ8_TOTAL/8

chow_dem <- chow_dem %>%
  select(Record_ID, age_demo, gender, PANAS_POS, PANAS_NEG,PHQ8_TOTAL, PHQ) %>%
  rename(
    participant_number = Record_ID,
    age = age_demo,
  ) %>%
  mutate(
    gender = case_when(
      gender == 0 ~ "female",
      gender == 1 ~ "male",
      gender == 3 ~ "other",
    )
  )


# keep only participants present in both
chow_dem <- chow_apps %>%
  inner_join(
    chow_dem %>% select(participant_number, age, gender, PANAS_POS, PANAS_NEG, PHQ),
    by = "participant_number"
  )
chow_dem$country <- "United States"
chow_dem$STRESS <- NA
chow_dem$SWLS <- NA

length(unique(chow_dem$participant_number[!is.na(chow_dem$PANAS_NEG)]))


write.csv(
  chow_dem[-1],
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/chow_with_dem.csv"
)
colnames(chow_dem[-1])
############# Yannik (phone study) ############# 
# Gender: Demo_GE1, 1 = m, 2 = w, 3 = d

phonestudy_dem <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Yannik/shared_ManyApps (1)/t0_quest.csv")

# Define SWLS columns
swls_cols <- c("SWLS_1", "SWLS_2", "SWLS_3", "SWLS_4", "SWLS_5")

# Create SWLS mean scale
phonestudy_dem$SWLS <- rowMeans(
  phonestudy_dem[swls_cols],
  na.rm = TRUE
)

colnames(phonestudy_dem)
phonestudy_apps <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/yannik_apps.csv")[-1]

length(unique(phonestudy_dem$user_id))
length(unique(phonestudy_apps$participant_number))

phonestudy_dem <- phonestudy_dem %>%
  select(user_id, Demo_GE1, Demo_A1, SWLS) %>%
  rename(
    participant_number = user_id,
    age = Demo_A1,
    gender = Demo_GE1
  ) %>%
  mutate(
    gender = case_when(
      gender == 0 ~ "female",
      gender == 1 ~ "male",
      gender == 3 ~ "other",
    )
  )


# keep only participants present in both
phonestudy_dem <- phonestudy_apps %>%
  inner_join(
    phonestudy_dem %>% select(participant_number, age, gender, SWLS),
    by = "participant_number"
  )
phonestudy_dem$country <- NA
phonestudy_dem$PANAS_POS <- NA
phonestudy_dem$PANAS_NEG <- NA
phonestudy_dem$STRESS <- NA
phonestudy_dem$PHQ <- NA

length(unique(phonestudy_dem$participant_number[!is.na(phonestudy_dem$SWLS)]))

write.csv(
  phonestudy_dem,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/phonestudy_with_dem.csv"
)

colnames(phonestudy_dem)


####### Aidan

aidan_apps <- read.csv(
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/aidan_apps.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
if (names(aidan_apps)[1] == "") aidan_apps <- aidan_apps[, -1, drop = FALSE]
if ("X" %in% names(aidan_apps)) aidan_apps <- aidan_apps[, setdiff(names(aidan_apps), "X"), drop = FALSE]

length(unique(aidan_apps$participant_number))

aidan_dem <- read.csv(
  "/Users/f007qrc/projects/ManyApps_Data/Aidan/ILIADD_Baseline_Cleaned-3.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

length(unique(aidan_dem$PID))


aidan_dem <- aidan_dem %>%
  transmute(
    participant_number = as.character(PID),
    age = suppressWarnings(as.numeric(age)),
    gender_raw = suppressWarnings(as.numeric(sex)),
    PHQ = suppressWarnings(as.numeric(PHQ_total)) / 8
  ) %>%
  mutate(
    gender = case_when(
      gender_raw == 1 ~ "male",
      gender_raw == 2 ~ "female",
      gender_raw == 3 ~ "other",
      TRUE ~ "other"
    )
  ) %>%
  select(participant_number, age, gender, PHQ)

aidan_dem <- aidan_apps %>%
  mutate(participant_number = as.character(participant_number)) %>%
  inner_join(aidan_dem, by = "participant_number")

aidan_dem$country <- "USA"
aidan_dem$PANAS_POS <- NA
aidan_dem$PANAS_NEG <- NA
aidan_dem$STRESS <- NA
aidan_dem$SWLS <- NA

length(unique(aidan_dem$participant_number))


write.csv(
  aidan_dem,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/aidan_apps_with_dem.csv",
  row.names = FALSE
)
