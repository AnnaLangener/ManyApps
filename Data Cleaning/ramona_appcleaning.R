### For Ramona ###

library(data.table)
library(stringr)
library(dplyr)

#################################################################
########## Data Cleaning as described in Preregistration ########
#################################################################

# Dataset: manyapps_all_apps_raw

# includes the following columns

# "participant_number": original participant ID as recorded in the dataset [chr]
# "Package_name": app package identifier [chr]
# "timestamp": Raw timestamp of start of app usage event [chr]
# "Dataset": Name for dataset (e.g., WHALE) [chr]
# "duration": duration of app usage (in seconds(!!!)) [num]
# "unique_participant_number": Dataset + participant_number [paste0(substr(Dataset, 1, 3), "_", participant_number)] (to make sure its unique across datasets) [chr]

# (next variables are baseline variables that are just repeated, if not measured they are coded as "NA")
# "age": age [int]
# "gender": options are: female, male, other [chr]
# "PHQ": mean PHQ-8 [num]
# "country": country of data collection (e.g., "Germany") [chr]
# "PANAS_POS": Mean of positive affect items (scale 1-5) [num]
# "PANAS_NEG": Mean of negative affect items (scale 1-5) [num]
# "STRESS": Mean Perceived Stress Scale [num]
# "SWLS": Mean SWLS Scale [num]
# "date_only": Cleaned date from timestamp: "YYYY-MM-DD" (e.g., 2023-06-13) [chr]
# "time_only": Cleaned hour from timestamp: "HH:MM:SS"  (e.g., 19:09:23) [chr]
# "datetime_clean": Cleaned date from timestamp"  "YYYY-MM-DD HH:MM:SS" or ""YYYY-MM-DD" for corona datasets(e.g., 2023-06-13 19:09:23) [chr]



manyapps_all_apps_raw <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/whale_apps_with_dem.csv")
manyapps_all_apps_raw$timestamp <- as.character(manyapps_all_apps_raw$timestamp)

parts <- strsplit(manyapps_all_apps_raw$timestamp, "[ T]")

manyapps_all_apps_raw$date_only <- sapply(parts, function(x) x[1])

manyapps_all_apps_raw$time_only <- sapply(parts, function(x) {
  if (length(x) >= 2) x[2] else NA_character_
})

manyapps_all_apps_raw$datetime_clean <- ifelse(
  is.na(manyapps_all_apps_raw$time_only) | manyapps_all_apps_raw$time_only == "",
  manyapps_all_apps_raw$date_only,
  paste(manyapps_all_apps_raw$date_only, manyapps_all_apps_raw$time_only)
)

manyapps_all_apps_raw$date_only <- as.character(manyapps_all_apps_raw$date_only)
manyapps_all_apps_raw$time_only <- as.character(manyapps_all_apps_raw$time_only)
manyapps_all_apps_raw$datetime_clean <- as.character(manyapps_all_apps_raw$datetime_clean)

#########

# add age group column: age groups will be categorized into 5-year bins following Diaz et al. (2021). Specifically, ages 18–19 will form one group, followed by 20–24 and 25–29
unique(manyapps_all_apps_raw$age)

unique(manyapps_all_apps_raw$unique_participant_number[manyapps_all_apps_raw$age == 17]) # remove two participants as they are under 18

# find the participants under 18
under18_ids <- unique(manyapps_all_apps_raw$unique_participant_number[
  manyapps_all_apps_raw$age == 17
])

# remove them from the dataset
manyapps_all_apps_raw <- manyapps_all_apps_raw[
  !(manyapps_all_apps_raw$unique_participant_number %in% under18_ids), 
]

manyapps_all_apps_raw$age_group <- cut(
  manyapps_all_apps_raw$age,
  breaks = c(18, 20, seq(25, 85, by = 5), Inf),
  right = FALSE,
  labels = c("18-19", "20-24", "25-29", "30-34", "35-39",
             "40-44", "45-49", "50-54", "55-59", "60-64",
             "65-69", "70-74", "75-79", "80-84", "85+")
)


# Keep only valid timestamps and non-negative duration
manyapps_all_apps_raw <- manyapps_all_apps_raw %>%
  filter(!is.na(timestamp), !is.na(participant_number), !is.na(Package_name)) %>%
  mutate(
    duration = suppressWarnings(as.numeric(duration)),
    duration = ifelse(is.na(duration) | duration < 0, NA_real_, duration),
    day = as.Date(date_only)
  )

# Use only first 14 days per participant within each dataset (to avoid overrepresentation of one specific dataset)
participant_windows <- manyapps_all_apps_raw %>%
  group_by(Dataset, unique_participant_number) %>%
  summarise(first_day = min(day, na.rm = TRUE), .groups = "drop") %>%
  mutate(last_day_14 = first_day + 13)

manyapps_14d <- manyapps_all_apps_raw %>%
  inner_join(participant_windows, by = c("Dataset", "unique_participant_number")) %>%
  filter(day >= first_day, day <= last_day_14)

# Label days with no app usage as missing (to exclude participants with less than 50% observed days or less than 7 days)
participant_day_grid <- participant_windows %>%
  rowwise() %>%
  mutate(day = list(seq.Date(first_day, last_day_14, by = "day"))) %>%
  ungroup() %>%
  tidyr::unnest(day)

observed_days <- manyapps_14d %>%
  distinct(Dataset, unique_participant_number, day) %>%
  mutate(has_data = TRUE)

manyapps_day_coverage <- participant_day_grid %>%
  left_join(observed_days, by = c("Dataset", "unique_participant_number", "day")) %>%
  mutate(
    has_data = ifelse(is.na(has_data), FALSE, has_data),
    day_status = ifelse(has_data, "observed", "missing")
  )

# Exclude participants with <50% observed days OR <7 days with data
participant_quality <- manyapps_day_coverage %>%
  group_by(Dataset, unique_participant_number) %>%
  summarise(
    n_days_study_period = n(),
    n_days_with_data = sum(has_data),
    prop_days_with_data = n_days_with_data / n_days_study_period,
    include_participant = prop_days_with_data >= 0.5 & n_days_with_data >= 7,
    .groups = "drop"
  )

sum(participant_quality$prop_days_with_data <= 0.5) 

# # Count participants with prop_days_with_data < 0.5 by dataset
# participant_low_data <- participant_quality %>%
#   group_by(Dataset) %>%
#   summarise(n_low_data = sum(prop_days_with_data < 0.5, na.rm = TRUE)) %>%
#   arrange(desc(n_low_data))
# 
# participant_low_data

sum(participant_quality$include_participant == TRUE)


included_participants <- participant_quality %>%
  filter(include_participant) %>%
  select(Dataset,  unique_participant_number)

manyapps_all_apps <- manyapps_14d %>%
  inner_join(included_participants, by = c("Dataset",  "unique_participant_number"))


length(unique(manyapps_all_apps$unique_participant_number))
length(unique(manyapps_all_apps$Dataset))


#Filter out system apps based on Ramonas Category System and Google

system_schoedel <- read.csv("/Users/f007qrc/projects/ManyApps_Data/systemapps_schoedel.csv")
system_schoedel <- system_schoedel$App_name[system_schoedel$Final_Rating == "System"]
system <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Apps_categorization_final.csv")
system <- system$app_name[system$googleCats == "system"]

system_all <- unique(c(system,system_schoedel))

sum(manyapps_all_apps$Package_name %in% system_all)/nrow(manyapps_all_apps)

manyapps_all_apps <- manyapps_all_apps[!manyapps_all_apps$Package_name %in% system_all, ]


# Filter out duplicated values and duration that equals zero
manyapps_all_apps <- manyapps_all_apps[!duplicated(manyapps_all_apps[,1:5]),] # based on participant_number, Package_name, timestamp, Dataset, duration
manyapps_all_apps <- manyapps_all_apps[manyapps_all_apps$duration > 0,]


write.csv(manyapps_all_apps,"/Users/f007qrc/projects/ManyApps_Data/complete_data_cleaning_stage1.csv")

manyapps_all_apps = read.csv("/Users/f007qrc/projects/ManyApps_Data/complete_data_cleaning_stage1.csv")

# test <-  c("Corona_Parent", "Spain 1", "MoodyLife")
# 
# manyapps_all_apps <- manyapps_all_apps[manyapps_all_apps$Dataset %in% test,]

# 
# excluded <- c("Corona_Parent", "Corona_Health", "Corona_Stress")
# 
# quantile(
#   manyapps_all_apps$duration[!manyapps_all_apps$Dataset %in% excluded],
#   0.9999,
#   na.rm = TRUE
# )

rm(manyapps_14d,manyapps_all_apps_raw,manyapps_day_coverage,observed_days,participant_day_grid,participant_windows, unique_participants_dataset )

# ---------------------------
# Full Overview (Including Package Name)
# ---------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(lubridate)

#### The next code will calculate duration per hour (will account for usage duration that spans over several hours)

split_session_fast <- function(start, end) {
  if (is.na(start) || is.na(end) || end <= start) {
    return(data.table(
      hour_start = as.POSIXct(character(0)),
      hour_end = as.POSIXct(character(0)),
      duration_in_hour_sec = numeric(0)
    ))
  }
  
  hs <- floor_date(start, "hour")
  he <- ceiling_date(end, "hour")
  hour_seq <- seq(hs, he, by = "hour")
  if (length(hour_seq) < 2) hour_seq <- c(hs, hs + hours(1))
  
  hour_start <- hour_seq[-length(hour_seq)]
  hour_end   <- hour_seq[-1]
  
  s <- as.numeric(start)
  e <- as.numeric(end)
  hs_num <- as.numeric(hour_start)
  he_num <- as.numeric(hour_end)
  
  duration <- pmax(0, pmin(e, he_num) - pmax(s, hs_num))
  keep <- duration > 0
  
  data.table(
    hour_start = hour_start[keep],
    hour_end = hour_end[keep],
    duration_in_hour_sec = duration[keep]
  )
}

DT <- as.data.table(manyapps_all_apps)

# Ensure datetime_clean is parseable even if it is date-only
DT[, datetime_clean := as.character(datetime_clean)]
DT[, datetime_clean := ifelse(
  grepl("^\\d{4}-\\d{2}-\\d{2}$", datetime_clean),
  paste(datetime_clean, "00:00:00"),
  datetime_clean
)]
DT[, datetime_clean := as.POSIXct(datetime_clean, tz = "UTC")]

DT[, duration := suppressWarnings(as.numeric(duration))]
DT[, end_time := datetime_clean + duration]
DT[, is_daily_only := is.na(time_only)]

DT <- DT[!is.na(datetime_clean) & !is.na(duration) & duration > 0]
DT[, row_id := .I]

expanded <- DT[, {
  if (is_daily_only) {
    data.table(
      hour_start = as.POSIXct(NA),
      hour_end = as.POSIXct(NA),
      duration_in_hour_sec = NA_real_
    )
  } else {
    split_session_fast(datetime_clean, end_time)
  }
}, by = row_id]

manyapps_all_apps_split <- DT[expanded, on = "row_id", allow.cartesian = TRUE][
  , `:=`(
    date_only = as.Date(hour_start),
    hourly_time = ifelse(is.na(hour_start), NA_character_, format(hour_start, "%H:00:00")),
    day = as.Date(datetime_clean)
  )
][, row_id := NULL]


rm(DT, expanded)

# ---------------------------
# Daily per participant per app
# ---------------------------

participant_app_daily <- manyapps_all_apps_split %>%
  group_by(Dataset, unique_participant_number, Package_name, day) %>%
  summarise(
    total_daily_app_usage = pmin(
      if (first(is_daily_only)) {
        sum(duration, na.rm = TRUE)  
      } else {
        sum(duration_in_hour_sec, na.rm = TRUE)
      },
      86400  # cap at 24 hours in seconds
    ),
    .groups = "drop"
  )

# ---------------------------
# Participant-level mean across days per app
# ---------------------------

study_app_daily <- participant_app_daily %>%
  group_by(Dataset, unique_participant_number, Package_name) %>%
  summarise(
    study_average_daily_app_usage = mean(total_daily_app_usage, na.rm = TRUE),
    .groups = "drop"
  )

# ---------------------------
# Join back with correct keys
# ---------------------------

manyapps_all_apps_split <- manyapps_all_apps_split %>%
  left_join(participant_app_daily, by = c("Dataset", "unique_participant_number", "Package_name", "day")) %>%
  left_join(study_app_daily, by = c("Dataset", "unique_participant_number", "Package_name"))

# ---------------------------
# Calculate hourly usage, NA for daily-only participants
# ---------------------------
manyapps_app_hourly_final <- manyapps_all_apps_split %>%
  group_by(Dataset, unique_participant_number, Package_name,day, hour_start) %>%
  summarise(
    total_hourly_app_usage = if (all(is.na(hour_start))) NA_real_ else sum(duration_in_hour_sec, na.rm = TRUE), # in case app was opened multiple times in the hour
    total_daily_app_usage = first(total_daily_app_usage),
    study_average_daily_app_usage = first(study_average_daily_app_usage),
    across(
      .cols = everything(),
      .fns = ~ first(.x),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  select(
    -any_of(c(
      "timestamp", "time_only", "datetime_clean","duration","date_only","is_daily_only",
      "first_day", "last_day_14", "end_time", "hour_end","duration_in_hour_sec","is_daily_only.x","is_daily_only.y","X","X.1"
    ))
  )

#rm(manyapps_all_apps_split,participant_app_daily)

write.csv(manyapps_app_hourly_final,"/Users/f007qrc/projects/ManyApps_Data/complete_data_split_per_app.csv") ## Includes all datasets besides Ramonas (02/04)

# ---------------------------
# Create dataframe for RQ4
# ---------------------------
# a) age group, b) gender, c) sample
# For each age group, gender, and sample, the total minutes of use per app will be summed across all valid days and then divided by the total amount of time spent on all apps per person. 

top_apps_by_agegroup <- manyapps_app_hourly_final %>%
  mutate(app_minutes = total_daily_app_usage) %>% # daily because of corona studies
  group_by(unique_participant_number,Package_name, Dataset, age_group, gender) %>% 
  summarise(app_minutes = sum(app_minutes, na.rm = TRUE), .groups = "drop") %>% # Calculate how much time a person spent on each app
  group_by(unique_participant_number) %>%
  mutate(total_minutes_all_apps = sum(app_minutes, na.rm = TRUE)) %>% # Calculate how much time a person spent on all apps
  ungroup() %>%
  mutate(app_share_person = ifelse(total_minutes_all_apps > 0, app_minutes / total_minutes_all_apps, NA_real_)) %>% # Calculate proportion of time spent on an app 
  group_by(age_group, Package_name) %>%
  summarise(
    share = sum(app_share_person, na.rm = TRUE),# sum share across all participants
    n_people = n_distinct(unique_participant_number),
    .groups = "drop"
  ) %>%
  group_by(age_group) %>%
  arrange(desc(share), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10) %>%
  ungroup()

top_apps_by_agegroup$mean_share = top_apps_by_agegroup$share/top_apps_by_agegroup$n_people

write.csv(top_apps_by_agegroup,"/Users/f007qrc/projects/ManyApps_Data/top_apps_by_agegroup.csv")


top_apps_by_gender <- manyapps_app_hourly_final %>%
  mutate(app_minutes = total_daily_app_usage) %>% # daily because of corona studies
  group_by(unique_participant_number, Package_name, Dataset, age_group, gender) %>%
  summarise(app_minutes = sum(app_minutes, na.rm = TRUE), .groups = "drop") %>%
  group_by(Dataset, unique_participant_number) %>%
  mutate(total_minutes_all_apps = sum(app_minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(app_share_person = ifelse(total_minutes_all_apps > 0, app_minutes / total_minutes_all_apps, NA_real_)) %>%
  group_by(gender,Package_name) %>%
  summarise(
    share = sum(app_share_person, na.rm = TRUE),
    n_people = n_distinct(unique_participant_number),
    .groups = "drop"
  ) %>%
  group_by(gender) %>%
  arrange(desc(share), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10) %>%
  ungroup()

top_apps_by_gender$mean_share = top_apps_by_gender$share/top_apps_by_gender$n_people
write.csv(top_apps_by_gender,"/Users/f007qrc/projects/ManyApps_Data/top_apps_by_gender.csv")


top_apps_by_sample <- manyapps_app_hourly_final %>%
  mutate(app_minutes = total_daily_app_usage) %>% # daily because of corona studies
  group_by(unique_participant_number, Package_name, Dataset, age_group, gender) %>%
  summarise(app_minutes = sum(app_minutes, na.rm = TRUE), .groups = "drop") %>%
  group_by(Dataset, unique_participant_number) %>%
  mutate(total_minutes_all_apps = sum(app_minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(app_share_person = ifelse(total_minutes_all_apps > 0, app_minutes / total_minutes_all_apps, NA_real_)) %>%
  group_by(Dataset,Package_name) %>%
  summarise(
    share = sum(app_share_person, na.rm = TRUE),
    n_people = n_distinct(unique_participant_number),
    .groups = "drop"
  ) %>%
  group_by(Dataset) %>%
  arrange(desc(share), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10) %>%
  ungroup()

top_apps_by_sample$mean_share = top_apps_by_sample$share/top_apps_by_sample$n_people # Ramona
write.csv(top_apps_by_sample,"/Users/f007qrc/projects/ManyApps_Data/top_apps_by_sample.csv")

rm(top_apps_by_agegroup,top_apps_by_gender,top_apps_by_sample)


top_apps_by_country <- manyapps_app_hourly_final %>%
  mutate(app_minutes = total_daily_app_usage) %>% # daily because of corona studies
  group_by(unique_participant_number, Package_name, Dataset, age_group, gender,country) %>%
  summarise(app_minutes = sum(app_minutes, na.rm = TRUE), .groups = "drop") %>%
  group_by(Dataset, unique_participant_number) %>%
  mutate(total_minutes_all_apps = sum(app_minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(app_share_person = ifelse(total_minutes_all_apps > 0, app_minutes / total_minutes_all_apps, NA_real_)) %>%
  group_by(country,Package_name) %>%
  summarise(
    share = sum(app_share_person, na.rm = TRUE),
    n_people = n_distinct(unique_participant_number),
    .groups = "drop"
  ) %>%
  group_by(country) %>%
  arrange(desc(share), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 10) %>%
  ungroup()


### CHECKED UNTIL HERE, CONTINUE HERE

# ---------------------------
# No-app hourly table (keep demographics/other columns)
# ---------------------------

manyapps_app_hourly_final <- read.csv("/Users/f007qrc/projects/ManyApps_Data/complete_data_split_per_app.csv")[-1]

# test <-  c("Corona_Parent", "Spain 1", "MoodyLife")
# 
# manyapps_app_hourly_final <- manyapps_app_hourly_final[manyapps_app_hourly_final$Dataset %in% test,]
# "total_hourly_app_usage"  sum per hour in seconds
# "total_daily_app_usage" sum per day in seconds
# "study_average_daily_app_usage" mean over observed days, does not include not observed days, so might not be accurate


join_keys_hour <- c("Dataset", "unique_participant_number", "day", "hourly_time")
join_keys_day <- c("Dataset", "unique_participant_number", "day")
join_keys_person <- c("Dataset", "unique_participant_number")

# -------------------------------------------------
# Context columns (unchanged)
# -------------------------------------------------

context_cols <- setdiff(
  names(manyapps_app_hourly_final),
  c("Package_name",
    "total_hourly_app_usage",
    "total_daily_app_usage",
    "study_average_daily_app_usage")
)

hourly_context <- manyapps_app_hourly_final %>% 
  group_by(across(all_of(join_keys_hour))) %>%
  summarise(
    across(all_of(setdiff(context_cols, join_keys_hour)), ~ dplyr::first(.x)),
    .groups = "drop"
  ) # to merge static variables back

# -------------------------------------------------
# HOURLY USAGE (NA-safe)
# -------------------------------------------------

manyapps_hourly_noapp <- manyapps_app_hourly_final %>%
  group_by(across(all_of(join_keys_hour))) %>%
  summarise(
    hourly_usage_raw =
      if (all(is.na(total_hourly_app_usage))) NA_real_
    else sum(total_hourly_app_usage, na.rm = TRUE),
    
    total_hourly_app_usage =
      if (is.na(hourly_usage_raw)) NA_real_
    else pmin(hourly_usage_raw, 60 * 60),  # cap 60 min/hour
    
    .groups = "drop"
  ) %>%
  left_join(hourly_context, by = join_keys_hour)

# -------------------------------------------------
# PARTICIPANT DAILY TOTAL (not app-specific)
# IMPORTANT: sum across apps for daily-only datasets
# -------------------------------------------------

daily_noapp <- manyapps_app_hourly_final %>%
  group_by(across(all_of(join_keys_day))) %>%
  summarise(
    total_daily_app_usage =
      if (all(is.na(total_hourly_app_usage))) {
        pmin(sum(total_daily_app_usage, na.rm = TRUE), 60 * 60 * 24)
      } else {
        pmin(sum(total_hourly_app_usage, na.rm = TRUE), 60 * 60 * 24)
      },
    .groups = "drop"
  )

# -------------------------------------------------
# STUDY DAILY TOTAL
# -------------------------------------------------

study_daily_noapp <- daily_noapp %>%
  group_by(Dataset, unique_participant_number) %>%
  summarise(
    study_average_daily_app_usage = mean(total_daily_app_usage, na.rm = TRUE),
    .groups = "drop"
  ) # We calculate mean over days. So probably good to not fill missing days and treat them as missing

# -------------------------------------------------
# UNIQUE APP COUNTS
# Only count where hourly data actually exists
# -------------------------------------------------

uapps_hour <- manyapps_app_hourly_final %>%
  filter(!is.na(total_hourly_app_usage) & total_hourly_app_usage > 0) %>%
  group_by(across(all_of(join_keys_hour))) %>%
  summarise(unique_apps_hour = n_distinct(Package_name), .groups = "drop")

uapps_day <- manyapps_app_hourly_final %>%
  group_by(across(all_of(join_keys_day))) %>%
  summarise(unique_apps_day = n_distinct(Package_name), .groups = "drop")

uapps_overall <- manyapps_app_hourly_final %>%
  group_by(across(all_of(join_keys_person))) %>%
  summarise(unique_apps_overall = n_distinct(Package_name), .groups = "drop")

# -------------------------------------------------
# FINAL MERGE
# -------------------------------------------------

manyapps_hourly_noapp <- manyapps_hourly_noapp %>%
  left_join(daily_noapp, by = join_keys_day) %>%
  left_join(study_daily_noapp, by = c("Dataset", "unique_participant_number")) %>%
  left_join(uapps_hour, by = join_keys_hour) %>%
  left_join(uapps_day, by = join_keys_day) %>%
  left_join(uapps_overall, by = join_keys_person) 



write.csv(manyapps_hourly_noapp,"/Users/f007qrc/projects/ManyApps_Data/Final_noapp_overview.csv")

