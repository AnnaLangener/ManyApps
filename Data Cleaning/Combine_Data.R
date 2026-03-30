
##############################################
########## Combine + Hourly Aggregate ########
##############################################
library(data.table)
library(stringr)
library(dplyr)

# -------------------------------
# 1. Set directory and get CSVs
# -------------------------------
cleaned_apps_dir <- "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all"
available_csv <- list.files(cleaned_apps_dir, pattern = "\\.csv$", full.names = TRUE)

# -------------------------------
# 2. Read all CSVs fast
# -------------------------------
apps_list <- lapply(available_csv, fread, stringsAsFactors = FALSE, check.names = TRUE)
names(apps_list) <- sub("\\.csv$", "", basename(available_csv))

# -------------------------------
# 3. Clean timestamps vectorized
# -------------------------------
apps_list <- lapply(apps_list, function(df) {
  
  setDT(df)  # Convert to data.table for fast operations
  
  if ("timestamp" %in% names(df)) {
    # Remove milliseconds
    df[, timestamp := str_remove(as.character(timestamp), "\\..*$")]
    
    # Split into date_only and time_only
    df[, c("date_only", "time_only") := tstrsplit(timestamp, " ", fixed = TRUE)]
    
    # Fill missing times with "00:00:00"
    df[is.na(time_only), time_only := "00:00:00"]
    
    # Create datetime_clean
    df[, datetime_clean := paste(date_only, time_only)]
  }
  
  return(df)
})

# -------------------------------
# 4. Combine all datasets efficiently
# -------------------------------
all_apps_dt <- rbindlist(apps_list, fill = TRUE)  # fill=TRUE handles different column sets

# Convert back to data.frame if needed
all_apps_df <- as.data.frame(all_apps_dt)

# -------------------------------
# 5. Analyze participants
# -------------------------------
manyapps_all_apps_raw <- all_apps_df

# Unique participants by dataset
unique_participants_dataset <- unique(manyapps_all_apps_raw[, c("participant_number", "Dataset")])
table(unique_participants_dataset$Dataset)

# # Unique participants by country
# unique_participants_country <- unique(manyapps_all_apps_raw[, c("unique_participant_number", "country")])
# table(unique_participants_country$country)
# 
# library(treemapify)
# # -------------------------------
# # 1. Summarize participant counts by country
# # -------------------------------
# plot_data <- unique_participants_country %>%
#   group_by(country) %>%
#   summarise(n_participants = n()) %>%
#   ungroup() %>%
#   # Optional: treat numeric codes/NA as "Other"
#   mutate(country = ifelse(is.na(country) | grepl("^[0-9]+$", country), "Other", country))
# 
# # -------------------------------
# # 2. Treemap plot
# # -------------------------------
# ggplot(plot_data, aes(area = n_participants, fill = country, label = country)) +
#   geom_treemap() +
#   geom_treemap_text(color = "white", place = "center", size = 12) +
#   labs(title = "Participants by Country") +
#   theme(legend.position = "none")


manyapps_all_apps_raw <- manyapps_all_apps_raw[, !colnames(manyapps_all_apps_raw) %in% "V1"]
write.csv(manyapps_all_apps_raw,"/Users/f007qrc/projects/ManyApps_Data/complete_data_beforecleaning.csv")

sum(is.na(manyapps_all_apps_raw$datetime_clean))
unique(manyapps_all_apps_raw$Dataset[is.na(manyapps_all_apps_raw$datetime_clean)])




#################################################################
########## Data Cleaning as described in Preregistration ########
#################################################################

# Dataset: manyapps_all_apps_raw

# includes the following columns

# "participant_number": original participant ID as recorded in the dataset [chr]
# "Package_name": app package identifier [chr]
# "timestamp": Raw timestamp of start of app usage event [chr]
# "Dataset": Name for dataset (e.g., WHALE) [chr]
# "duration": duration of app usage (in seconds!) [num]
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
# "datetime_clean": Cleaned date from timestamp"  "YYYY-MM-DD HH:MM:SS"  (e.g., 2023-06-13 19:09:23) [chr]


# Keep only valid timestamps and non-negative duration
manyapps_all_apps_raw <- manyapps_all_apps_raw %>%
  filter(!is.na(timestamp), !is.na(participant_number), !is.na(Package_name)) %>%
  mutate(
    duration = suppressWarnings(as.numeric(duration)),
    duration = ifelse(is.na(duration) | duration < 0, NA_real_, duration),
    day = as.Date(timestamp)
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

sum(participant_quality$prop_days_with_data < 0.5) 

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


write.csv(manyapps_all_apps,"/Users/f007qrc/projects/ManyApps_Data/complete_data_cleaning_stage1.csv")

length(unique(manyapps_all_apps$unique_participant_number))
length(unique(manyapps_all_apps$Dataset))


system_schoedel <- read.csv("/Users/f007qrc/projects/ManyApps_Data/systemapps_schoedel.csv")
system_schoedel <- system_schoedel$App_name[system_schoedel$Final_Rating == "System"]
system <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Apps_categorization_final.csv")
system <- system$app_name[system$googleCats == "system"]

system_all <- unique(c(system,system_schoedel))

sum(manyapps_all_apps$Package_name %in% system_all)/nrow(manyapps_all_apps)

manyapps_all_apps <- manyapps_all_apps[!manyapps_all_apps$Package_name %in% system_all, ]

write.csv(manyapps_all_apps,"/Users/f007qrc/projects/ManyApps_Data/complete_data_cleaning_stage1.csv")


manyapps_all_apps <- manyapps_all_apps[!duplicated(manyapps_all_apps[,1:5]),]


manyapps_all_apps <- manyapps_all_apps[manyapps_all_apps$duration > 0,]

manyapps_all_apps = read.csv("/Users/f007qrc/projects/ManyApps_Data/complete_data_cleaning_stage1.csv")
# 
# # Exclude specific datasets
# excluded <- c("Corona_Parent", "Corona_Health", "Corona_Stress")
# 
# quantile(
#   manyapps_all_apps$duration[!manyapps_all_apps$Dataset %in% excluded],
#   0.9999,
#   na.rm = TRUE
# )

manyapps_all_apps= manyapps_all_apps[manyapps_all_apps$Dataset == "Corona_Parent" | manyapps_all_apps$Dataset == "Spain 1",]

# ---------------------------
# Full Overview (Including Package Name)
# ---------------------------

####




library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(lubridate)

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
DT[, datetime_clean := as.POSIXct(datetime_clean)]
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



# 
# #######
# 
# # robust splitter (seconds)
# split_session <- function(start, end) {
#   if (is.na(start) || is.na(end) || end <= start) {
#     return(tibble(
#       hour_start = as.POSIXct(character(0)),
#       hour_end = as.POSIXct(character(0)),
#       duration_in_hour_sec = numeric(0)
#     ))
#   }
#   
#   hour_seq <- seq(floor_date(start, "hour"), ceiling_date(end, "hour"), by = "hour")
#   if (length(hour_seq) < 2) {
#     hour_seq <- c(floor_date(start, "hour"), floor_date(start, "hour") + hours(1))
#   }
#   
#   tibble(
#     hour_start = head(hour_seq, -1),
#     hour_end   = tail(hour_seq, -1)
#   ) %>%
#     mutate(
#       duration_in_hour_sec = pmax(
#         0,
#         pmin(as.numeric(end), as.numeric(hour_end)) -
#           pmax(as.numeric(start), as.numeric(hour_start))
#       )
#     ) %>%
#     filter(duration_in_hour_sec > 0)
# }
# 
# # ---------------------------
# # Expand to hourly rows, KEEPING all original columns
# # ---------------------------
# 
# manyapps_all_apps_split <- manyapps_all_apps %>%
#   mutate(
#     datetime_clean = as.POSIXct(datetime_clean),
#     duration = suppressWarnings(as.numeric(duration)),
#     end_time = datetime_clean + duration,
#     is_daily_only = is.na(time_only)
#   ) %>%
#   filter(!is.na(datetime_clean), !is.na(duration), duration > 0) %>%
#   rowwise() %>%
#   mutate(
#     hourly_list = if (!is_daily_only) {
#       list(split_session(datetime_clean, end_time))
#     } else {
#       list(tibble(
#         hour_start = as.POSIXct(NA),
#         hour_end = as.POSIXct(NA),
#         duration_in_hour_sec = NA_real_
#       ))
#     }
#   ) %>%
#   unnest(hourly_list, keep_empty = TRUE) %>%
#   ungroup() %>%
#   mutate( date_only = as.Date(hour_start), 
#           hourly_time = ifelse(is.na(hour_start), NA, format(hour_start, "%H:00:00")), 
#           day = as.Date(datetime_clean) )



# ---------------------------
# Daily per participant per app
# ---------------------------

participant_app_daily <- manyapps_all_apps_split %>%
  group_by(Dataset, unique_participant_number, Package_name, day) %>%
  summarise(
    total_daily_app_usage = if (first(is_daily_only)) {
      sum(duration, na.rm = TRUE)  # use original duration for daily-only
    } else {
      sum(duration_in_hour_sec, na.rm = TRUE)
    },
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
    total_hourly_app_usage = if (all(is.na(hour_start))) NA_real_ else sum(duration_in_hour_sec, na.rm = TRUE),
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

write.csv(manyapps_app_hourly_final,"/Users/f007qrc/projects/ManyApps_Data/complete_data_split_per_app.csv")


# ---------------------------
# No-app hourly table (keep demographics/other columns)
# ---------------------------

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
    "study_average_daily_app_usage")
)

hourly_context <- manyapps_app_hourly_final %>%
  group_by(across(all_of(join_keys_hour))) %>%
  summarise(
    across(all_of(setdiff(context_cols, join_keys_hour)), ~ dplyr::first(.x)),
    .groups = "drop"
  )

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
# IMPORTANT: ignore rows without hourly data
# -------------------------------------------------

daily_noapp <- manyapps_hourly_noapp %>%
  group_by(across(all_of(join_keys_day))) %>%
  summarise(
    total_daily_app_usage =
      if (all(is.na(total_hourly_app_usage))) pmin(sum(total_daily_app_usage),60*60*24) # cap daily seconds
    else sum(total_hourly_app_usage, na.rm = TRUE),
    .groups = "drop"
  )

manyapps_hourly_noapp = manyapps_hourly_noapp %>% select(-c(total_daily_app_usage))
# -------------------------------------------------
# STUDY DAILY TOTAL
# -------------------------------------------------

study_daily_noapp <- daily_noapp %>%
  group_by(Dataset,unique_participant_number) %>%
  summarise(study_average_daily_app_usage = sum(total_daily_app_usage, na.rm = TRUE),
    .groups = "drop"
  )

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
  left_join(uapps_overall, by = join_keys_person) %>%
  mutate(
    unique_apps_hour = dplyr::coalesce(unique_apps_hour, 0L),
    unique_apps_day = dplyr::coalesce(unique_apps_day, 0L),
    unique_apps_overall = dplyr::coalesce(unique_apps_overall, 0L)
  )



##### Some plots


library(dplyr)
library(ggplot2)
library(lubridate)


length(unique(manyapps_hourly_noapp$unique_participant_number))
length(unique(manyapps_hourly_noapp$Dataset))


# One plot: 7 weekday lines across 24 hours (with SD ribbon)
plot_df <- manyapps_hourly_noapp %>%
  mutate(
    hour = lubridate::hour(hour_start),
    wday_num = lubridate::wday(day, week_start = 1),
    wday = factor(
      wday_num,
      levels = 1:7,
      labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    )
  ) %>%
  group_by(wday, hour) %>%
  summarise(
    mean_min = median(total_hourly_app_usage, na.rm = TRUE),
    sd_min = sd(total_hourly_app_usage, na.rm = TRUE),
    .groups = "drop"
  )


peak_hour_df <- plot_df %>%
  group_by(hour) %>%
  summarise(mean_usage = mean(mean_min, na.rm = TRUE), .groups = "drop") %>%
  slice_max(mean_usage, n = 1)

#########

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(ggtext)



# Compute peak hour
peak_hour_df <- plot_df %>%
  group_by(hour) %>%
  summarise(mean_usage = mean(mean_min, na.rm = TRUE), .groups = "drop") %>%
  slice_max(mean_usage, n = 1)

min_hour_df <- plot_df %>%
  group_by(hour) %>%
  summarise(mean_usage = mean(mean_min, na.rm = TRUE), .groups = "drop") %>%
  slice_min(mean_usage, n = 1)

plot_df <- plot_df %>% mutate( day_group = case_when( wday == "Friday" ~ "Friday", wday == "Saturday" ~ "Saturday", wday == "Sunday" ~ "Sunday", TRUE ~ "Weekday" ) )



pa  = ggplot(plot_df,
            aes(x = hour, y = mean_min, group = wday)) +
  
  # -----------------------
# Weekdays
geom_line(
  data = subset(plot_df, day_group == "Weekday"),
  color = "#A0A6AC",
  linewidth = 0.9,
  alpha = 0.40
) +
  
  # Friday
  geom_line(
    data = subset(plot_df, day_group == "Friday"),
    color = "#457B9D",
    linewidth = 1.3
  ) +
  
  # Saturday
  geom_line(
    data = subset(plot_df, day_group == "Saturday"),
    color = "#2A9D8F",
    linewidth = 1.6
  ) +
  
  # Sunday
  geom_line(
    data = subset(plot_df, day_group == "Sunday"),
    color = "#E76F51",
    linewidth = 1.6
  ) +
  
  # -----------------------
# Annotations
annotate(
  "text",
  x = 19, y = 5.6,
  label = "Friday & Saturday:\nless phone use\nin afternoon/evening",
  hjust = 0,
  size = 3.7,
  color = "grey20"
) +
  
  annotate(
    "text",
    x = 7, y = 3.2,
    label = "Sunday:\nphone use\nstarts later",
    hjust = 0,
    size = 3.7,
    color = "grey20"
  ) +
  
  # -----------------------
# Highlight peak hour
geom_vline(
  data = peak_hour_df,
  aes(xintercept = hour),
  linetype = "dashed",
  color = "grey35",
  linewidth = 0.7
) +
  
  annotate(
    "label",
    x = peak_hour_df$hour,
    y = 11.2,
    label = paste0("Peak usage\n", peak_hour_df$hour, ":00"),
    size = 3.6,
    label.size = 0,
    fill = "white",
    color = "grey20"
  ) +
  
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  
  labs(
    title = paste0(
      "Average App Usage per Hour — ",
      "<span style='color:#A0A6AC;'>Mon–Thu</span>, ",
      "<span style='color:#457B9D;'>Friday</span>, ",
      "<span style='color:#2A9D8F;'>Saturday</span>, ",
      "<span style='color:#E76F51;'>Sunday</span>"
    ),
    x = "Hour of Day",
    y = "Average App Usage (min/hour)"
  ) +
  
  theme_ipsum(base_size = 13) +
  geom_vline(
    data = min_hour_df,
    aes(xintercept = hour),
    linetype = "dashed",
    color = "grey60",
    linewidth = 0.7
  ) +
  
  # label for minimum usage
  annotate(
    "label",
    x = min_hour_df$hour,
    y = 11.2,  # slightly above line
    label = paste0("Lowest usage\n", min_hour_df$hour, ":00"),
    size = 3.6,
    label.size = 0,
    fill = "white",
    color = "grey40"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_markdown(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )




pa




library(ggplot2)
library(hrbrthemes)
library(ggtext)


plot_df <- manyapps_app_usage_hourly %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    date = as.Date(timestamp),
    hour = hour(timestamp),
    wday = weekdays(date)
  ) %>%
  group_by(unique_participant_number, date, wday, hour) %>%
  summarise(
    hour_min = sum(duration, na.rm = TRUE) * 60, # convert from hours to minutes
    .groups = "drop"
  ) %>%
  group_by(wday, hour) %>%
  summarise(
    mean_min = median(hour_min, na.rm = TRUE),
    sd_min   = sd(hour_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    day_group = case_when(
      wday == "Friday"   ~ "Friday",
      wday == "Saturday" ~ "Saturday",
      wday == "Sunday"   ~ "Sunday",
      TRUE               ~ "Weekday"
    )
  )



week_plot_df <- manyapps_app_usage_hourly %>%
  mutate(
    date = as.Date(timestamp),
    wday = weekdays(date)
  ) %>%
  group_by(unique_participant_number, date, wday) %>%
  summarise(
    day_min = sum(duration, na.rm = TRUE), # minutes/day
    .groups = "drop"
  ) %>%
  group_by(wday) %>%
  summarise(
    mean_min = median(day_min, na.rm = TRUE),
    sd_min   = sd(day_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    wday = factor(
      wday,
      levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    ),
    day_group = case_when(
      wday == "Friday"   ~ "Friday",
      wday == "Saturday" ~ "Saturday",
      wday == "Sunday"   ~ "Sunday",
      TRUE               ~ "Weekday"
    )
  )


pb = ggplot(week_plot_df,
            aes(x = wday, y = mean_min/60, group = 1)) +
  
  # connecting baseline (subtle)
  geom_line(color = "#D0D4D8", linewidth = 0.8) +
  
  # weekdays
  geom_point(
    data = subset(week_plot_df, day_group == "Weekday"),
    color = "#A0A6AC",
    size = 3
  ) +
  
  # friday
  geom_point(
    data = subset(week_plot_df, day_group == "Friday"),
    color = "#457B9D",
    size = 3
  ) +
  
  # saturday
  geom_point(
    data = subset(week_plot_df, day_group == "Saturday"),
    color = "#2A9D8F",
    size = 3
  ) +
  
  # sunday
  geom_point(
    data = subset(week_plot_df, day_group == "Sunday"),
    color = "#E76F51",
    size = 3
  ) +
  
  # optional SD error bars (light + minimal)
  geom_errorbar(
    aes(ymin = mean_min/60 - sd_min/60,
        ymax = mean_min/60 + sd_min/60),
    width = 0.15,
    color = "grey70",
    linewidth = 0.5
  ) +
  
  labs(
    title = paste0(
      "Average Daily App Usage — ",
      "<span style='color:#A0A6AC;'>Mon–Thu</span>, ",
      "<span style='color:#457B9D;'>Friday</span>, ",
      "<span style='color:#2A9D8F;'>Saturday</span>, ",
      "<span style='color:#E76F51;'>Sunday</span>"
    ),
    x = NULL,
    y = "Average Daily Usage (hours)"
  ) +
  
  theme_ipsum(base_size = 13) +
  theme(
    legend.position = "none",
    plot.title = element_markdown(),
    panel.grid.minor = element_blank()
  )
#ylim(c(0,3))



library(patchwork)

pa / pb + 
  plot_layout(heights = c(2, 1))
