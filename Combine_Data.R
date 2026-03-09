
##############################################
########## Combine + Hourly Aggregate ########
##############################################

library(dplyr)


cleaned_apps_dir <- "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all"
available_csv <- list.files(cleaned_apps_dir, pattern = "\\.csv$", full.names = FALSE)
existing_app_objects <- sub("\\.csv$", "", available_csv)

apps_list <- lapply(existing_app_objects, function(obj_name) {
  f <- file.path(cleaned_apps_dir, paste0(obj_name, ".csv"))
  df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  df
})

names(apps_list) <- existing_app_objects

apps_list
### Harmonize timstamps
### Check timestamps
for (nm in names(apps_list)) {
  cat("\n---", nm, "---\n")
  
  df <- apps_list[[nm]]
  
  if ("timestamp" %in% names(df)) {
    ts <- df$timestamp
    cat("Class:", class(ts), "\n")
    cat("Example values:\n")
    print(head(unique(ts), 5))
  } else {
    cat("No timestamp column\n")
  }
}

for (nm in names(apps_list)) {
  
  df <- apps_list[[nm]]
  
  # skip datasets without timestamp
  if (!("timestamp" %in% names(df))) next
  
  ts <- as.character(df$timestamp)
  
  # ---- remove milliseconds if present ----
  ts_clean <- sub("\\..*$", "", ts)
  
  # detect date-only rows
  has_time <- grepl(" ", ts_clean)
  
  # ---- create columns ----
  date_only <- ifelse(
    has_time,
    sub(" .*", "", ts_clean),
    ts_clean
  )
  
  time_only <- ifelse(
    has_time,
    sub(".* ", "", ts_clean),
    NA
  )
  
  datetime_clean <- ifelse(
    has_time,
    ts_clean,
    paste(ts_clean, "00:00:00")
  )
  
  # add back to dataset
  df$date_only <- date_only
  df$time_only <- time_only
  df$datetime_clean <- datetime_clean
  
  apps_list[[nm]] <- df
}

##########
apps_list <- lapply(apps_list, function(df) {
  df[, names(df) != "", drop = FALSE]
})

all_cols <- unique(unlist(lapply(apps_list, names)))
all_apps_df <- do.call(rbind, apps_list)


write.csv(all_apps_df,"/Users/f007qrc/projects/ManyApps_Data/complete_data_beforecleaning.csv")
all_apps_df = read.csv("/Users/f007qrc/projects/ManyApps_Data/complete_data_beforecleaning.csv")

manyapps_all_apps_raw <- all_apps_df
unique_participants <- unique(manyapps_all_apps_raw[, c("participant_number", "Dataset")])
table(unique_participants$Dataset)
unique_participants <- unique(manyapps_all_apps_raw[, c("participant_number", "country")])
table(unique_participants$country)

length(unique(manyapps_all_apps_raw$unique_participant_number))

#################################################################
########## Data Cleaning as described in Preregistration ########
#################################################################
sum(is.na(manyapps_all_apps_raw$timestamp))
unique(manyapps_all_apps_raw$Dataset[is.na(manyapps_all_apps_raw$timestamp)])


# Keep only valid timestamps and non-negative duration
manyapps_all_apps_raw <- manyapps_all_apps_raw %>%
  filter(!is.na(timestamp), !is.na(participant_number), !is.na(Package_name)) %>%
  mutate(
    duration = suppressWarnings(as.numeric(duration)),
    duration = ifelse(is.na(duration) | duration < 0, NA_real_, duration),
    day = as.Date(timestamp)
  )

# Use only first 14 days per participant within each dataset
participant_windows <- manyapps_all_apps_raw %>%
  group_by(Dataset, unique_participant_number) %>%
  summarise(first_day = min(day, na.rm = TRUE), .groups = "drop") %>%
  mutate(last_day_14 = first_day + 13)


manyapps_14d <- manyapps_all_apps_raw %>%
  inner_join(participant_windows, by = c("Dataset", "unique_participant_number")) %>%
  filter(day >= first_day, day <= last_day_14)

# Label days with no app usage as missing
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
sum(participant_quality$include_participant == TRUE)


included_participants <- participant_quality %>%
  filter(include_participant) %>%
  select(Dataset,  unique_participant_number)

manyapps_all_apps <- manyapps_14d %>%
  inner_join(included_participants, by = c("Dataset",  "unique_participant_number"))




write.csv(manyapps_all_apps,"/Users/f007qrc/projects/ManyApps_Data/complete_data_cleaning_stage1.csv")

length(unique(manyapps_all_apps$unique_participant_number))
length(unique(manyapps_all_apps$Dataset))

###########

system_schoedel <- read.csv("/Users/f007qrc/projects/ManyApps_Data/systemapps_schoedel.csv")
system_schoedel <- system_schoedel$App_name[system_schoedel$Final_Rating == "System"]
system <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Apps_categorization_final.csv")
system <- system$app_name[system$googleCats == "system"]

system_all <- c(system,system_schoedel)
manyapps_all_apps <- manyapps_all_apps[!manyapps_all_apps$Package_name %in% system_all, ]

write.csv(manyapps_all_apps,"/Users/f007qrc/projects/ManyApps_Data/complete_data_cleaning_stage1.csv")

manyapps_all_apps = read.csv("/Users/f007qrc/projects/ManyApps_Data/complete_data_cleaning_stage1.csv")

manyapps_all_apps= manyapps_all_apps[manyapps_all_apps$Dataset == "Corona_Parent" | manyapps_all_apps$Dataset == "Spain 1",]
manyapps_all_apps = manyapps_all_apps_copy

# ---------------------------
# Full Overview (Including Package Name)
# ---------------------------

library(dplyr)
library(tidyr)
library(lubridate)

# robust splitter (seconds)
split_session <- function(start, end) {
  if (is.na(start) || is.na(end) || end <= start) {
    return(tibble(
      hour_start = as.POSIXct(character(0)),
      hour_end = as.POSIXct(character(0)),
      duration_in_hour_sec = numeric(0)
    ))
  }
  
  hour_seq <- seq(floor_date(start, "hour"), ceiling_date(end, "hour"), by = "hour")
  if (length(hour_seq) < 2) {
    hour_seq <- c(floor_date(start, "hour"), floor_date(start, "hour") + hours(1))
  }
  
  tibble(
    hour_start = head(hour_seq, -1),
    hour_end   = tail(hour_seq, -1)
  ) %>%
    mutate(
      duration_in_hour_sec = pmax(
        0,
        pmin(as.numeric(end), as.numeric(hour_end)) -
          pmax(as.numeric(start), as.numeric(hour_start))
      )
    ) %>%
    filter(duration_in_hour_sec > 0)
}

# ---------------------------
# Expand to hourly rows, KEEPING all original columns
# ---------------------------

manyapps_all_apps_split <- manyapps_all_apps %>%
  mutate(
    datetime_clean = as.POSIXct(datetime_clean),
    duration = suppressWarnings(as.numeric(duration)),
    end_time = datetime_clean + duration,
    is_daily_only = is.na(time_only)
  ) %>%
  filter(!is.na(datetime_clean), !is.na(duration), duration > 0) %>%
  rowwise() %>%
  mutate(
    hourly_list = if (!is_daily_only) {
      list(split_session(datetime_clean, end_time))
    } else {
      list(tibble(
        hour_start = as.POSIXct(NA),
        hour_end = as.POSIXct(NA),
        duration_in_hour_sec = NA_real_
      ))
    }
  ) %>%
  unnest(hourly_list, keep_empty = TRUE) %>%
  ungroup() %>%
  mutate( date_only = as.Date(hour_start), 
          hourly_time = ifelse(is.na(hour_start), NA, format(hour_start, "%H:00:00")), 
          day = as.Date(datetime_clean) )

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





# Exclude problematic hours from app-switch calculations
manyapps_app_switches_hourly <- manyapps_all_apps %>%
  mutate(hour = floor_date(timestamp, unit = "hour")) %>%
  left_join(
    manyapps_hourly_totals %>%
      select(Dataset, unique_participant_number, hour, hour_exceeds_60min),
    by = c("Dataset", "unique_participant_number", "hour")
  ) %>%
  filter(is.na(hour_exceeds_60min) | hour_exceeds_60min == FALSE) %>%
  arrange(Dataset, unique_participant_number, hour, timestamp) %>%
  group_by(Dataset, unique_participant_number, hour) %>%
  summarise(
    app_switches = sum(Package_name != lag(Package_name), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(timestamp = hour)





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



pa  =ggplot(plot_df,
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
