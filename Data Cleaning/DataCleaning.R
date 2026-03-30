
####################################
######## Data Cleaning #############
####################################

library(data.table)
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(jsonlite)



#cd("/Users/f007qrc/projects/ManyApps_Data")

################################################
########## Load individual datasets  ###########
################################################
# We first load all datasets and do some basic transformation (e.g., rename all columns)

############# klingelhoefer ############
klingelhoefer <- read.csv("/Users/f007qrc/projects/ManyApps_Data/klingelhoefer/data_klingelhoefer_disconnection.csv")
klingelhoefer_apps <- klingelhoefer[colnames(klingelhoefer) %in% c("participant_number","Package_name","start_time","end_time","Dataset" )]

klingelhoefer_apps$start_time <- ymd_hms(klingelhoefer_apps$start_time)
klingelhoefer_apps$end_time   <- ymd_hms(klingelhoefer_apps$end_time)
klingelhoefer_apps$duration <- as.numeric(klingelhoefer_apps$end_time - klingelhoefer_apps$start_time)

colnames(klingelhoefer_apps)[colnames(klingelhoefer_apps) == "start_time"] <- "timestamp"

klingelhoefer_apps <- klingelhoefer_apps[colnames(klingelhoefer_apps) %in% c("participant_number","Package_name","duration","timestamp","Dataset" )]
klingelhoefer_apps$unique_participant_number <- paste0(
  substr(klingelhoefer_apps$Dataset, 1, 3),
  "_",
  klingelhoefer_apps$participant_number
)

colnames(klingelhoefer_apps)

top20_packages <- klingelhoefer_apps %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

#top20_packages

klingelhoefer_apps <- klingelhoefer_apps[klingelhoefer_apps$Package_name != "com.movisens.xs.android.core", ]

write.csv(klingelhoefer_apps,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/klingelhoefer_apps.csv")

############# ERC ############# 
erc_file <- "/Users/f007qrc/projects/ManyApps_Data/ERC/manyApps_rawdata_disconnect.csv"

erc_raw <- fread(erc_file, na.strings = c("", "NA"))

erc_apps <- erc_raw %>%
  mutate(
    participant_number = as.character(participant_number),
    start_time = suppressWarnings(ymd_hms(start_time, quiet = TRUE)),
    end_time = suppressWarnings(ymd_hms(end_time, quiet = TRUE)),
    duration = as.numeric(end_time - start_time),   # seconds
    timestamp = start_time,
    Dataset = as.character(Dataset)
  ) %>%
  select(participant_number, Package_name, duration, timestamp, Dataset)

erc_apps$unique_participant_number <- paste0(
  substr(erc_apps$Dataset, 1, 3),
  "_",
  erc_apps$participant_number
)


top20_packages <- erc_apps %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

#top20_packages

erc_apps <- erc_apps[erc_apps$Package_name != "io.m_Path.kuleuven", ]

write.csv(erc_apps,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/erc_apps.csv")


############# Chow ############# 
chow_folder <- "/Users/f007qrc/projects/ManyApps_Data/Chow/APPUSAGE"

# Read both export styles:
# 1) *_APPUSAGE_usageLog_*.csv (has id_participant, tm_usagewindow_start)
# 2) APPUSAGE_data_*.csv (participant id embedded in filename)
chow_files <- list.files(
  chow_folder,
  pattern = "(_APPUSAGE_usageLog_.*\\.csv$)|(^APPUSAGE_data_.*\\.csv$)",
  full.names = TRUE
)

parse_chow_file <- function(f) {
  dt <- fread(f, fill = TRUE, na.strings = c("", "NA"))
  
  # Skip file if id_participant is missing
  if (!"id_participant" %in% names(dt)) {
    message("Skipping file (no id_participant): ", f)
    return(NULL)
  }
  
  participant_number <- suppressWarnings(as.numeric(dt$id_participant))
  
  # timestamp
  timestamp <- if ("tm_usagewindow_start" %in% names(dt)) {
    suppressWarnings(ymd_hms(dt$tm_usagewindow_start, quiet = TRUE))
  } else {
    as.POSIXct(rep(NA, nrow(dt)))
  }
  
  # duration in seconds (to match klingelhoefer_apps style)
  duration <- if ("n_foreground_ms" %in% names(dt)) as.numeric(dt$n_foreground_ms) / 1000 else NA_real_
  
  out <- data.frame(
    participant_number = participant_number,
    Package_name = if ("id_app" %in% names(dt)) dt$id_app else NA_character_,
    duration = duration,
    timestamp = timestamp,
    Dataset = "Chow_APPUSAGE",
    stringsAsFactors = FALSE
  )
  
  out$unique_participant_number <- paste0(substr(out$Dataset, 1, 3), "_", out$participant_number)
  out
}

# Bind only non-NULL results
chow_apps <- bind_rows(lapply(chow_files, parse_chow_file))

# Match exact klingelhoefer_apps format (if object exists)
if (exists("klingelhoefer_apps")) {
  missing_cols <- setdiff(colnames(klingelhoefer_apps), colnames(chow_apps))
  for (nm in missing_cols) chow_apps[[nm]] <- NA
  chow_apps <- chow_apps[, colnames(klingelhoefer_apps), drop = FALSE]
}

length(unique(chow_apps$participant_number))

remove_ids <- c(4, 20, 70, 94, 214, 253, 283, 494, 153)
chow_apps <- chow_apps[!chow_apps$participant_number %in% remove_ids, ]

# Check how many unique participants remain
length(unique(chow_apps$participant_number))


sum(is.na(chow_apps$timestamp))/nrow(chow_apps)


top20_packages <- chow_apps %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

#top20_packages

write.csv(chow_apps,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/chow_apps.csv")



############# Corona Health (Daily Summary) #############


parse_corona_app_daily <- function(appdata_str, participant_id, dataset_label, tz = "Europe/Berlin") {
  empty <- data.frame(
    participant_number = character(0),
    Package_name = character(0),
    duration = numeric(0),
    timestamp = as.Date(character(0)),
    Dataset = character(0),
    stringsAsFactors = FALSE
  )

  if (is.na(appdata_str) || !nzchar(appdata_str)) return(empty)

  appdata_json <- gsub("'", "\"", appdata_str, fixed = TRUE)
  app_list <- tryCatch(jsonlite::fromJSON(appdata_json, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(app_list) || length(app_list) == 0) return(empty)

  rows <- lapply(app_list, function(app) {
    app_name <- if (!is.null(app$packageName) && length(app$packageName) > 0) app$packageName else NA_character_
    daily_vals <- app$dailyValues
    if (is.null(daily_vals) || length(daily_vals) == 0) return(NULL)

    out <- lapply(daily_vals, function(dv) {
      use_seconds <- suppressWarnings(as.numeric(dv$useTime)) / 1000
      if (is.na(use_seconds) || use_seconds <= 0) return(NULL)

      ts_candidates <- suppressWarnings(as.numeric(c(
        dv$firstUseTime, dv$lastUseTime, dv$firstFgServiceUseTime, dv$lastFgServiceUseTime
      )))
      ts_candidates <- ts_candidates[is.finite(ts_candidates) & ts_candidates > 0]
      if (length(ts_candidates) == 0) return(NULL)

      day <- as.Date(with_tz(as_datetime(ts_candidates[1], tz = "UTC"), tzone = tz))
      data.frame(
        participant_number = as.character(participant_id),
        Package_name = app_name,
        duration = use_seconds,
        timestamp = day,
        Dataset = dataset_label,
        stringsAsFactors = FALSE
      )
    })

    out <- Filter(Negate(is.null), out)
    if (length(out) == 0) return(NULL)
    do.call(rbind, out)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)

  do.call(rbind, rows) 

}

build_corona_daily <- function(df, dataset_label, tz = "Europe/Berlin") {
  ids_withapp <- unique(df$user_id[!is.na(df$appdata_apps) & nzchar(df$appdata_apps)])
  df <- df[df$user_id %in% ids_withapp, , drop = FALSE]

  out <- lapply(seq_len(nrow(df)), function(i) {
    parse_corona_app_daily(
      appdata_str = df$appdata_apps[i],
      participant_id = df$user_id[i],
      dataset_label = dataset_label,
      tz = tz
    )
  })

  out <- Filter(function(x) nrow(x) > 0, out)
  if (length(out) == 0) {
    return(data.frame(
      participant_number = character(0),
      Package_name = character(0),
      duration = numeric(0),
      timestamp = as.Date(character(0)),
      Dataset = character(0),
      unique_participant_number = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- bind_rows(out) 

  out$unique_participant_number <- paste0(substr(out$Dataset, 1, 3), "_", out$participant_number)
  out
}

stress_b <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_stress_baseline_090226.csv", sep = ",")
stress_f <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_stress_followUp_090226.csv", sep = ",")
heart <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_heart_baseline_090226.csv", sep = ";")
parent_b <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_parent_baseline_090226.csv", sep = ";")
parent_f <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Corona Health/ch_parent_followUp_090226.csv", sep = ";")



stress_b_apps_daily <- build_corona_daily(stress_b, "Corona_Stress", tz = "Europe/Berlin")
stress_f_apps_daily <- build_corona_daily(stress_f, "Corona_Stress", tz = "Europe/Berlin")
stress_apps_daily <- bind_rows(stress_b_apps_daily, stress_f_apps_daily)

heart_apps_daily <- build_corona_daily(heart, "Corona_Heart", tz = "Europe/Berlin")

parent_b_apps_daily <- build_corona_daily(parent_b, "Corona_Parent", tz = "Europe/Berlin")
parent_f_apps_daily <- build_corona_daily(parent_f, "Corona_Parent", tz = "Europe/Berlin")
parent_apps_daily <- bind_rows(parent_b_apps_daily, parent_f_apps_daily)


top20_packages <- heart_apps_daily %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 50)

#top20_packages

write.csv(heart_apps_daily,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_apps_daily.csv")

write.csv(stress_apps_daily,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/stress_apps_daily.csv")

write.csv(parent_apps_daily,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/parent_apps_daily.csv")


############# eMotion ############# 
# looks clean

emotion_dem <- read.csv("/Users/f007qrc/projects/ManyApps_Data/eMotion/ManyApps_eMotion_Demographics.csv")[-1]
emotion_apps <- read.csv("/Users/f007qrc/projects/ManyApps_Data/eMotion/ManyApps_eMotion_RawData.csv")[-1]

emotion_apps$unique_participant_number <- paste0(
  substr(emotion_apps$Dataset, 1, 3),
  "_",
  emotion_apps$participant_number
)



emotion_apps <- emotion_apps %>%
  mutate(
    start_time = suppressWarnings(ymd_hms(start_time)),
    end_time = suppressWarnings(ymd_hms(end_time)),
    duration = as.numeric(end_time - start_time),
    timestamp = start_time
  ) %>%
  select(participant_number, Package_name, duration, timestamp, Dataset, unique_participant_number)

colnames(emotion_apps)



top20_packages <- emotion_apps %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

#top20_packages

write.csv(emotion_apps,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/emotion_apps.csv")
############# Aurelio ############# 

aur_env1 <- new.env()
aur_env2 <- new.env()
load("/Users/f007qrc/projects/ManyApps_Data/Aurelio/New/AppData_Spain1.RData", envir = aur_env1)
load("/Users/f007qrc/projects/ManyApps_Data/Aurelio/New/AppData_Spain2.RData", envir = aur_env2)

aurelio_apps_raw <- bind_rows(aur_env1$ld_SP1, aur_env2$ld_SP2)

aurelio_apps <- aurelio_apps_raw %>%
  mutate(
    timestamp = as.POSIXct(paste(day, start_time), format = "%Y-%m-%d %H:%M:%S"),
    duration = as.numeric(difftime(
      as.POSIXct(paste(day, end_time), format="%Y-%m-%d %H:%M:%S"),
      as.POSIXct(paste(day, start_time), format="%Y-%m-%d %H:%M:%S"),
      units = "secs"
    ))
  ) %>%
  select(participant_number, Package_name, duration, timestamp, Dataset) %>%
  mutate(
    unique_participant_number = paste0(substr(Dataset, 1, 3), "_", participant_number)
  )
missing_cols <- setdiff(colnames(klingelhoefer_apps), colnames(aurelio_apps))
for (nm in missing_cols) {
  aurelio_apps[[nm]] <- NA
}
aurelio_apps <- aurelio_apps[, colnames(klingelhoefer_apps), drop = FALSE]


top20_packages <- aurelio_apps %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

#top20_packages

aurelio_apps <- aurelio_apps[aurelio_apps$Package_name != "com.ethica.logger", ]

write.csv(aurelio_apps,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/aurelio_apps.csv")

############# Study_Smart ############# 

build_study_smart_wave <- function(folder, dataset_label, target_cols) {
  files <- sort(list.files(folder, pattern = "\\.csv$", full.names = TRUE))

  dt_list <- lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    if (length(lines) < 2) return(NULL)

    header <- lines[1]
    rows <- lines[-1]

    # Drop a wrapping quote around each full row and unescape doubled quotes
    rows <- sub('^"(.*)"$', "\\1", rows)
    rows <- gsub('""', '"', rows, fixed = TRUE)

    fixed_text <- paste(c(header, rows), collapse = "\n")
    fread(
      text = fixed_text,
      sep = ",",
      header = TRUE,
      colClasses = "character",
      fill = TRUE,
      na.strings = c("", "NA")
    )
  })

  dt_list <- dt_list[!vapply(dt_list, is.null, logical(1))]
  combined <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

  wave_df <- combined %>%
    transmute(
      participant_number = suppressWarnings(as.numeric(gsub(",", "", `User ID`, fixed = TRUE))),
      Package_name = `Package Name`,
      duration = suppressWarnings(as.numeric(gsub(",", "", Duration, fixed = TRUE))),
      timestamp = suppressWarnings(ymd_hm(`Start Time`)),
      Dataset = dataset_label
    )

  wave_df$unique_participant_number <- paste0(
    substr(wave_df$Dataset, 1, 3),
    "_",
    wave_df$participant_number
  )

  missing_cols <- setdiff(target_cols, colnames(wave_df))
  for (nm in missing_cols) {
    wave_df[[nm]] <- NA
  }
  wave_df <- wave_df[, ..target_cols]
  wave_df
}

study_smart_w1 <- build_study_smart_wave(
  folder = "/Users/f007qrc/projects/ManyApps_Data/Study_Smart/Daten_1.Welle",
  dataset_label = "Study_Smart_W1",
  target_cols = colnames(klingelhoefer_apps)
)

study_smart_w2 <- build_study_smart_wave(
  folder = "/Users/f007qrc/projects/ManyApps_Data/Study_Smart/Daten_2.Welle",
  dataset_label = "Study_Smart_W2",
  target_cols = colnames(klingelhoefer_apps)
)

study_smart_w3 <- build_study_smart_wave(
  folder = "/Users/f007qrc/projects/ManyApps_Data/Study_Smart/Daten_3.Welle",
  dataset_label = "Study_Smart_W3",
  target_cols = colnames(klingelhoefer_apps)
)

study_smart = rbind(study_smart_w3,study_smart_w2, study_smart_w1)

top20_packages <- study_smart %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

#top20_packages

write.csv(study_smart,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/study_smart.csv")


### demographics

path <- "/Users/f007qrc/projects/ManyApps_Data/Study_Smart/Studysmart_WBMeasures_withtime.csv"

lines <- readLines(path, warn = FALSE)
fixed <- sub('^"(.*)"$', '\\1', lines)          # remove outer row quotes
fixed <- gsub('""', '"', fixed, fixed = TRUE)   # unescape quotes

study_smart_dem <- read.csv(
  text = paste(fixed, collapse = "\n"),
  sep = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# optional: drop empty first column
if (names(study_smart_dem)[1] == "") {
  study_smart_dem <- study_smart_dem[, -1]
}



############# Yannik ############# 

yannik <- read.csv(
  "/Users/f007qrc/projects/ManyApps_Data/Yannik/hetzner_ssps.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Drop index-like columns from export
if (names(yannik)[1] == "") {
  yannik <- yannik[, -1, drop = FALSE]
}
if ("X" %in% names(yannik)) {
  yannik <- yannik[, setdiff(names(yannik), "X"), drop = FALSE]
}

yannik_apps <- yannik %>%
  transmute(
    participant_number = as.character(user_id),
    Package_name = packageName,
    duration = suppressWarnings(as.numeric(session_duration)),
    timestamp = as.POSIXct(session_start, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Berlin"),
    Dataset = "Yannik"
  )

yannik_apps$unique_participant_number <- paste0(
  substr(yannik_apps$Dataset, 1, 3),
  "_",
  yannik_apps$participant_number
)

unique(yannik_apps$unique_participant_number )


top20_packages <- yannik_apps %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

top20_packages

write.csv(yannik_apps,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/yannik_apps.csv")


####### Aidan #####

aidan_folder <- "/Users/f007qrc/projects/ManyApps_Data/Aidan/APPUSAGE"
aidan_files <- list.files(
  aidan_folder,
  pattern = "_APPUSAGE_usageLog_.*\\.csv$",
  full.names = TRUE
)

parse_aidan_file <- function(f) {
  dt <- fread(f, fill = TRUE, na.strings = c("", "NA"))

  # Same style as Chow: require participant/app/start-time fields
  if (!"id_participant" %in% names(dt)) {
    message("Skipping file (no id_participant): ", f)
    return(NULL)
  }

  participant_number <- suppressWarnings(as.numeric(dt$id_participant))

  # Use readable timestamp field, not epoch
  timestamp <- if ("tm_usagewindow_start" %in% names(dt)) {
    suppressWarnings(ymd_hms(trimws(as.character(dt$tm_usagewindow_start)), quiet = TRUE))
  } else {
    as.POSIXct(rep(NA, nrow(dt)))
  }

  # duration in seconds
  duration <- if ("n_foreground_ms" %in% names(dt)) as.numeric(dt$n_foreground_ms) / 1000 else NA_real_

  out <- data.frame(
    participant_number = participant_number,
    Package_name = if ("id_app" %in% names(dt)) dt$id_app else NA_character_,
    duration = duration,
    timestamp = timestamp,
    Dataset = "Aidan_APPUSAGE",
    stringsAsFactors = FALSE
  )

  out$unique_participant_number <- paste0(substr(out$Dataset, 1, 3), "_", out$participant_number)
  out
}

aidan_apps <- bind_rows(lapply(aidan_files, parse_aidan_file))


top20_packages <- aidan_apps %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

top20_packages


if (exists("klingelhoefer_apps")) {
  missing_cols <- setdiff(colnames(klingelhoefer_apps), colnames(aidan_apps))
  for (nm in missing_cols) aidan_apps[[nm]] <- NA
  aidan_apps <- aidan_apps[, colnames(klingelhoefer_apps), drop = FALSE]
}

write.csv(aidan_apps, "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/aidan_apps.csv")



###### MoodyLife ####

moodylife <- read.csv("/Users/f007qrc/projects/ManyApps_Data/MoodyLife/Data Exchange/sensingdata.csv")

moodylife

colnames(moodylife)[colnames(moodylife) == "package_name"] <- "Package_name"

colnames(moodylife)[colnames(moodylife) == "start_time"] <- "timestamp"

colnames(moodylife)[colnames(moodylife) == "dataset"] <- "Dataset"


moodylife <- moodylife[colnames(moodylife) %in% c("participant_number","Package_name","duration","timestamp","Dataset" )]
moodylife$unique_participant_number <- paste0(
  substr(moodylife$Dataset, 1, 3),
  "_",
  moodylife$participant_number
)

colnames(moodylife)

top20_packages <- moodylife %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

top20_packages

write.csv(moodylife,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/moodylife_apps.csv")

length(unique(moodylife$participant_number))
