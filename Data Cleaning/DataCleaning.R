
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

inc_stat <- function(env, name, n = 1L) {
  if (is.null(env[[name]])) env[[name]] <- 0L
  env[[name]] <- env[[name]] + as.integer(n)
}

inc_field_counts <- function(env, name, fields) {
  if (is.null(env[[name]])) env[[name]] <- list()
  if (length(fields) == 0) return(invisible(NULL))
  for (f in fields) {
    if (is.null(env[[name]][[f]])) env[[name]][[f]] <- 0L
    env[[name]][[f]] <- env[[name]][[f]] + 1L
  }
}

update_minmax <- function(env, name_min, name_max, value) {
  if (is.na(value)) return(invisible(NULL))
  if (is.null(env[[name_min]]) || value < env[[name_min]]) env[[name_min]] <- value
  if (is.null(env[[name_max]]) || value > env[[name_max]]) env[[name_max]] <- value
}

parse_corona_app_daily <- function(appdata_str, participant_id, dataset_label, tz = "Europe/Berlin", stats_env = NULL) {
  empty <- data.frame(
    participant_number = character(0),
    Package_name = character(0),
    duration = numeric(0),
    timestamp = as.Date(character(0)),
    Dataset = character(0),
    stringsAsFactors = FALSE
  )

  if (is.na(appdata_str) || !nzchar(appdata_str)) {
    if (!is.null(stats_env)) inc_stat(stats_env, "empty_appdata")
    return(empty)
  }

  appdata_json <- gsub("'", "\"", appdata_str, fixed = TRUE)
  app_list <- tryCatch(jsonlite::fromJSON(appdata_json, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(app_list)) {
    if (!is.null(stats_env)) inc_stat(stats_env, "json_parse_fail")
    return(empty)
  }
  if (length(app_list) == 0) {
    if (!is.null(stats_env)) inc_stat(stats_env, "no_apps")
    return(empty)
  }

  rows <- lapply(app_list, function(app) {
    app_name <- if (!is.null(app$packageName) && length(app$packageName) > 0) app$packageName else NA_character_
    daily_vals <- app$dailyValues
    if (is.null(daily_vals) || length(daily_vals) == 0) {
      if (!is.null(stats_env)) inc_stat(stats_env, "no_daily_values")
      return(NULL)
    }
    app_complete_raw <- suppressWarnings(as.numeric(app$completeUseTime))
    if (!is.null(stats_env) && length(app_complete_raw) == 1 && !is.na(app_complete_raw)) {
      inc_stat(stats_env, "app_complete_present")
      if (is.null(stats_env$app_complete_sum)) stats_env$app_complete_sum <- 0
      stats_env$app_complete_sum <- stats_env$app_complete_sum + app_complete_raw
    }
    app_daily_sum <- 0
    app_daily_count <- 0

    out <- lapply(daily_vals, function(dv) {
      use_time_raw <- suppressWarnings(as.numeric(dv$useTime))
      complete_use_raw <- suppressWarnings(as.numeric(dv$completeUseTime))
      if (!is.null(stats_env) && length(complete_use_raw) == 1 && !is.na(complete_use_raw)) {
        inc_stat(stats_env, "complete_use_present")
        if (is.null(stats_env$complete_use_sum)) stats_env$complete_use_sum <- 0
        stats_env$complete_use_sum <- stats_env$complete_use_sum + complete_use_raw
      }
      if (!is.null(stats_env) && length(use_time_raw) == 1 && length(complete_use_raw) == 1 &&
          !is.na(use_time_raw) && !is.na(complete_use_raw)) {
        inc_stat(stats_env, "use_and_complete_present")
        if (is.null(stats_env$use_sum_for_complete)) stats_env$use_sum_for_complete <- 0
        stats_env$use_sum_for_complete <- stats_env$use_sum_for_complete + use_time_raw
      }
      if (!is.null(stats_env) && length(use_time_raw) == 1 && !is.na(use_time_raw)) {
        inc_stat(stats_env, "use_time_total")
        update_minmax(stats_env, "use_time_min", "use_time_max", use_time_raw)
      }
      if (length(use_time_raw) != 1 || is.na(use_time_raw)) {
        if (!is.null(stats_env)) {
          inc_stat(stats_env, "use_time_missing")
          inc_field_counts(stats_env, "use_time_missing_fields", names(dv))
        }
        return(NULL)
      }
      if (use_time_raw <= 0) {
        if (!is.null(stats_env)) {
          inc_stat(stats_env, "use_time_nonpositive")
          if (use_time_raw == 0) inc_stat(stats_env, "use_time_zero")
          if (use_time_raw < 0) inc_stat(stats_env, "use_time_negative")
        }
        return(NULL)
      }
      app_daily_sum <<- app_daily_sum + use_time_raw
      app_daily_count <<- app_daily_count + 1
      # useTime is in milliseconds; normalize to seconds.
      use_seconds <- use_time_raw / 1000
      if (!is.null(stats_env)) {
        inc_stat(stats_env, "use_time_positive")
        if (is.null(stats_env$use_time_pos_sum)) stats_env$use_time_pos_sum <- 0
        stats_env$use_time_pos_sum <- stats_env$use_time_pos_sum + use_time_raw
        update_minmax(stats_env, "use_time_pos_min", "use_time_pos_max", use_time_raw)
      }

      ts_candidates <- suppressWarnings(as.numeric(c(
        dv$firstUseTime, dv$lastUseTime, dv$firstFgServiceUseTime, dv$lastFgServiceUseTime
      )))
      ts_candidates <- ts_candidates[is.finite(ts_candidates) & ts_candidates > 0]
      if (length(ts_candidates) == 0) {
        if (!is.null(stats_env)) inc_stat(stats_env, "no_timestamps")
        return(NULL)
      }

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
    if (!is.null(stats_env) && length(app_complete_raw) == 1 && !is.na(app_complete_raw)) {
      inc_stat(stats_env, "app_complete_with_daily")
      if (is.null(stats_env$app_daily_sum_for_complete)) stats_env$app_daily_sum_for_complete <- 0
      stats_env$app_daily_sum_for_complete <- stats_env$app_daily_sum_for_complete + app_daily_sum
      if (app_daily_count > 0) {
        if (is.null(stats_env$app_daily_count)) stats_env$app_daily_count <- 0
        stats_env$app_daily_count <- stats_env$app_daily_count + app_daily_count
      }
    }
    do.call(rbind, out)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)

  out_rows <- do.call(rbind, rows)
  if (!is.null(stats_env)) inc_stat(stats_env, "rows_returned", nrow(out_rows))
  out_rows

}

build_corona_daily <- function(df, dataset_label, tz = "Europe/Berlin") {
  stats_env <- new.env(parent = emptyenv())
  ids_withapp <- unique(df$user_id[!is.na(df$appdata_apps) & nzchar(df$appdata_apps)])
  df <- df[df$user_id %in% ids_withapp, , drop = FALSE]

  out <- lapply(seq_len(nrow(df)), function(i) {
    parse_corona_app_daily(
      appdata_str = df$appdata_apps[i],
      participant_id = df$user_id[i],
      dataset_label = dataset_label,
      tz = tz,
      stats_env = stats_env
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
  daily_totals <- out %>%
    group_by(unique_participant_number, timestamp) %>%
    summarise(total_sec = sum(duration, na.rm = TRUE), .groups = "drop")
  if (nrow(daily_totals) > 0) {
    message(
      "Daily total (observed days) summary: ",
      "n_days=", nrow(daily_totals), ", ",
      "mean_hours=", sprintf("%.2f", mean(daily_totals$total_sec, na.rm = TRUE) / 3600), ", ",
      "median_hours=", sprintf("%.2f", median(daily_totals$total_sec, na.rm = TRUE) / 3600), ", ",
      "max_hours=", sprintf("%.2f", max(daily_totals$total_sec, na.rm = TRUE) / 3600)
    )
  }
  message(
    "Corona cleaning stats: ",
    "empty_appdata=", ifelse(is.null(stats_env$empty_appdata), 0, stats_env$empty_appdata), ", ",
    "json_parse_fail=", ifelse(is.null(stats_env$json_parse_fail), 0, stats_env$json_parse_fail), ", ",
    "no_apps=", ifelse(is.null(stats_env$no_apps), 0, stats_env$no_apps), ", ",
    "no_daily_values=", ifelse(is.null(stats_env$no_daily_values), 0, stats_env$no_daily_values), ", ",
    "use_time_missing=", ifelse(is.null(stats_env$use_time_missing), 0, stats_env$use_time_missing), ", ",
    "use_time_nonpositive=", ifelse(is.null(stats_env$use_time_nonpositive), 0, stats_env$use_time_nonpositive), ", ",
    "no_timestamps=", ifelse(is.null(stats_env$no_timestamps), 0, stats_env$no_timestamps), ", ",
    "rows_returned=", ifelse(is.null(stats_env$rows_returned), 0, stats_env$rows_returned)
  )
  if (!is.null(stats_env$use_time_total)) {
    total <- stats_env$use_time_total
    zeros <- ifelse(is.null(stats_env$use_time_zero), 0, stats_env$use_time_zero)
    negs <- ifelse(is.null(stats_env$use_time_negative), 0, stats_env$use_time_negative)
    pos <- ifelse(is.null(stats_env$use_time_positive), 0, stats_env$use_time_positive)
    pos_mean <- if (pos > 0) stats_env$use_time_pos_sum / pos else NA_real_
    message(
      "useTime summary (ms): ",
      "total=", total, ", ",
      "zero=", zeros, ", ",
      "negative=", negs, ", ",
      "positive=", pos, ", ",
      "pct_zero=", sprintf("%.2f", ifelse(total > 0, 100 * zeros / total, 0)), ", ",
      "pos_min=", ifelse(is.null(stats_env$use_time_pos_min), NA, stats_env$use_time_pos_min), ", ",
      "pos_mean=", ifelse(is.na(pos_mean), NA, sprintf("%.2f", pos_mean)), ", ",
      "pos_max=", ifelse(is.null(stats_env$use_time_pos_max), NA, stats_env$use_time_pos_max)
    )
  }
  if (!is.null(stats_env$complete_use_present)) {
    c_count <- stats_env$complete_use_present
    both_count <- ifelse(is.null(stats_env$use_and_complete_present), 0, stats_env$use_and_complete_present)
    c_sum <- ifelse(is.null(stats_env$complete_use_sum), 0, stats_env$complete_use_sum)
    u_sum <- ifelse(is.null(stats_env$use_sum_for_complete), 0, stats_env$use_sum_for_complete)
    ratio <- ifelse(u_sum > 0, c_sum / u_sum, NA_real_)
    message(
      "completeUseTime summary (ms): ",
      "present=", c_count, ", ",
      "both_present=", both_count, ", ",
      "sum_complete=", sprintf("%.2f", c_sum), ", ",
      "sum_use=", sprintf("%.2f", u_sum), ", ",
      "ratio_complete_to_use=", ifelse(is.na(ratio), NA, sprintf("%.4f", ratio))
    )
  }
  if (!is.null(stats_env$app_complete_present)) {
    app_c_sum <- ifelse(is.null(stats_env$app_complete_sum), 0, stats_env$app_complete_sum)
    app_d_sum <- ifelse(is.null(stats_env$app_daily_sum_for_complete), 0, stats_env$app_daily_sum_for_complete)
    app_ratio <- ifelse(app_d_sum > 0, app_c_sum / app_d_sum, NA_real_)
    message(
      "app-level completeUseTime vs daily sum (ms): ",
      "apps_with_complete=", ifelse(is.null(stats_env$app_complete_present), 0, stats_env$app_complete_present), ", ",
      "apps_with_complete_and_daily=", ifelse(is.null(stats_env$app_complete_with_daily), 0, stats_env$app_complete_with_daily), ", ",
      "sum_complete=", sprintf("%.2f", app_c_sum), ", ",
      "sum_daily=", sprintf("%.2f", app_d_sum), ", ",
      "ratio_complete_to_daily=", ifelse(is.na(app_ratio), NA, sprintf("%.4f", app_ratio))
    )
  }
  if (!is.null(stats_env$use_time_missing_fields)) {
    field_counts <- stats_env$use_time_missing_fields
    field_counts <- sort(unlist(field_counts), decreasing = TRUE)
    top_fields <- names(field_counts)[seq_len(min(10, length(field_counts)))]
    message(
      "Top fields when useTime missing: ",
      paste(paste0(top_fields, "=", field_counts[top_fields]), collapse = ", ")
    )
  }
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

mean(stress_b_apps_daily$duration)

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

############# Corona Heart (Raw Sensor Data) #############
# Parse app durations from raw sensordata JSON files in Rohdaten_heart.
corona_heart_raw_dir <- "/Users/f007qrc/projects/ManyApps_Data/Corona Health/Rohdaten_heart"
corona_heart_raw_files <- list.files(
  corona_heart_raw_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

parse_corona_sensordata <- function(sensordata_str, participant_id, dataset_label, tz = "Europe/Berlin") {
  empty <- data.frame(
    participant_number = character(0),
    Package_name = character(0),
    duration = numeric(0),
    timestamp = as.Date(character(0)),
    Dataset = character(0),
    stringsAsFactors = FALSE
  )

  if (is.na(sensordata_str) || !nzchar(sensordata_str)) {
    return(empty)
  }

  sensor_json <- tryCatch(
    jsonlite::fromJSON(sensordata_str, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(sensor_json) || length(sensor_json) == 0) {
    return(empty)
  }

  rows <- lapply(sensor_json, function(item) {
    if (is.null(item$apps) || length(item$apps) == 0) return(NULL)
    apps <- item$apps
    app_rows <- lapply(apps, function(app) {
      app_name <- if (!is.null(app$packageName)) app$packageName else NA_character_
      daily_vals <- app$dailyValues
      if (is.null(daily_vals) || length(daily_vals) == 0) return(NULL)

      dv_rows <- lapply(daily_vals, function(dv) {
        use_time_raw <- suppressWarnings(as.numeric(dv$useTime))
        if (length(use_time_raw) != 1 || is.na(use_time_raw) || use_time_raw <= 0) return(NULL)

        # useTime appears to be in milliseconds; normalize to seconds.
        use_seconds <- use_time_raw / 1000

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

      dv_rows <- Filter(Negate(is.null), dv_rows)
      if (length(dv_rows) == 0) return(NULL)
      do.call(rbind, dv_rows)
    })

    app_rows <- Filter(Negate(is.null), app_rows)
    if (length(app_rows) == 0) return(NULL)
    do.call(rbind, app_rows)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(empty)
  do.call(rbind, rows)
}

corona_heart_raw_list <- lapply(corona_heart_raw_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA", "NULL"))
  if (!"sensordata" %in% names(df)) {
    message("Skipping file (no sensordata): ", f)
    return(NULL)
  }

  participant_col <- if ("user_id" %in% names(df)) "user_id" else if ("participant_number" %in% names(df)) "participant_number" else NULL
  if (is.null(participant_col)) {
    message("Skipping file (no participant id column): ", f)
    return(NULL)
  }

  out <- lapply(seq_len(nrow(df)), function(i) {
    parse_corona_sensordata(
      sensordata_str = df$sensordata[i],
      participant_id = df[[participant_col]][i],
      dataset_label = "Corona_Heart_Raw",
      tz = "Europe/Berlin"
    )
  })

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(NULL)
  bind_rows(out)
})

corona_heart_raw_apps <- bind_rows(corona_heart_raw_list)
if (nrow(corona_heart_raw_apps) > 0) {
  corona_heart_raw_apps$unique_participant_number <- paste0(
    substr(corona_heart_raw_apps$Dataset, 1, 3),
    "_",
    corona_heart_raw_apps$participant_number
  )

  write.csv(
    corona_heart_raw_apps,
    "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_apps_raw.csv",
    row.names = FALSE
  )

  # ----------------------------
  # Compare raw vs daily outputs
  # ----------------------------
  if (exists("heart_apps_daily") && nrow(heart_apps_daily) > 0) {
    raw_df <- corona_heart_raw_apps %>%
      mutate(
        day = as.Date(timestamp),
        participant_number = as.character(participant_number),
        Package_name = as.character(Package_name)
      )

    daily_df <- heart_apps_daily %>%
      mutate(
        day = as.Date(timestamp),
        participant_number = as.character(participant_number),
        Package_name = as.character(Package_name)
      )

    # 1) Participant-day totals
    raw_participant_day <- raw_df %>%
      group_by(participant_number, day) %>%
      summarise(raw_total_sec = sum(duration, na.rm = TRUE), .groups = "drop")

    daily_participant_day <- daily_df %>%
      group_by(participant_number, day) %>%
      summarise(daily_total_sec = sum(duration, na.rm = TRUE), .groups = "drop")

    participant_day_compare <- full_join(
      raw_participant_day,
      daily_participant_day,
      by = c("participant_number", "day")
    ) %>%
      mutate(
        raw_total_sec = ifelse(is.na(raw_total_sec), 0, raw_total_sec),
        daily_total_sec = ifelse(is.na(daily_total_sec), 0, daily_total_sec),
        diff_sec = raw_total_sec - daily_total_sec,
        ratio_raw_to_daily = ifelse(daily_total_sec > 0, raw_total_sec / daily_total_sec, NA_real_)
      )

    # 2) App-day totals
    raw_app_day <- raw_df %>%
      group_by(Package_name, day) %>%
      summarise(raw_total_sec = sum(duration, na.rm = TRUE), .groups = "drop")

    daily_app_day <- daily_df %>%
      group_by(Package_name, day) %>%
      summarise(daily_total_sec = sum(duration, na.rm = TRUE), .groups = "drop")

    app_day_compare <- full_join(
      raw_app_day,
      daily_app_day,
      by = c("Package_name", "day")
    ) %>%
      mutate(
        raw_total_sec = ifelse(is.na(raw_total_sec), 0, raw_total_sec),
        daily_total_sec = ifelse(is.na(daily_total_sec), 0, daily_total_sec),
        diff_sec = raw_total_sec - daily_total_sec,
        ratio_raw_to_daily = ifelse(daily_total_sec > 0, raw_total_sec / daily_total_sec, NA_real_)
      )

    # 3) Coverage checks
    coverage <- data.frame(
      metric = c(
        "participants_raw",
        "participants_daily",
        "days_raw",
        "days_daily",
        "participant_days_raw",
        "participant_days_daily",
        "apps_raw",
        "apps_daily",
        "app_days_raw",
        "app_days_daily"
      ),
      value = c(
        length(unique(raw_df$participant_number)),
        length(unique(daily_df$participant_number)),
        length(unique(raw_df$day)),
        length(unique(daily_df$day)),
        nrow(raw_participant_day),
        nrow(daily_participant_day),
        length(unique(raw_df$Package_name)),
        length(unique(daily_df$Package_name)),
        nrow(raw_app_day),
        nrow(daily_app_day)
      ),
      stringsAsFactors = FALSE
    )

    write.csv(
      participant_day_compare,
      "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_compare_participant_day.csv",
      row.names = FALSE
    )
    write.csv(
      app_day_compare,
      "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_compare_app_day.csv",
      row.names = FALSE
    )
    write.csv(
      coverage,
      "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_compare_coverage.csv",
      row.names = FALSE
    )

    message(
      "Heart raw vs daily comparison written to: ",
      "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_compare_participant_day.csv, ",
      "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_compare_app_day.csv, ",
      "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/heart_compare_coverage.csv"
    )
  }
}


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
    timestamp = as.POSIXct(session_start, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Berlin"),
    session_end = as.POSIXct(session_end, format = "%Y-%m-%d %H:%M:%OS", tz = "Europe/Berlin"),
    duration = as.numeric(difftime(session_end, timestamp, units = "secs")),
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

moodylife$duration = moodylife$duration * 60 # times 60 because seems to be in minutes

colnames(moodylife)

top20_packages <- moodylife %>%
  group_by(Package_name) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) %>%
  arrange(desc(total_duration)) %>%
  slice_head(n = 20)

top20_packages

write.csv(moodylife,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/moodylife_apps.csv")

length(unique(moodylife$participant_number))




###### Ramona [whale] ########

whale <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Ramona/data_sessions_raw.csv")

colnames(whale)

whale$participant_number <- whale$user
whale$Package_name <- whale$package_no
whale$participant_number <- whale$user

library(dplyr)
library(lubridate)

whale <- whale %>%
  mutate(
    participant_number = user,
    Package_name = package_no
  )

studyday_dates <- whale %>%
  distinct(studyday_shuffled, timestamp_year, weekday) %>%
  mutate(
    jan1 = ymd(paste0(timestamp_year, "-01-01")),
    target_wday = match(weekday, c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
    days_to_add = (target_wday - wday(jan1)) %% 7,
    date_fixed = jan1 + days(days_to_add)
  ) %>%
  select(studyday_shuffled, date_fixed) %>%
  distinct(studyday_shuffled, .keep_all = TRUE)

whale <- whale %>%
  mutate(
    timestamp_hour_fixed = if_else(timestamp_hour == 24, 0, timestamp_hour)
  ) %>%
  left_join(studyday_dates, by = "studyday_shuffled") %>%
  mutate(
    timestamp = date_fixed + hours(timestamp_hour_fixed)
  ) %>%
  select(
    participant_number,
    Package_name,
    duration,
    timestamp,
    Dataset,
    studyday_shuffled
  )

whale <- whale[colnames(whale) %in% c("participant_number","Package_name","duration","timestamp","Dataset" )]


whale$unique_participant_number <- paste0(
  substr(whale$Dataset, 1, 3),
  "_",
  whale$participant_number
)
whale$timestamp <- format(whale$timestamp, "%Y-%m-%d %H:%M:%S")



write.csv(whale,"/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/whale_apps.csv")


