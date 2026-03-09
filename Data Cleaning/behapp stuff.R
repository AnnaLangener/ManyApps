
library(data.table)
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(jsonlite)

demo <- read.csv('/Users/f007qrc/projects/ManyApps_Data/Behapp/Demographics.csv')

id1 <- read.csv('/Users/f007qrc/projects/ManyApps_Data/Behapp/Behapp_ID__RecodedID.csv')
id2 <- read.csv('/Users/f007qrc/projects/ManyApps_Data/Behapp/PRISM2_ID__RecodedID.csv')


##### Behapp IDs
behapp_folder <- "/Users/f007qrc/projects/ManyApps_Data/Behapp/anna_export"
behapp_files <- sort(list.files(behapp_folder, pattern = "\\.csv$", full.names = TRUE))

behapp_list <- lapply(behapp_files, function(f) {
  dt <- fread(f, na.strings = c("", "NA"), fill = TRUE)
  
  # Drop index column created during export
  if ("V1" %in% names(dt)) {
    dt[, V1 := NULL]
  }
  
  dt
})

behapp_raw <- rbindlist(behapp_list, use.names = TRUE, fill = TRUE)

behapp_apps <- behapp_raw %>%
  transmute(
    participant_number = as.character(participant_number),
    Package_name = package_name,
    duration = suppressWarnings(as.numeric(duration)) / 1000,
    timestamp = suppressWarnings(ymd_hms(hour_start, tz = "Europe/Berlin")),
    Dataset = if ("Dataset" %in% names(behapp_raw)) paste0("Behapp_", as.character(Dataset)) else "Behapp"
  )



id1

merged = merge(behapp_apps, id1,
      by.x = "participant_number",
      by.y = "Behapp_ID")


merged = merge(merged, id2,
               by.x = "RecodedID",
               by.y = "RecodedID")

merged = merge(merged, demo,
               by.x = "PRISM2_ID",
               by.y = "SUBJECT.ID")



Behapp <- merged[
  merged$GRPID %in% c("Major Depressive Disorder", "Healthy Control"),
]

Behapp <- Behapp %>%
  mutate(
    participant_number = as.character(participant_number),
    age = suppressWarnings(as.numeric(as.character(AGE))),
    gender_raw = toupper(trimws(as.character(SEX))),
    Dataset = as.character(Dataset)
  ) %>%
  mutate(
    gender = case_when(
      gender_raw == "M" ~ "male",
      gender_raw == "F" ~ "female",
      TRUE ~ "other"
    ),
    unique_participant_number = paste0(substr(Dataset, 1, 3), "_", participant_number)
  ) %>%
  select(participant_number, age, gender, Dataset,
         unique_participant_number, timestamp,
         duration, Package_name, SITE)

test = Behapp %>%
  group_by(participant_number) %>%
  summarise(total_duration = sum(duration, na.rm = TRUE)) 


length(unique(Behapp$participant_number))

Behapp$PANAS_POS = NA
Behapp$PANAS_NEG = NA
Behapp$PHQ = NA
Behapp$STRESS = NA
Behapp$SWLS = NA

Behapp <- Behapp %>%
  mutate(
    country = case_when(
      SITE %in% c("La Princesa", "Gregorio Maranon") ~ "Spain",
      SITE %in% c("Leiden (LUMC)", "Amsterdam (VUMC)") ~ "Netherlands",
      TRUE ~ NA_character_
    )
  )

Behapp <- Behapp %>%
  select(-SITE)


write.csv(
  Behapp,
  "/Users/f007qrc/projects/ManyApps_Data/Cleaned_Apps/all/Behapp_with_dem.csv",
  row.names = FALSE
)


