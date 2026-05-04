# ManyApps

R scripts for cleaning and analyzing multi-study app-usage datasets.

The data itself is **not** stored here. Scripts read from a local data directory and write cleaned outputs back to that same location.

## Structure

### **`Data Cleaning/`**

-   Dataset-specific cleaning and preprocessing scripts.
-   Demographics joins and filtering steps.
-   Combined datasets and hourly aggregation.

## Workflow for Data Cleaning

1.  **Clean each dataset**
    -   `Data Cleaning/DataCleaning.R`
    -   Reads "raw" app usage datasets, standardizes columns, and exports cleaned CSVs.
    -   NOTE: For WHALE study we changed date to a random date that still matches weekday and year
2.  **Merge demographics**
    -   `Data Cleaning/Demographics.R`
    -   Joins demographic variables to cleaned app data.
3.  **Combine and aggregate**
    -   `Data Cleaning/Combine_Data.R`
    -   Combines all cleaned CSVs and produces aggregated outputs.

Other scripts

-   `Data Cleaning/behapp_stuff.R`
    -   used for Behapp data cleaning

## Main Outputs Data Cleaning (see Combine_Data.R):

### Hourly and Daily Total App Usage (RQ 1-3) `manyapps_hourly_noapp`

This dataset summarizes total smartphone usage without app-level detail, derived from cleaned and processed app event data.

Includes only participants and days passing preregistered quality criteria: - Age ≥ 18 - First 14 days per participant - ≥ 50% observed days AND ≥ 7 valid days - System apps removed - Duplicate and zero-duration events removed

#### 1. Hourly aggregation

App usage is summed across all apps per participant and hour:

\- `total_hourly_app_usage` = sum of app usage within hour

\- Capped at 60 minutes per hour - If no hourly data exists (daily-only datasets), values are `NA` (this was the case for corona datasets)

#### 2. Daily totals

Daily usage per participant is computed as:

\- Sum of hourly usage (if available), OR - Sum of daily app usage (for daily-only datasets)

\- Capped at 24 hours per day

#### 3. Study averages

-   `study_average_daily_app_usage` = mean daily usage across observed days

#### 4. App diversity

-   `unique_apps_hour`: number of apps used per hour
-   `unique_apps_day`: number of apps used per day
-   `unique_apps_overall`: total apps used per participant

#### 5. Zero-usage hours

For all days except the first: - Missing hours are filled to create full 24-hour coverage - Usage is set to 0 where no activity is recorded

### Key columns

-   `total_hourly_app_usage`: total usage per hour (seconds, capped) [per participant × day × hour]
-   `total_daily_app_usage`: total usage per day (seconds, capped) [per participant × day]
-   `study_average_daily_app_usage`: average daily usage across observed days (seconds) [per participant]
-   `unique_apps_hour`: number of apps used in an hour [per participant × day × hour]
-   `unique_apps_day`: number of apps used in a day [per participant × day]
-   `unique_apps_overall`: total distinct apps used [per participant]
-   `hour_start`: timestamp for each hour [per participant × day × hour]
-   demographic variables: `age`, `gender`, `age_group`, `country`, etc.

## Notes

-   Usage is capped at 60 min/hour and 24 hours/day.
-   Missing hourly data is treated as:
    -   `NA` for daily-only datasets
    -   `0` for filled hours in observed days
-   Results reflect total usage behavior, not app-specific patterns.

### Top Used Apps (RQ 4)

For each participant, daily app usage is summed by app. Each app’s share is calculated as:

`app_share_person = app_minutes (on specific app) / total_minutes_all_apps`

App shares are computed at the participant level and then aggregated, giving equal weight to each participant rather than weighting by total usage.

Shares are then summed within each grouping and app. The final ranking selects the top 10 apps per group.

-   top_apps_by_agegroup.csv: Top 10 apps within each `age_group`.
-   top_apps_by_gender.csv: Top 10 apps within each `gender`.
-   top_apps_by_sample.csv:Top 10 apps within each `Dataset`.
-   top_apps_by_country.csv:Top 10 apps within each `country`.

## Output columns

-   `Package_name`: app package name
-   grouping column: `age_group`, `gender`, `Dataset`, or `country`
-   `share`: summed participant-level app share
-   `n_people`: number of participants contributing to the app/group
-   `rank`: rank within group
-   `mean_share`: average participant-level share, calculated as `share / n_people`

Notes: **WHALE study does not include this information. So we ignore it for RQ4.** Usage is based on total_daily_app_usage. Missing values are ignored when summing minutes.

## `Data Analyses/`

-   Visualization and summary scripts built on cleaned outputs [to do: add more]

## 
