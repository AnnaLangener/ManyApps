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
2.  **Merge demographics**
    -   `Data Cleaning/Demographics.R`
    -   Joins demographic variables to cleaned app data.
3.  **Combine and aggregate**
    -   `Data Cleaning/Combine_Data.R`
    -   Combines all cleaned CSVs and produces aggregated outputs.

Other scripts

-   `Data Cleaning/ramona_appcleaning.R`
    -   shortened version of Combine_Data.R
-   `Data Cleaning/behapp_stuff.R`
    -   used for Behapp data cleaning

## [To do: add main outputs]

## `Data Analyses/`

-   Visualization and summary scripts built on cleaned outputs [to do: add more]

## 
