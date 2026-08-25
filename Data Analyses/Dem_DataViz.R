library(dplyr)
library(ggplot2)
library(lubridate)
library(psych)
library(readr)
library(gtsummary)
library(skimr)

######### Load Datasets ###########
top_apps_by_agegroup <- read.csv("/Users/f007qrc/projects/ManyApps_Data/top_apps_by_agegroup.csv")
top_apps_by_gender <- read.csv("/Users/f007qrc/projects/ManyApps_Data/top_apps_by_gender.csv")
top_apps_by_sample <- read.csv("/Users/f007qrc/projects/ManyApps_Data/top_apps_by_sample.csv")


manyapps_hourly_noapp <- read_csv(
  "/Users/f007qrc/projects/ManyApps_Data/Final_noapp_overview.csv",
)[-1]

length(unique(manyapps_hourly_noapp$unique_participant_number))

###############################

library(lme4)
library(dplyr)
daily_icc_data <- manyapps_hourly_noapp %>%
  group_by(Dataset, unique_participant_number, day) %>%
  summarise(
    daily_use_hours = first(
      total_daily_app_usage[!is.na(total_daily_app_usage)],
      default = NA_real_
    ) / 3600,
    daily_number_apps = first(
      unique_apps_day[!is.na(unique_apps_day)],
      default = NA_real_
    ),
    .groups = "drop"
  )

icc_use_model <- lmer(
  daily_use_hours ~ 1 + (1 | unique_participant_number),
  data = daily_icc_data,
  REML = TRUE,
  na.action = na.omit
)

icc_apps_model <- lmer(
  daily_number_apps ~ 1 + (1 | unique_participant_number),
  data = daily_icc_data,
  REML = TRUE,
  na.action = na.omit
)

extract_icc <- function(model, outcome) {
  
  variance_components <- as.data.frame(VarCorr(model))
  
  between_variance <- variance_components %>%
    filter(grp == "unique_participant_number") %>%
    pull(vcov)
  
  within_variance <- variance_components %>%
    filter(grp == "Residual") %>%
    pull(vcov)
  
  tibble(
    outcome = outcome,
    between_person_variance = between_variance,
    within_person_variance = within_variance,
    total_variance = between_variance + within_variance,
    ICC = between_variance / total_variance,
    between_person_percent = 100 * ICC,
    within_person_percent = 100 * (1 - ICC)
  )
}

icc_results <- bind_rows(
  extract_icc(icc_use_model, "Daily use duration"),
  extract_icc(icc_apps_model, "Daily number of apps")
)

icc_results
############################################################
########### Basic Descriptive (Sample & Procedure) #########
############################################################

participant_df <- manyapps_hourly_noapp %>%
  group_by(unique_participant_number) %>%
  summarise(
    country = first(na.omit(country)),
    Dataset = first(na.omit(Dataset)),
    gender = first(na.omit(gender)),
    age = first(na.omit(age)),
    age_group = first(na.omit(age_group)),
    PHQ = first(na.omit(PHQ)),
    PANAS_POS = first(na.omit(PANAS_POS)),
    PANAS_NEG = first(na.omit(PANAS_NEG)),
    STRESS = first(na.omit(STRESS)),
    SWLS = first(na.omit(SWLS)),
    study_average_daily_app_usage = first(na.omit(study_average_daily_app_usage)),
    unique_apps_overall = first(na.omit(unique_apps_overall)),
    n_hourly_rows = n(),
    n_days = as.numeric(n_distinct(day)),
    mean_daily_app_usage = mean(total_daily_app_usage, na.rm = TRUE),
    mean_hourly_app_usage = mean(total_hourly_app_usage, na.rm = TRUE),
    mean_unique_apps_day = mean(unique_apps_day, na.rm = TRUE)
  ) %>%
  ungroup()

mean(participant_df$n_days)
sd(participant_df$n_days)

des <- participant_df %>%
  select(
    gender,
    age,
    age_group,
    n_days,
    country
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    missing = "ifany"
  ) %>%
  add_overall() %>%
  bold_labels()

des

mean(participant_df$age, na.rm = T)
sd(participant_df$age, na.rm = T)

sum(!is.na(participant_df$PANAS_POS))
sum(!is.na(participant_df$STRESS))
sum(!is.na(participant_df$SWLS))
sum(!is.na(participant_df$PHQ))

############################################################
################## Research Question 1 #####################
############################################################

# 1. Reduce to one row per participant-day
daily_use <- manyapps_hourly_noapp %>%
  distinct(unique_participant_number, day, total_daily_app_usage)

# 2. Compute person-specific daily-use summaries
participant_daily_stats <- daily_use %>%
  group_by(unique_participant_number) %>%
  summarise(
    n_days = n(),
    person_mean_daily_use = mean(total_daily_app_usage, na.rm = TRUE)/60/60,
    person_sd_daily_use = sd(total_daily_app_usage, na.rm = TRUE)/60/60,
    .groups = "drop"
  )

# 3. RQ1a: overall mean and SD of person-specific average daily smartphone use
rq1a_summary <- participant_daily_stats %>%
  summarise(
    n_participants = n(),
    mean_person_mean_daily_use = mean(person_mean_daily_use, na.rm = TRUE),
    median_person_mean_daily_use = median(person_mean_daily_use, na.rm = TRUE),
    sd_person_mean_daily_use = sd(person_mean_daily_use, na.rm = TRUE)
  )

rq1a_summary

# 4. RQ1b: within-person variation
# global mean and SD of each participant's within-person SD
rq1b_within_person_summary <- participant_daily_stats %>%
  summarise(
    n_participants_with_sd = sum(!is.na(person_sd_daily_use)),
    mean_within_person_sd = mean(person_sd_daily_use, na.rm = TRUE),
    sd_within_person_sd = sd(person_sd_daily_use, na.rm = TRUE)
  )

rq1b_within_person_summary

library(tidyverse)
library(patchwork)
library(hrbrthemes)

mean_use <- rq1a_summary$mean_person_mean_daily_use


base_col <- "#213547"
accent_col <- "#5B8E7D"
soft_col <- "#B8D8D8"
warm_col <- "#E7A977"
rose_col <- "#D97B66"
grid_col <- "#D9E1E5"
bg_col <- "white"

cat_cols <- c(
  "female" = "#5B8E7D",
  "male" = "#213547",
  "other" = "#B8A16B",
  "Weekday" = "#D97B66",
  "Weekend" = "#E7A977"
)
max_count_mean <- ggplot_build(
  ggplot(participant_daily_stats, aes(x = person_mean_daily_use)) +
    geom_histogram(bins = 40)
)$data[[1]]$count |> max()

p_mean <- participant_daily_stats %>%
  ggplot(aes(x = person_mean_daily_use)) +
  geom_histogram(
    bins = 40,
    fill = accent_col,
    color = "white"
  ) +
  geom_vline(
    xintercept = mean_use,
    color = base_col,
    linewidth = 1
  ) +
  annotate(
    "curve",
    x = mean_use + 2,
    xend = mean_use,
    y = max_count_mean * .85,
    yend = max_count_mean * .70,
    curvature = -0.25,
    arrow = arrow(length = unit(0.15, "inches")),
    color = base_col,
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = mean_use + 2,
    y = max_count_mean * .88,
    label = paste0(
      "Participants used their\nsmartphones on average\n",
      round(mean_use, 1),
      " hours per day"
    ),
    hjust = 0,
    color = base_col,
    fontface = "bold",
    size = 3.5
  ) +
  labs(
    title = "Average Daily Use Duration",
    x = "Person-specific mean daily use (hours)",
    y = "Number of participants"
  ) +
  theme_ipsum(base_size = 10, axis_title_size = 12)

mean_sd <- rq1b_within_person_summary$mean_within_person_sd

max_count_sd <- ggplot_build(
  ggplot(
    participant_daily_stats %>% filter(!is.na(person_sd_daily_use)),
    aes(x = person_sd_daily_use)
  ) +
    geom_histogram(bins = 40)
)$data[[1]]$count |> max()

p_sd <- participant_daily_stats %>%
  filter(!is.na(person_sd_daily_use)) %>%
  ggplot(aes(x = person_sd_daily_use)) +
  geom_histogram(
    bins = 40,
    fill = warm_col,
    color = "white"
  ) +
  geom_vline(
    xintercept = mean_sd,
    color = base_col,
    linewidth = 1
  ) +
  annotate(
    "curve",
    x = mean_sd + 1,
    xend = mean_sd,
    y = max_count_sd * .85,
    yend = max_count_sd * .70,
    curvature = -0.25,
    arrow = arrow(length = unit(0.15, "inches")),
    color = base_col,
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = mean_sd + 1,
    y = max_count_sd * .88,
    label = paste0(
      "Participants' smartphone use\nvaried by about ",
      round(mean_sd, 1),
      " hours\nfrom one day to the next"
    ),
    hjust = 0,
    color = base_col,
    fontface = "bold",
    size = 3.5
  ) +
  labs(
    title = "Within-Person Variation",
    x = "Person-specific SD of daily use (hours)",
    y = "Number of participants"
  ) +
  theme_ipsum(base_size = 10, axis_title_size = 12)

p_rq1 <- p_mean + p_sd 

p_rq1

ggsave(
  filename = "Figures/RQ1_daily_smartphone_use_distribution.jpg",
  plot = p_rq1,
  width = 9,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "white"
)

############################################################
################## Research Question 2 #####################
############################################################
# 1. Reduce to one row per participant-day
daily_use <- manyapps_hourly_noapp %>%
  distinct(unique_participant_number, day, unique_apps_day)

# 2. Compute person-specific daily-use summaries
participant_daily_stats <- daily_use %>%
  group_by(unique_participant_number) %>%
  summarise(
    n_days = n(),
    person_mean_daily_use = mean(unique_apps_day, na.rm = TRUE),
    person_sd_daily_use = sd(unique_apps_day, na.rm = TRUE),
    .groups = "drop"
  )

# 3. RQ1a: overall mean and SD of person-specific average daily smartphone use
rq1a_summary <- participant_daily_stats %>%
  summarise(
    n_participants = n(),
    mean_person_mean_daily_use = mean(person_mean_daily_use, na.rm = TRUE),
    sd_person_mean_daily_use = sd(person_mean_daily_use, na.rm = TRUE)
  )

rq1a_summary

# 4. RQ1b: within-person variation
# global mean and SD of each participant's within-person SD
rq1b_within_person_summary <- participant_daily_stats %>%
  summarise(
    n_participants_with_sd = sum(!is.na(person_sd_daily_use)),
    mean_within_person_sd = mean(person_sd_daily_use, na.rm = TRUE),
    sd_within_person_sd = sd(person_sd_daily_use, na.rm = TRUE)
  )

rq1b_within_person_summary


mean_apps <- rq1a_summary$mean_person_mean_daily_use
mean_apps_sd <- rq1b_within_person_summary$mean_within_person_sd

max_count_mean <- ggplot_build(
  ggplot(participant_daily_stats, aes(x = person_mean_daily_use)) +
    geom_histogram(bins = 40)
)$data[[1]]$count |> max()

max_count_sd <- ggplot_build(
  ggplot(
    participant_daily_stats %>% filter(!is.na(person_sd_daily_use)),
    aes(x = person_sd_daily_use)
  ) +
    geom_histogram(bins = 40)
)$data[[1]]$count |> max()

p_mean <- participant_daily_stats %>%
  ggplot(aes(x = person_mean_daily_use)) +
  geom_histogram(bins = 40, fill = accent_col, color = "white") +
  geom_vline(xintercept = mean_apps, color = base_col, linewidth = 1) +
  annotate(
    "curve",
    x = mean_apps + 8,
    xend = mean_apps,
    y = max_count_mean * .85,
    yend = max_count_mean * .70,
    curvature = -0.25,
    arrow = arrow(length = unit(0.15, "inches")),
    color = base_col,
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = mean_apps + 8,
    y = max_count_mean * .90,
    label = paste0(
      "Participants used on average\n",
      round(mean_apps, 1),
      " unique apps per day"
    ),
    hjust = 0,
    color = base_col,
    fontface = "bold",
    size = 3.3
  ) +
  labs(
    title = "Average Daily Number of Apps",
    x = "Person-specific mean number of apps",
    y = "Number of participants"
  ) +
  theme_ipsum(base_size = 10, axis_title_size = 12)

p_sd <- participant_daily_stats %>%
  filter(!is.na(person_sd_daily_use)) %>%
  ggplot(aes(x = person_sd_daily_use)) +
  geom_histogram(bins = 40, fill = warm_col, color = "white") +
  geom_vline(xintercept = mean_apps_sd, color = base_col, linewidth = 1) +
  annotate(
    "curve",
    x = mean_apps_sd + 4,
    xend = mean_apps_sd,
    y = max_count_sd * .85,
    yend = max_count_sd * .70,
    curvature = -0.25,
    arrow = arrow(length = unit(0.15, "inches")),
    color = base_col,
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = mean_apps_sd + 4,
    y = max_count_sd * .90,
    label = paste0(
      "Daily app diversity fluctuated\nby an average of ",
      round(mean_apps_sd, 1),
      " apps"
    ),
    hjust = 0,
    color = base_col,
    fontface = "bold",
    size = 3.3
  ) +
  labs(
    title = "Within-Person Variation",
    x = "Person-specific SD of daily number of apps",
    y = "Number of participants"
  ) +
  theme_ipsum(base_size = 10, axis_title_size = 12)

p_rq1_apps <- p_mean + p_sd
p_rq1_apps

ggsave(
  filename = "Figures/RQ2_daily_app_diversity_distribution.jpg",
  plot = p_rq1_apps,
  width = 9,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "white"
)

p_rq1_apps <- p_mean + p_sd

combined = p_rq1 / p_rq1_apps

ggsave(
  filename = "Figures/RQ1_RQ2.jpg",
  plot = combined,
  width = 9,
  height = 6.5,
  units = "in",
  dpi = 300,
  bg = "white"
)


############################################################
################ Research Question 3 / 4 ####################
############################################################
############################################################
######## WELL-BEING: DESCRIPTIVE + LOCATION + SCALE #########
############################################################

library(tidyverse)
library(hrbrthemes)
library(patchwork)


# ============================================================
# 0. VARIABLE NAME
# ============================================================

UNIQUE_APPS_VAR <- "unique_apps_day"


# ============================================================
# 1. COLORS
#
# ONE COLOR PER OUTCOME
# ============================================================

bg_col <- "white"

base_col <- "#17324D"

# Smartphone use
usage_col <- "#17324D"

# Unique apps
apps_col <- "#E58E7E"

grid_col <- "#D9E1E5"

bg_col <- "white"
base_col <- "#17324D"
grid_col <- "#D9E1E5"

construct_cols <- c(
  "Positive\nAffect"       = "#83AF9B",
  "Negative\nAffect"       = "#E58E7E",
  "Life\nSatisfaction"     = "#CDB67C",
  "Depressive\nSymptoms"   = "#536878"
)

construct_cols_model <- c(
  "Positive Affect"      = "#83AF9B",
  "Negative Affect"      = "#E58E7E",
  "Life Satisfaction"    = "#CDB67C",
  "Depressive Symptoms"  = "#536878"
)
# ============================================================
# 2. HELPERS
# ============================================================

first_non_na <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA)
  }
  
  x[1]
}


safe_mean <- function(x) {
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}

# ============================================================
# 3. PARTICIPANT-LEVEL SMARTPHONE USE + WELL-BEING
# ============================================================

rq3_participant <- manyapps_hourly_noapp %>%
  
  group_by(
    Dataset,
    unique_participant_number
  ) %>%
  
  summarise(
    
    smartphone_hours =
      first_non_na(
        study_average_daily_app_usage
      ) / 3600,
    
    PANAS_POS =
      as.numeric(
        first_non_na(PANAS_POS)
      ),
    
    PANAS_NEG =
      as.numeric(
        first_non_na(PANAS_NEG)
      ),
    
    SWLS =
      as.numeric(
        first_non_na(SWLS)
      ),
    
    PHQ =
      as.numeric(
        first_non_na(PHQ)
      ),
    
    .groups = "drop"
  )


# ============================================================
# 4. PARTICIPANT-LEVEL UNIQUE APPS
# ============================================================

participant_apps <- manyapps_hourly_noapp %>%
  
  group_by(
    Dataset,
    unique_participant_number,
    day
  ) %>%
  
  summarise(
    
    unique_apps =
      as.numeric(
        first_non_na(
          .data[[UNIQUE_APPS_VAR]]
        )
      ),
    
    .groups = "drop"
  ) %>%
  
  group_by(
    Dataset,
    unique_participant_number
  ) %>%
  
  summarise(
    
    unique_apps =
      safe_mean(unique_apps),
    
    .groups = "drop"
  )


# ============================================================
# 5. JOIN
# ============================================================

rq3_participant <- rq3_participant %>%
  
  left_join(
    participant_apps,
    by = c(
      "Dataset",
      "unique_participant_number"
    )
  )


# ============================================================
# 6. WELL-BEING LONG FORMAT
# ============================================================

rq3_cont <- rq3_participant %>%
  
  pivot_longer(
    
    cols = c(
      PANAS_POS,
      PANAS_NEG,
      SWLS,
      PHQ
    ),
    
    names_to = "Construct",
    values_to = "Score"
  ) %>%
  
  mutate(
    
    Construct = factor(
      
      Construct,
      
      levels = c(
        "PANAS_POS",
        "PANAS_NEG",
        "SWLS",
        "PHQ"
      ),
      
      labels = c(
        "Positive\nAffect",
        "Negative\nAffect",
        "Life\nSatisfaction",
        "Depressive\nSymptoms"
      )
    )
  )


# ============================================================
# 7. COMMON THEME
# ============================================================

common_theme <- theme_ipsum(
  base_size = 11,
  axis_title_size = 12
) +
  
  theme(
    
    plot.background =
      element_rect(
        fill = bg_col,
        color = NA
      ),
    
    panel.background =
      element_rect(
        fill = bg_col,
        color = NA
      ),
    
    panel.grid.minor =
      element_blank(),
    
    panel.grid.major =
      element_line(
        color = grid_col,
        linewidth = 0.30
      ),
    
    strip.background =
      element_rect(
        fill = "white",
        color = NA
      ),
    
    strip.text =
      element_text(
        face = "bold",
        size = 10,
        color = base_col
      ),
    
    axis.text =
      element_text(
        color = base_col,
        size = 8
      ),
    
    axis.title =
      element_text(
        color = base_col,
        size = 12
      ),
    
    plot.title =
      element_text(
        color = base_col,
        face = "bold",
        size = 11
      ),
    
    plot.subtitle =
      element_text(
        size = 8.5,
        color = "grey40"
      )
  )

# ============================================================
# 8. DESCRIPTIVE SMARTPHONE-USE PLOT
# ============================================================

p_usage <- ggplot(
  rq3_cont,
  aes(
    x = Score,
    y = smartphone_hours,
    color = Construct,
    fill = Construct
  )
) +
  
  geom_point(
    alpha = 0.22,
    size = 0.55
  ) +
  
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = base_col,
    linewidth = 1.05
  ) +
  
  facet_wrap(
    ~Construct,
    scales = "free_x",
    nrow = 1
  ) +
  
  scale_color_manual(
    values = construct_cols
  ) +
  
  scale_fill_manual(
    values = scales::alpha(
      construct_cols,
      0.14
    )
  ) +
  
  scale_x_continuous(
    labels = function(x) {
      out <- sub(
        "\\.?0+$",
        "",
        formatC(
          x,
          format = "f",
          digits = 2
        )
      )
      out <- sub("^0\\.", ".", out)
      out <- sub("^-0\\.", "-.", out)
      out
    }
  ) +
  
  labs(
    title = "Average daily smartphone use",
    x = NULL,
    y = NULL
  ) +
  
  common_theme +
  
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )


# ============================================================
# 9. DESCRIPTIVE UNIQUE-APPS PLOT
# ============================================================

p_apps <- ggplot(
  rq3_cont,
  aes(
    x = Score,
    y = unique_apps,
    color = Construct,
    fill = Construct
  )
) +
  
  geom_point(
    alpha = 0.22,
    size = 0.55
  ) +
  
  geom_smooth(
    method = "loess",
    se = TRUE,
    color = base_col,
    linewidth = 1.05
  ) +
  
  facet_wrap(
    ~Construct,
    scales = "free_x",
    nrow = 1
  ) +
  
  scale_color_manual(
    values = construct_cols
  ) +
  
  scale_fill_manual(
    values = scales::alpha(
      construct_cols,
      0.14
    )
  ) +
  
  scale_x_continuous(
    labels = function(x) {
      out <- sub(
        "\\.?0+$",
        "",
        formatC(
          x,
          format = "f",
          digits = 2
        )
      )
      out <- sub("^0\\.", ".", out)
      out <- sub("^-0\\.", "-.", out)
      out
    }
  ) +
  
  labs(
    title = "Average daily number of apps",
    x = NULL,
    y = NULL
  ) +
  
  common_theme +
  
  theme(
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )


# ============================================================
# 10. MODEL RESULTS
#
# Read the saved wellbeing location-scale model summaries.
# All panels are shown on the native model-estimate scale with
# 95% confidence intervals.
# ============================================================

model_results <- read_csv(
  "wellbeing_plot_data.csv",
  show_col_types = FALSE
) %>%
  
  transmute(
    Construct = case_when(
      Variable %in% c("PANAS_POS", "PANAS_POS_score") ~ "Positive Affect",
      Variable %in% c("PANAS_NEG", "PANAS_NEG_score") ~ "Negative Affect",
      Variable == "SWLS" ~ "Life Satisfaction",
      Variable == "PHQ" ~ "Depressive Symptoms",
      TRUE ~ Variable_Clean
    ),
    Outcome = case_when(
      DV == "Duration (Hours/Day)" ~ "Smartphone use",
      DV == "Apps (Unique Count)" ~ "Unique apps",
      TRUE ~ DV
    ),
    Component = case_when(
      ParamType == "Fixed Effects (Location)" ~ "Fixed Effects\n(Location)",
      ParamType == "Within-Person Variance (Scale)" ~ "Within-Person Variance\n(Scale)",
      TRUE ~ NA_character_
    ),
    estimate = Estimate,
    SE = SE,
    p = p_value
  ) %>%
  
  filter(
    !is.na(Construct),
    !is.na(Outcome),
    !is.na(Component)
  )


# ============================================================
# 11. PREP MODEL RESULTS
# ============================================================

construct_order <- c(
  "Positive Affect",
  "Negative Affect",
  "Life Satisfaction",
  "Depressive Symptoms"
)


model_results <- model_results %>%
  
  mutate(
    
    low =
      estimate -
      1.96 * SE,
    
    high =
      estimate +
      1.96 * SE,
    
    estimate_plot = estimate,
    low_plot = low,
    high_plot = high,
    
    sig_label = case_when(
      p < .001 ~ "***",
      p < .01 ~ "**",
      p < .05 ~ "*",
      TRUE ~ ""
    ),
    
    
    Construct =
      factor(
        Construct,
        levels =
          rev(
            construct_order
          )
      )
  )
# ============================================================
# 12. COMPACT LOLLIPOP FUNCTION
# ============================================================

make_lollipop <- function(
    outcome_name,
    component_name,
    show_title = TRUE,
    show_y_labels = TRUE
) {
  
  component_match <- gsub("\n", " ", component_name, fixed = TRUE)
  
  dat <- model_results %>%
    
    filter(
      Outcome == outcome_name,
      gsub("\n", " ", Component, fixed = TRUE) == component_match
    ) %>%
    
    mutate(
      Construct_chr = as.character(Construct)
    )
  
  
  # ----------------------------------------------------------
  # Axis title
  # ----------------------------------------------------------
  
  axis_label <- case_when(
    
    TRUE ~ "Estimate"
  )
  
  
  # ----------------------------------------------------------
  # Symmetrical x-axis
  # ----------------------------------------------------------
  
  max_abs <- max(
    abs(
      c(
        dat$low_plot,
        dat$high_plot
      )
    ),
    na.rm = TRUE
  )
  
  
  x_limit <- max_abs * 1.45
  
  dat <- dat %>%
    mutate(
      star_x = if_else(
        estimate_plot >= 0,
        high_plot + x_limit * 0.03,
        low_plot - x_limit * 0.03
      ),
      star_hjust = if_else(
        estimate_plot >= 0,
        0,
        1
      )
    )
  
  
  ggplot(
    dat,
    aes(
      y = Construct,
      color = Construct_chr
    )
  ) +
    
    # zero reference
    geom_vline(
      xintercept = 0,
      linetype = "dotted",
      color = "#D95F5F",
      linewidth = 0.9
    ) +
    
    # 95% CI
    geom_segment(
      aes(
        x = low_plot,
        xend = high_plot,
        yend = Construct
      ),
      linewidth = 0.9
    ) +
    
    geom_segment(
      aes(
        x = low_plot,
        xend = low_plot,
        y = as.numeric(Construct) - 0.14,
        yend = as.numeric(Construct) + 0.14,
        color = Construct_chr
      ),
      linewidth = 0.9,
      inherit.aes = FALSE
    ) +
    
    geom_segment(
      aes(
        x = high_plot,
        xend = high_plot,
        y = as.numeric(Construct) - 0.14,
        yend = as.numeric(Construct) + 0.14,
        color = Construct_chr
      ),
      linewidth = 0.9,
      inherit.aes = FALSE
    ) +
    
    # estimate
    geom_point(
      aes(
        x = estimate_plot
      ),
      size = 3.2
    ) +
    
    geom_text(
      aes(
        x = star_x,
        label = sig_label,
        hjust = star_hjust
      ),
      size = 3,
      color = base_col,
      fontface = "bold",
      show.legend = FALSE
    ) +
    
    scale_color_manual(
      values = construct_cols_model
    ) +
    
    scale_x_continuous(
      labels = function(x) {
        out <- sub(
          "\\.?0+$",
          "",
          formatC(
            x,
            format = "f",
            digits = 2
          )
        )
        out <- sub("^0\\.", ".", out)
        out <- sub("^-0\\.", "-.", out)
        out
      }
    ) +
    
    coord_cartesian(
      xlim = c(
        -x_limit,
        x_limit
      ),
      clip = "off"
    ) +
    
    labs(
      
    title =
        if (show_title) {
          component_name
        } else {
          NULL
        },
      
      x = axis_label,
      
      y = NULL
    ) +
    
    theme_ipsum(base_size = 10, axis_title_size = 12) +
    
    theme(
      
      plot.background =
        element_rect(
          fill = bg_col,
          color = NA
        ),
      
      panel.background =
        element_rect(
          fill = bg_col,
          color = NA
        ),
      
      panel.grid.major.y =
        element_blank(),
      
      panel.grid.minor =
        element_blank(),
      
      panel.grid.major.x =
        element_line(
          color = grid_col,
          linewidth = 0.25
        ),
      
      axis.text.y =
        if (show_y_labels) {
          
          element_text(
            size = 10.5,
            color = base_col
          )
          
        } else {
          
          element_blank()
        },
      
      axis.ticks.y =
        element_blank(),
      
      axis.text.x =
        element_text(
          size = 9,
          color = "grey40"
        ),
      
      axis.title.x =
        element_text(
          size = 12,
          color = "grey40"
        ),
      
      plot.title =
        element_text(
          size = 11,
          face = "bold",
          color = base_col,
          hjust = 0.5
        ),
      
      legend.position =
        "none",
      
      plot.margin =
        margin(
          4,
          2,
          4,
          2
        )
    )
}

# ============================================================
# 13. MODEL PANELS
# ============================================================


# ------------------------------------------------------------
# Smartphone use
# ------------------------------------------------------------

p_usage_location <-
  make_lollipop(
    outcome_name = "Smartphone use",
    component_name = "Fixed Effects\n(Location)",
    show_title = TRUE,
    show_y_labels = TRUE
  )


p_usage_scale <-
  make_lollipop(
    outcome_name = "Smartphone use",
    component_name = "Within-Person Variance\n(Scale)",
    show_title = TRUE,
    show_y_labels = FALSE
  )


# ------------------------------------------------------------
# Unique apps
# ------------------------------------------------------------

p_apps_location <-
  make_lollipop(
    outcome_name = "Unique apps",
    component_name = "Fixed Effects (Location)",
    show_title = FALSE,
    show_y_labels = TRUE
  )


p_apps_scale <-
  make_lollipop(
    outcome_name = "Unique apps",
    component_name = "Within-Person Variance (Scale)",
    show_title = FALSE,
    show_y_labels = FALSE
  )

# ============================================================
# 14. SMARTPHONE-USE ROW
# ============================================================
row_usage <-
  (
    p_usage |
      p_usage_location |
      p_usage_scale
  ) +
  plot_layout(
    widths = c(
      2.4,
      1.1,
      1
    )
  )


# ============================================================
# 15. UNIQUE-APPS ROW
# ============================================================

row_apps <-
  (
  p_apps |
  
  p_apps_location |
  
  p_apps_scale ) +
  
  plot_layout(
    widths = c(
      2.4,
      1.1,
      1
    )
  )


# ============================================================
# 16. FINAL FIGURE
# ============================================================

p_wellbeing <-
  
  row_usage /
  
  row_apps +
  
  plot_layout(
    heights = c(
      1,
      1
    )
  ) +
  
  plot_annotation(
    
    # title =
    #   "Smartphone Use, App Diversity, and Well-Being",
    
    subtitle =
      paste(
        ""
      ),
    
    caption = "",
    
    theme = theme(
      
      plot.background =
        element_rect(
          fill = bg_col,
          color = NA
        ),
      
      plot.title =
        element_text(
          size = 17,
          face = "bold",
          color = base_col
        ),
      
      plot.subtitle =
        element_text(
          size = 10,
          color = "grey35"
        ),
      
      plot.caption =
        element_text(
          size = 8,
          color = "grey40",
          hjust = 0
        )
    )
  )

# ============================================================
# 17. DISPLAY
# ============================================================

p_wellbeing


# ============================================================
# 18. SAVE
# ============================================================

ggsave(
  
  filename =
    "Figures/wellbeing_descriptive_location_scale.jpg",
  
  plot =
    p_wellbeing,
  
  width =
    9.5,
  
  height =
    5,
  
  units =
    "in",
  
  dpi =
    300,
  
  bg =
    bg_col
)





 # ============================================================
 # 19. DEMOGRAPHIC DESCRIPTIVES + LOCATION-SCALE MODELS
 # ============================================================
 
demo_cat_cols <- c(
  "Female" = accent_col,
  "Male" = base_col,
  "Other" = "#CDB67C",
  "Weekday" = "#E58E7E",
  "Weekend" = "#F1B77D"
)

demo_age_col <- "#536878"
demo_gender_cols <- c(
  "Female" = "#83AF9B",
  "Male" = "#4F7F72",
  "Other" = "#B8D3C8"
)
demo_day_cols <- c(
  "Weekday" = "#E58E7E",
  "Weekend" = "#F1B77D"
)

demo_term_cols <- c(
  "Age (Centered)" = demo_age_col,
  "Female vs. male" = unname(demo_gender_cols["Female"]),
  "Other vs. male" = unname(demo_gender_cols["Other"]),
  "Weekend vs. weekday" = unname(demo_day_cols["Weekday"])
)
 
 dem_participant <- manyapps_hourly_noapp %>%
   group_by(unique_participant_number) %>%
   summarise(
     age = as.numeric(first_non_na(age)),
     Gender = first_non_na(gender),
     smartphone_hours = first_non_na(study_average_daily_app_usage) / 3600,
     unique_apps = safe_mean(unique_apps_day),
     .groups = "drop"
   ) %>%
   mutate(
     Gender = case_when(
       tolower(Gender) == "female" ~ "Female",
       tolower(Gender) == "male" ~ "Male",
       tolower(Gender) == "other" ~ "Other",
       TRUE ~ as.character(Gender)
     ),
     Gender = factor(Gender, levels = c("Female", "Male", "Other"))
   )
 
 dem_day <- manyapps_hourly_noapp %>%
   distinct(
     unique_participant_number,
     day,
     total_daily_app_usage,
     unique_apps_day
   ) %>%
   mutate(
     day = as.Date(day),
     Day = if_else(
       wday(day, week_start = 1) %in% 1:5,
       "Weekday",
       "Weekend"
     ),
     smartphone_hours = total_daily_app_usage / 3600,
     unique_apps = unique_apps_day
   ) %>%
   group_by(unique_participant_number, Day) %>%
   summarise(
     smartphone_hours = safe_mean(smartphone_hours),
     unique_apps = safe_mean(unique_apps),
     .groups = "drop"
   ) %>%
   mutate(
     Day = factor(Day, levels = c("Weekday", "Weekend"))
   )
 
 baseline_demo_models <- read_csv(
   "baseline_plot_data.csv",
   show_col_types = FALSE
 ) %>%
   filter(
     Variable %in% c(
       "age_grandCentere",
       "genderfemale",
       "genderother",
       "weekend"
     )
   ) %>%
   transmute(
     Term = case_when(
       Variable == "age_grandCentere" ~ "Age (Centered)",
       Variable == "genderfemale" ~ "Female vs. male",
       Variable == "genderother" ~ "Other vs. male",
       Variable == "weekend" ~ "Weekend vs. weekday",
       TRUE ~ Variable_Clean
     ),
     Outcome = case_when(
       DV == "Duration (Hours/Day)" ~ "Smartphone use",
       DV == "Apps (Unique Count)" ~ "Unique apps",
       TRUE ~ DV
     ),
     Component = case_when(
       ParamType == "Fixed Effects (Location)" ~ "Fixed Effects (Location)",
       ParamType == "Within-Person Variance (Scale)" ~ "Within-Person Variance (Scale)",
       TRUE ~ NA_character_
     ),
     estimate = Estimate,
     SE = SE,
     p = p_value
   ) %>%
   filter(!is.na(Component)) %>%
   mutate(
     low = estimate - 1.96 * SE,
     high = estimate + 1.96 * SE,
     sig_label = case_when(
       p < .001 ~ "***",
       p < .01 ~ "**",
       p < .05 ~ "*",
       TRUE ~ ""
     ),
     Term = factor(
       Term,
       levels = rev(c(
         "Age (Centered)",
         "Female vs. male",
         "Other vs. male",
         "Weekend vs. weekday"
       ))
     )
   )
 
 demo_mean_ci <- function(x) {
   x <- x[!is.na(x)]
   n <- length(x)
   if (n < 2) {
     return(data.frame(y = mean(x), ymin = NA_real_, ymax = NA_real_))
   }
   m <- mean(x)
   se <- sd(x) / sqrt(n)
   ci <- qt(.975, df = n - 1) * se
   data.frame(y = m, ymin = m - ci, ymax = m + ci)
 }
 
 
 demo_age_line_col <- grDevices::colorRampPalette(
   c(demo_age_col, "black")
 )(5)[2]  # 25% lighter
 
 
 p_demo_usage_age <- ggplot(
   dem_participant,
   aes(x = age, y = smartphone_hours)
 ) +
   geom_point(
     color = scales::alpha(demo_age_col, 0.18),
     size = 0.8
   ) +
   geom_smooth(
     color = demo_age_line_col,
     fill = scales::alpha(demo_age_col, 0.12),
     linewidth = 1.4,
     se = TRUE,
     method = "loess"
   ) +
   labs(
     title = "Average daily smartphone use",
     x = NULL,
     y = NULL
   ) +
   common_theme +
   theme(plot.margin = margin(5, 5, 5, 5))
 
 
 p_demo_usage_gender <- ggplot(
   dem_participant %>% filter(!is.na(Gender)),
   aes(x = Gender, y = smartphone_hours, fill = Gender)
) +
  geom_violin(alpha = 0.80, color = NA, trim = TRUE, width = 0.85) +
  stat_summary(
     fun.data = demo_mean_ci,
     geom = "errorbar",
     width = 0.12,
     linewidth = 0.55,
     color = base_col
   ) +
   stat_summary(
     fun = mean,
     geom = "point",
     size = 2.4,
     color = base_col
  ) +
  scale_fill_manual(values = demo_gender_cols) +
  labs(
  #  title = "Gender",
     x = NULL,
     y = NULL
   ) +
   common_theme +
   theme(
     legend.position = "none",
     plot.margin = margin(5, 5, 5, 5)
   )
 
 p_demo_usage_day <- ggplot(
   dem_day,
   aes(x = Day, y = smartphone_hours, fill = Day)
) +
  geom_violin(alpha = 0.80, color = NA, trim = TRUE, width = 0.85) +
  stat_summary(
     fun.data = demo_mean_ci,
     geom = "errorbar",
     width = 0.12,
     linewidth = 0.55,
     color = base_col
   ) +
   stat_summary(
     fun = mean,
     geom = "point",
     size = 2.4,
     color = base_col
  ) +
  scale_fill_manual(values = demo_day_cols) +
  labs(
  #  title = "Weekday vs. Weekend",
     x = NULL,
     y = NULL
   ) +
   common_theme +
   theme(
     legend.position = "none",
     plot.margin = margin(5, 5, 5, 5)
   )
 
 p_demo_apps_age <- ggplot(
   dem_participant,
  aes(x = age, y = unique_apps)
) +
  geom_point(
    color = scales::alpha(demo_age_col, 0.22),
    size = 0.8
  ) +
  geom_smooth(
    color = demo_age_line_col,
    fill = scales::alpha(demo_age_col, 0.18),
    linewidth = 1,
    se = TRUE,
    method = "loess"
   ) +
   labs(
     title = "Average daily number of apps",
     x = "Age",
     y = NULL
   ) +
   common_theme +
   theme(plot.margin = margin(5, 5, 5, 5))
 
 p_demo_apps_gender <- ggplot(
   dem_participant %>% filter(!is.na(Gender)),
   aes(x = Gender, y = unique_apps, fill = Gender)
) +
  geom_violin(alpha = 0.80, color = NA, trim = TRUE, width = 0.85) +
  stat_summary(
     fun.data = demo_mean_ci,
     geom = "errorbar",
     width = 0.12,
     linewidth = 0.55,
     color = base_col
   ) +
   stat_summary(
     fun = mean,
     geom = "point",
     size = 2.4,
     color = base_col
  ) +
  scale_fill_manual(values = demo_gender_cols) +
  labs(
  #  title = "Gender",
     x = NULL,
     y = NULL
   ) +
   common_theme +
   theme(
     legend.position = "none",
     plot.margin = margin(5, 5, 5, 5)
   )
 
 p_demo_apps_day <- ggplot(
   dem_day,
   aes(x = Day, y = unique_apps, fill = Day)
) +
  geom_violin(alpha = 0.80, color = NA, width = 0.85,  trim = TRUE) +
  stat_summary(
     fun.data = demo_mean_ci,
     geom = "errorbar",
     width = 0.12,
     linewidth = 0.55,
     color = base_col
   ) +
   stat_summary(
     fun = mean,
     geom = "point",
     size = 2.4,
     color = base_col
  ) +
  scale_fill_manual(values = demo_day_cols) +
  labs(
  #  title = "Weekday vs. Weekend",
     x = NULL,
     y = NULL
   ) +
   common_theme +
   theme(
     legend.position = "none",
     plot.margin = margin(5, 5, 5, 5)
   )
 
make_demographic_model_panel <- function(
    outcome_name,
    component_name,
    show_title = TRUE,
    show_y_labels = TRUE
) {
  
   component_match <- gsub("\n", " ", component_name, fixed = TRUE)
   
   dat <- baseline_demo_models %>%
     filter(
       Outcome == outcome_name,
       gsub("\n", " ", Component, fixed = TRUE) == component_match
     ) %>%
     mutate(
       Term_chr = as.character(Term)
     )
   
   max_abs <- max(abs(c(dat$low, dat$high)), na.rm = TRUE)
   x_limit <- max_abs * 1.45
   
   dat <- dat %>%
     mutate(
       star_x = if_else(
         estimate >= 0,
         high + x_limit * 0.03,
         low - x_limit * 0.03
       ),
       star_hjust = if_else(
         estimate >= 0,
         0,
         1
       )
     )
   
   ggplot(
     dat,
     aes(y = Term, color = Term_chr)
   ) +
     geom_vline(
       xintercept = 0,
       linetype = "dotted",
       color = "#D95F5F",
       linewidth = 0.9
     ) +
     geom_segment(
       aes(
         x = low,
         xend = high,
         yend = Term
       ),
       linewidth = 0.9
     ) +
     geom_segment(
       aes(
         x = low,
         xend = low,
         y = as.numeric(Term) - 0.14,
         yend = as.numeric(Term) + 0.14,
         color = Term_chr
       ),
       linewidth = 0.9,
       inherit.aes = FALSE
     ) +
     geom_segment(
       aes(
         x = high,
         xend = high,
         y = as.numeric(Term) - 0.14,
         yend = as.numeric(Term) + 0.14,
         color = Term_chr
       ),
       linewidth = 0.9,
       inherit.aes = FALSE
     ) +
     geom_point(
       aes(x = estimate),
       size = 3.2
     ) +
     geom_text(
       aes(
         x = star_x,
         label = sig_label,
         hjust = star_hjust
       ),
       size = 3,
       color = base_col,
       fontface = "bold",
       show.legend = FALSE
     ) +
     scale_color_manual(values = demo_term_cols) +
     coord_cartesian(
       xlim = c(-x_limit, x_limit),
       clip = "off"
     ) +
     labs(
       title = if (show_title) component_name else NULL,
       x = "Estimate",
       y = NULL
     ) +
     theme_ipsum(base_size = 10, axis_title_size = 12) +
     theme(
       plot.background = element_rect(fill = bg_col, color = NA),
       panel.background = element_rect(fill = bg_col, color = NA),
       panel.grid.major.y = element_blank(),
       panel.grid.minor = element_blank(),
       panel.grid.major.x = element_line(color = grid_col, linewidth = 0.25),
       axis.text.y = if (show_y_labels) {
         element_text(size = 10.5, color = base_col)
       } else {
         element_blank()
       },
       axis.ticks.y = element_blank(),
       axis.text.x = element_text(size = 9, color = "grey40"),
       axis.title.x = element_text(size = 12, color = "grey40"),
       plot.title = element_text(size = 11, face = "bold", color = base_col, hjust = 0.5),
       legend.position = "none",
       plot.margin = margin(5, 4, 5, 4)
     )
 }
 
 p_demo_model_usage_location <- make_demographic_model_panel(
   outcome_name = "Smartphone use",
   component_name = "Fixed Effects\n(Location)",
   show_title = TRUE,
   show_y_labels = TRUE
 )
 
 p_demo_model_usage_scale <- make_demographic_model_panel(
   outcome_name = "Smartphone use",
   component_name = "Within-Person Variance\n(Scale)",
   show_title = TRUE,
   show_y_labels = FALSE
 )
 
 p_demo_model_apps_location <- make_demographic_model_panel(
   outcome_name = "Unique apps",
   component_name = "Fixed Effects (Location)",
   show_title = FALSE,
   show_y_labels = TRUE
 )
 
 p_demo_model_apps_scale <- make_demographic_model_panel(
   outcome_name = "Unique apps",
   component_name = "Within-Person Variance (Scale)",
   show_title = FALSE,
   show_y_labels = FALSE
 )
 
 row_demo_usage <-
   (
     p_demo_usage_age |
       p_demo_usage_gender |
       p_demo_usage_day |
       p_demo_model_usage_location |
       p_demo_model_usage_scale
   ) +
   plot_layout(widths = c(0.9, 0.9, 0.9, 1.1,1))
 
 row_demo_apps <-
   (
     p_demo_apps_age |
       p_demo_apps_gender |
       p_demo_apps_day |
       p_demo_model_apps_location |
       p_demo_model_apps_scale
   ) +
   plot_layout(widths = c(0.9, 0.9, 0.9, 1.1,1))
 
 p_demographic <-
   row_demo_usage /
   row_demo_apps +
   plot_layout(heights = c(1, 1)) +
   plot_annotation(
   #  title = "Smartphone Use, App Diversity, and Demographics",
     subtitle = "",
     caption = "",
     theme = theme(
       plot.background = element_rect(fill = bg_col, color = NA),
       plot.title = element_text(size = 17, face = "bold", color = base_col),
       plot.subtitle = element_text(size = 10, color = "grey35"),
       plot.caption = element_text(size = 8, color = "grey40", hjust = 0)
     )
   )
 
 p_demographic
 
 ggsave(
   filename = "Figures/demographic_descriptive_location_scale.jpg",
   plot = p_demographic,
   width = 9.5,
   height = 5,
   units = "in",
   dpi = 300,
   bg = bg_col
 )

############################################################
################## Research Question 4 #####################
############################################################

base_col <- "#213547"
accent_col <- "#5B8E7D"
soft_col <- "#B8D8D8"
warm_col <- "#E7A977"
rose_col <- "#D97B66"
grid_col <- "#D9E1E5"

max_share <- max(
  top_apps_by_agegroup$mean_share_group,
  top_apps_by_gender$mean_share_group,
  top_apps_by_sample$mean_share_group,
  na.rm = TRUE
)

fill_scale <- scale_fill_gradientn(
  colours = c(soft_col, accent_col, base_col),
  limits = c(0, max_share),
  name = "Mean share",
  labels = scales::percent
)

pretty_app_name <- function(x) {
  manual_map <- c(
    "com.whatsapp" = "WhatsApp",
    "com.instagram.android" = "Instagram",
    "com.facebook.katana" = "Facebook",
    "com.facebook.lite" = "Facebook Lite",
    "com.facebook.orca" = "Facebook Messenger",
    "com.google.android.youtube" = "YouTube",
    "com.zhiliaoapp.musically" = "TikTok",
    "com.twitter.android" = "Twitter",
    "com.snapchat.android" = "Snapchat",
    "org.telegram.messenger" = "Telegram",
    "com.spotify.music" = "Spotify",
    "com.google.android.gm" = "Gmail",
    "com.android.chrome" = "Chrome",
    "org.mozilla.firefox" = "Firefox",
    "com.duckduckgo.mobile.android" = "DuckDuckGo",
    "com.ecosia.android" = "Ecosia",
    "com.google.android.apps.maps" = "Google Maps",
    "com.google.android.apps.messaging" = "Google Messages",
    "com.samsung.android.messaging" = "Samsung Messages",
    "com.sec.android.app.sbrowser" = "Samsung Internet",
    "com.google.android.deskclock" = "Clock",
    "com.netflix.mediaclient" = "Netflix",
    "com.amazon.avod.thirdpartyclient" = "Prime Video",
    "com.amazon.mShop.android.shopping" = "Amazon",
    "com.microsoft.office.outlook" = "Outlook",
    "com.google.android.apps.photos" = "Google Photos",
    "com.linkedin.android" = "LinkedIn",
    "com.pinterest" = "Pinterest",
    "com.reddit.frontpage" = "Reddit",
    "com.metricwire.android3" = "MetricWire",
    "com.nianticlabs.pokemongo" = "Pokemon GO",
    "de.gmx.mobile.android.mail" = "GMX Mail",
    "de.web.mobile.android.mail" = "WEB.DE Mail",
    "net.oneplus.launcher" = "OnePlus Launcher"
  )

  mapped <- unname(manual_map[x])
  fallback <- x %>%
    stringr::str_replace("^.*\\.", "") %>%
    stringr::str_replace_all("[-_]", " ") %>%
    stringr::str_replace_all("\\b(android|app)\\b", "") %>%
    stringr::str_squish() %>%
    tolower() %>%
    tools::toTitleCase()

  ifelse(is.na(mapped) | mapped == "", ifelse(fallback == "", x, fallback), mapped)
}

make_app_heatmap <- function(dat, x_var, title) {
  
  plot_dat <- dat %>%
    filter(n_people_app >= 20, rank <= 10) %>%
    mutate(App_label = pretty_app_name(Package_name))
  
  app_order <- plot_dat %>%
    group_by(App_label) %>%
    summarise(
      overall_share = sum(mean_share_group, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(overall_share)) %>%
    pull(App_label)
  
  plot_dat <- plot_dat %>%
    mutate(
      App_label = factor(App_label, levels = rev(app_order))
    )
  
  ggplot(
    plot_dat,
    aes(
      x = {{ x_var }},
      y = App_label,
      fill = mean_share_group
    )
  ) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = rank),
      color = "white",
      fontface = "bold",
      size = 3
    ) +
    fill_scale +
    scale_y_discrete(expand = expansion(add = c(0.35, 0.35))) +
    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    theme_ipsum(base_size = 10, axis_title_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", color = base_col),
      axis.text.y = element_text(margin = margin(r = 10)),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

p_age <- make_app_heatmap(
  top_apps_by_agegroup,
  age_group,
  "Age group"
)


min(top_apps_by_agegroup$mean_share_group[top_apps_by_agegroup$Package_name == "com.whatsapp"])
max(top_apps_by_agegroup$mean_share_group[top_apps_by_agegroup$Package_name == "com.whatsapp"])

top_apps_by_agegroup$mean_share_group[top_apps_by_agegroup$Package_name == "com.instagram.android"]

p_gender <- make_app_heatmap(
  top_apps_by_gender,
  gender,
  "Gender"
)

p_sample <- make_app_heatmap(
  top_apps_by_sample,
  Dataset,
  "Sample"
)


top_apps_by_gender$mean_share_group[top_apps_by_agegroup$Package_name == "com.instagram.android"]

min(top_apps_by_gender$mean_share_group[top_apps_by_gender$Package_name == "com.whatsapp"])
max(top_apps_by_gender$mean_share_group[top_apps_by_gender$Package_name == "com.whatsapp"])

min(top_apps_by_sample$mean_share_group[top_apps_by_sample$Package_name == "com.whatsapp"])
max(top_apps_by_sample$mean_share_group[top_apps_by_sample$Package_name == "com.whatsapp"])



all <- (
  (p_age + p_gender + plot_layout(widths = c(2, 1))) /
    p_sample
) +
  plot_layout(
    heights = c(1, 1.2),
    guides = "collect"
  ) &
  theme(legend.position = "right")


ggsave(
  "Figures/top_apps_all.png",
  all,
  width = 8,
  height = 9,
  dpi = 300,
  bg = "white"
)


ggsave(
  "Figures/top_apps_agegroup.png",
  p_age,
  width = 8,
  height = 4,
  dpi = 300,
  bg = "white"
)


ggsave(
  "Figures/top_apps_gender.png",
  p_gender,
  width = 8,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  "Figures/top_apps_sample.png",
  p_sample,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


####################### Exploratory #############
#################################################


library(tidyverse)
library(lubridate)
library(ggrepel)
library(hrbrthemes)


# ------------------------------------------------------------
# 1. Create one observation per participant
# ------------------------------------------------------------

participant_use <- manyapps_hourly_noapp %>%
  filter(
    !is.na(study_average_daily_app_usage),
    is.finite(study_average_daily_app_usage),
    !is.na(day)
  ) %>%
  arrange(
    Dataset,
    unique_participant_number,
    day
  ) %>%
  mutate(
    year = lubridate::year(day),
    study_average_daily_app_usage = study_average_daily_app_usage / 60 /60
  ) %>%
  distinct(
    Dataset,
    unique_participant_number,
    .keep_all = TRUE
  ) %>%
  select(
    year,
    Dataset,
    country,
    unique_participant_number,
    study_average_daily_app_usage
  )


# ------------------------------------------------------------
# 2. Calculate yearly descriptives
# ------------------------------------------------------------

year_summary <- participant_use %>%
  group_by(year) %>%
  summarise(
    mean_usage = mean(
      study_average_daily_app_usage,
      na.rm = TRUE
    ),
    median_usage = median(
      study_average_daily_app_usage,
      na.rm = TRUE
    ),
    sd_usage = sd(
      study_average_daily_app_usage,
      na.rm = TRUE
    ),
    n_participants = n(),
    n_datasets = n_distinct(Dataset),
    countries = paste(
      sort(unique(na.omit(as.character(country)))),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    se_usage = sd_usage / sqrt(n_participants),
    
    # 95% confidence intervals
    critical_t = qt(
      0.975,
      df = pmax(n_participants - 1, 1)
    ),
    ci_lower = mean_usage - critical_t * se_usage,
    ci_upper = mean_usage + critical_t * se_usage,
    
    # Plot labels
    plot_label = paste0(
      str_wrap(countries, width = 22),
      "\n",
      n_datasets,
      if_else(
        n_datasets == 1,
        " dataset",
        " datasets"
      )
    )
  )


# ------------------------------------------------------------
# 3. Plot
# ------------------------------------------------------------

year_plot <- ggplot(
  year_summary,
  aes(x = year, y = mean_usage)
) +
  geom_line(
    color = base_col,
    linewidth = 0.8,
    alpha = 0.75
  ) +
  geom_errorbar(
    aes(
      ymin = ci_lower,
      ymax = ci_upper
    ),
    width = 0.10,
    linewidth = 0.65,
    color = base_col
  ) +
  geom_point(
    aes(size = n_participants),
    color = accent_col,
    alpha = 0.9
  ) +
  
  # Medians
  geom_point(
    aes(y = median_usage),
    shape = 4,
    size = 3,
    stroke = 0.9,
    color = base_col
  ) +
  
  # Labels without outlines
  geom_label_repel(
    aes(label = plot_label),
    size = 3.1,
    color = base_col,
    fill = scales::alpha("white", 0.88),
    linewidth = 0,
    label.padding = grid::unit(0.11, "lines"),
    label.r = grid::unit(0.12, "lines"),
    lineheight = 0.99,
    nudge_y = 0.6,
    direction = "y",
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.color = scales::alpha(base_col, 0.30),
    segment.linewidth = 0.35,
    seed = 123,
    show.legend = FALSE
  ) +
  scale_size_continuous(
    name = "Participants",
    range = c(3.5, 10)
  ) +
  scale_x_continuous(
    breaks = sort(unique(year_summary$year))
  ) +
  # scale_y_continuous(
  #   expand = expansion(mult = c(0.08, 0.20))
  # ) +
  labs(
    x = "Year",
    y = "Daily app usage (hours)") +
  theme_ipsum() +
  theme(
    legend.position = "right",
    plot.title = element_text(
      color = base_col,
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = scales::alpha(base_col, 0.75)
    ),
    axis.title = element_text(
      color = base_col,
      size = 60
    ),
    axis.text = element_text(
      color = base_col,
      size = 20
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 10,
      l = 10
    )
  )

year_plot




ggsave(
  filename = "Figures/year_plot.png",
  plot = year_plot,
  width = 8.5,
  height = 4.5,
  dpi = 300,
  bg = "white"
)















#### Hourly use


####################################



plot_df <-
  manyapps_hourly_noapp %>%
  mutate(
    hour = lubridate::hour(lubridate::hms(hourly_time)),
    wday_num = lubridate::wday(day, week_start = 1),
    wday = factor(
      wday_num,
      levels = 1:7,
      labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    )
  ) %>%
  group_by(Dataset, unique_participant_number,wday, hour) %>%   # <-- add dataset here
  summarise(
    median_min = median(total_hourly_app_usage, na.rm = TRUE)/60,
    sd_min = sd(total_hourly_app_usage, na.rm = TRUE)/60,
    .groups = "drop"
  )


################


length(unique(manyapps_hourly_noapp$unique_participant_number))
length(unique(manyapps_hourly_noapp$Dataset))



# One plot: 7 weekday lines across 24 hours (with SD ribbon)
plot_df <- 
  manyapps_hourly_noapp %>%
  mutate(
    hour = lubridate::hour(lubridate::hms(hourly_time)),
    wday_num = lubridate::wday(day, week_start = 1),
    wday = factor(
      wday_num,
      levels = 1:7,
      labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    )
  ) %>%
  group_by(wday, hour) %>%
  summarise(
    mean_min = mean(total_hourly_app_usage, na.rm = TRUE)/60,
    sd_min = sd(total_hourly_app_usage, na.rm = TRUE)/60,
    .groups = "drop"
  )


plot_df <- na.omit(plot_df)

#########

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(ggtext)

peak_hour_df <- plot_df %>%
  group_by(hour) %>%
  summarise(mean_min = sum(mean_min, na.rm = TRUE), .groups = "drop") %>%
  slice_max(mean_min, n = 1)

min_hour_df <- plot_df %>%
  group_by(hour) %>%
  summarise(mean_min = sum(mean_min, na.rm = TRUE), .groups = "drop") %>%
  slice_min(mean_min, n = 1)

plot_df <- plot_df %>% mutate( day_group = case_when( wday == "Friday" ~ "Friday", wday == "Saturday" ~ "Saturday", wday == "Sunday" ~ "Sunday", TRUE ~ "Weekday" ) )

pa  = ggplot(plot_df,
             aes(x = hour, y = mean_min, group = wday)) +
  
  # -----------------------
# Weekdays
geom_line(
  data = subset(plot_df, day_group == "Weekday"),
  color = "#A0A6AC",
  linewidth = 0.9,
  alpha = 0.90
) +
  
  # Friday
  geom_line(
    data = subset(plot_df, day_group == "Friday"),
    color = "#213547",
    linewidth = 1.3
  ) +
  
  # Saturday
  geom_line(
    data = subset(plot_df, day_group == "Saturday"),
    color = "#5B8E7D",
    linewidth = 1.6
  ) +
  
  # Sunday
  geom_line(
    data = subset(plot_df, day_group == "Sunday"),
    color = "#D97B66",
    linewidth = 1.6
  ) +
  
  # -----------------------
# # Annotations
# annotate(
#   "text",
#   x = 19, y = 5.6,
#   label = "Friday & Saturday:\nless phone use\nin afternoon/evening",
#   hjust = 0,
#   size = 3.7,
#   color = "grey20"
# ) +
#   
#   annotate(
#     "text",
#     x = 7, y = 3.2,
#     label = "Sunday:\nphone use\nstarts later",
#     hjust = 0,
#     size = 3.7,
#     color = "grey20"
#   ) +
#   
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
    y = 6,
    label = paste0("Peak usage\n", peak_hour_df$hour, ":00"),
    size = 3.6,
    label.size = 0,
    fill = "white",
    color = "grey20"
  ) +
  
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  
  labs(
    title = paste0(
      "<span style='color:#A0A6AC;'>Mon–Thu</span>, ",
      "<span style='color:#213547;'>Friday</span>, ",
      "<span style='color:#5B8E7D;'>Saturday</span>, ",
      "<span style='color:#D97B66;'>Sunday</span>"
    ),
    x = "Hour of Day",
    y = "Average Use Duration (min/hour)"
  ) +
  
  theme_ipsum(base_size = 13, axis_title_size = 15) +
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
    y = 11,  # slightly above line
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




ggsave(
  filename = "Figures/hourly.jpg",
  plot = pa,
  width = 9,
  height = 4,
  units = "in",
  dpi = 300,
  bg = bg_col
)

# One plot: 7 weekday lines across 24 hours (with SD ribbon)
plot_df <- manyapps_hourly_noapp %>%
  mutate(
    hour = lubridate::hour(lubridate::hms(hourly_time)),
    wday_num = lubridate::wday(day, week_start = 1),
    wday = factor(
      wday_num,
      levels = 1:7,
      labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    )
  ) %>%
  group_by(wday) %>%
  summarise(
    mean_min = mean(total_daily_app_usage, na.rm = TRUE)/60/60,
    sd_min = sd(total_daily_app_usage, na.rm = TRUE)/60/60,
    .groups = "drop"
  )%>%
  mutate(
    day_group = case_when(
      wday == "Friday"   ~ "Friday",
      wday == "Saturday" ~ "Saturday",
      wday == "Sunday"   ~ "Sunday",
      TRUE               ~ "Weekday"
    )
  )



plot_df <- na.omit(plot_df)



pb = ggplot(plot_df,
            aes(x = wday, y = mean_min, group = 1)) +
  
  # connecting baseline (subtle)
  geom_line(color = "#D0D4D8", linewidth = 0.8) +
  
  # weekdays
  geom_point(
    data = subset(plot_df, day_group == "Weekday"),
    color = "#A0A6AC",
    size = 3
  ) +
  
  # friday
  geom_point(
    data = subset(plot_df, day_group == "Friday"),
    color = "#213547",
    size = 3
  ) +
  
  # saturday
  geom_point(
    data = subset(plot_df, day_group == "Saturday"),
    color = "#5B8E7D",
    size = 3
  ) +
  
  # sunday
  geom_point(
    data = subset(plot_df, day_group == "Sunday"),
    color = "#D97B66",
    size = 3
  ) +
  
  # optional SD error bars (light + minimal)
  geom_errorbar(
    aes(ymin = mean_min - sd_min,
        ymax = mean_min + sd_min),
    width = 0.15,
    color = "grey70",
    linewidth = 0.5
  ) +
  
  labs(
    title = paste0(
      "Average Daily App Usage — ",
      "<span style='color:#A0A6AC;'>Mon–Thu</span>, ",
      "<span style='color:#457B9D;'>Friday</span>, ",
      "<span style='color:#5B8E7D;'>Saturday</span>, ",
      "<span style='color:#D97B66;'>Sunday</span>"
    ),
    x = NULL,
    y = "Average Daily Usage (hours)"
  ) +
  
  theme_ipsum(base_size = 13, axis_title_size = 15) +
  theme(
    legend.position = "none",
    plot.title = element_markdown(),
    panel.grid.minor = element_blank()
  )
#ylim(c(0,3))

pb

library(patchwork)

pa / pb + 
  plot_layout(heights = c(2, 1))


############################################################
########### Research Question 3 (now 4) [Old plots] ########
############################################################
## Descreptive plots
library(tidyverse)
library(hrbrthemes)
library(patchwork)
library(lubridate)


# -------------------------------------------------------------------------
# 1. Dataset overview
# -------------------------------------------------------------------------

rq3_overview <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    study_average_daily_app_usage,
    PANAS_NEG,
    PANAS_POS,
    SWLS,
    PHQ
  ) %>%
  group_by(Dataset) %>%
  summarise(
    n = n_distinct(unique_participant_number),
    
    app_usage_mean = mean(
      study_average_daily_app_usage / 3600,
      na.rm = TRUE
    ),
    app_usage_sd = sd(
      study_average_daily_app_usage / 3600,
      na.rm = TRUE
    ),
    app_usage_min = min(
      study_average_daily_app_usage / 3600,
      na.rm = TRUE
    ),
    app_usage_max = max(
      study_average_daily_app_usage / 3600,
      na.rm = TRUE
    ),
    
    PANAS_NEG_mean = mean(PANAS_NEG, na.rm = TRUE),
    PANAS_NEG_sd = sd(PANAS_NEG, na.rm = TRUE),
    PANAS_NEG_min = min(PANAS_NEG, na.rm = TRUE),
    PANAS_NEG_max = max(PANAS_NEG, na.rm = TRUE),
    
    PANAS_POS_mean = mean(PANAS_POS, na.rm = TRUE),
    PANAS_POS_sd = sd(PANAS_POS, na.rm = TRUE),
    PANAS_POS_min = min(PANAS_POS, na.rm = TRUE),
    PANAS_POS_max = max(PANAS_POS, na.rm = TRUE),
    
    SWLS_mean = mean(SWLS, na.rm = TRUE),
    SWLS_sd = sd(SWLS, na.rm = TRUE),
    SWLS_min = min(SWLS, na.rm = TRUE),
    SWLS_max = max(SWLS, na.rm = TRUE),
    
    PHQ_mean = mean(PHQ, na.rm = TRUE),
    PHQ_sd = sd(PHQ, na.rm = TRUE),
    PHQ_min = min(PHQ, na.rm = TRUE),
    PHQ_max = max(PHQ, na.rm = TRUE),
    
    .groups = "drop"
  )


# -------------------------------------------------------------------------
# 2. Participant-level data: well-being and gender
# -------------------------------------------------------------------------

rq3_participant <- manyapps_hourly_noapp %>%
  arrange(Dataset, unique_participant_number, day) %>%
  distinct(
    Dataset,
    unique_participant_number,
    .keep_all = TRUE
  ) %>%
  mutate(
    smartphone_hours = study_average_daily_app_usage / 3600,
    Gender = factor(gender)
  )


# -------------------------------------------------------------------------
# 3. Participant-level weekday and weekend averages
# -------------------------------------------------------------------------

rq3_day <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    day,
    total_daily_app_usage
  ) %>%
  mutate(
    day = as.Date(day),
    smartphone_hours = total_daily_app_usage / 3600,
    Day = if_else(
      wday(day, week_start = 1) %in% 1:5,
      "Weekday",
      "Weekend"
    )
  ) %>%
  group_by(
    Dataset,
    unique_participant_number,
    Day
  ) %>%
  summarise(
    smartphone_hours = mean(
      smartphone_hours,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Day = factor(
      Day,
      levels = c("Weekday", "Weekend")
    )
  )


# -------------------------------------------------------------------------
# 4. Continuous well-being data
# -------------------------------------------------------------------------

rq3_cont <- rq3_participant %>%
  pivot_longer(
    cols = c(
      PANAS_POS,
      PANAS_NEG,
      SWLS,
      PHQ
    ),
    names_to = "Construct",
    values_to = "Score"
  ) %>%
  mutate(
    Construct = factor(
      Construct,
      levels = c(
        "PANAS_POS",
        "PANAS_NEG",
        "SWLS",
        "PHQ"
      ),
      labels = c(
        "Positive Affect",
        "Negative Affect",
        "Life Satisfaction",
        "Depressive Symptoms"
      )
    )
  )


# -------------------------------------------------------------------------
# 5. Gender data
# -------------------------------------------------------------------------

rq3_cat_gender <- rq3_participant %>%
  transmute(
    Dataset,
    unique_participant_number,
    Construct = "Gender",
    Group = as.character(Gender),
    smartphone_hours
  )


# -------------------------------------------------------------------------
# 6. Weekday/weekend data
# -------------------------------------------------------------------------

rq3_cat_day <- rq3_day %>%
  transmute(
    Dataset,
    unique_participant_number,
    Construct = "Weekday vs. Weekend",
    Group = as.character(Day),
    smartphone_hours
  )


# -------------------------------------------------------------------------
# 7. Combine categorical data
# -------------------------------------------------------------------------

rq3_cat <- bind_rows(
  rq3_cat_gender,
  rq3_cat_day
) %>%
  filter(
    !is.na(Group),
    !is.na(smartphone_hours),
    is.finite(smartphone_hours)
  ) %>%
  mutate(
    Construct = factor(
      Construct,
      levels = c(
        "Gender",
        "Weekday vs. Weekend"
      )
    ),
    Group = factor(Group)
  )


# -------------------------------------------------------------------------
# 8. Pearson correlations for continuous variables
# -------------------------------------------------------------------------

rq3_correlations <- rq3_cont %>%
  filter(
    !is.na(Score),
    !is.na(smartphone_hours),
    is.finite(Score),
    is.finite(smartphone_hours)
  ) %>%
  group_by(Construct) %>%
  summarise(
    n = n(),
    r = cor(
      Score,
      smartphone_hours,
      method = "pearson"
    ),
    p = cor.test(
      Score,
      smartphone_hours,
      method = "pearson"
    )$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      "r = ",
      sprintf("%.2f", r),
      ", ",
      case_when(
        p < .001 ~ "p < .001",
        TRUE ~ paste0(
          "p = ",
          sprintf("%.3f", p)
        )
      ),
      "\nn = ",
      n
    )
  )


# -------------------------------------------------------------------------
# 9. Mean, SD, and n for categorical groups
# -------------------------------------------------------------------------

rq3_group_stats <- rq3_cat %>%
  group_by(
    Construct,
    Group
  ) %>%
  summarise(
    n = sum(!is.na(smartphone_hours)),
    mean = mean(
      smartphone_hours,
      na.rm = TRUE
    ),
    sd = sd(
      smartphone_hours,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0(
      "M = ",
      sprintf("%.2f", mean),
      "\nSD = ",
      sprintf("%.2f", sd),
      "\nn = ",
      n
    )
  )


# Position for categorical labels
cat_label_y <- max(
  rq3_cat$smartphone_hours,
  na.rm = TRUE
) + 0.25


# -------------------------------------------------------------------------
# 10. Function for mean and 95% confidence interval
# -------------------------------------------------------------------------

mean_ci <- function(x) {
  x <- x[!is.na(x)]
  
  n <- length(x)
  m <- mean(x)
  se <- sd(x) / sqrt(n)
  ci <- qt(.975, df = n - 1) * se
  
  data.frame(
    y = m,
    ymin = m - ci,
    ymax = m + ci
  )
}


# -------------------------------------------------------------------------
# 11. Common theme
# -------------------------------------------------------------------------

common_theme <- theme_ipsum(
  base_size = 13,
  axis_title_size = 14
) +
  theme(
    plot.background = element_rect(
      fill = bg_col,
      color = NA
    ),
    panel.background = element_rect(
      fill = bg_col,
      color = NA
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = grid_col,
      linewidth = 0.35
    ),
    strip.text = element_text(
      face = "bold",
      size = 11,
      margin = margin(6, 0, 6, 0)
    ),
    strip.background = element_rect(
      fill = "white",
      color = NA
    ),
    axis.text = element_text(
      color = base_col
    ),
    axis.title = element_text(
      color = base_col,
      size = 14
    ),
    plot.title = element_text(
      face = "bold",
      size = 18,
      color = base_col
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey30"
    ),
    plot.margin = margin(
      10,
      12,
      10,
      12
    )
  )


# -------------------------------------------------------------------------
# 12. Continuous plots with correlations
# -------------------------------------------------------------------------

p_cont <- ggplot(
  rq3_cont,
  aes(
    x = Score,
    y = smartphone_hours
  )
) +
  geom_point(
    color = accent_col,
    alpha = 0.5,
    size = 0.5
  ) +
  geom_smooth(
    method = "loess",
    color = base_col,
    fill = soft_col,
    linewidth = 1.05,
    se = TRUE
  ) +
  geom_text(
    data = rq3_correlations,
    aes(label = label),
    x = -Inf,
    y = Inf,
    hjust = -0.10,
    vjust = 1.15,
    size = 3.2,
    color = base_col,
    lineheight = 1.05,
    inherit.aes = FALSE
  ) +
  facet_wrap(
    ~Construct,
    scales = "free_x",
    ncol = 4
  ) +
  labs(
    x = NULL,
    y = "Average daily smartphone use (hours)"
  ) +
  common_theme


# -------------------------------------------------------------------------
# 13. Categorical plots with mean, 95% CI, mean, SD, and n
# -------------------------------------------------------------------------

p_cat <- ggplot(
  rq3_cat,
  aes(
    x = Group,
    y = smartphone_hours,
    fill = Group
  )
) +
  geom_violin(
    alpha = 0.75,
    color = NA,
    trim = FALSE,
    width = 0.9
  ) +
  stat_summary(
    fun.data = mean_ci,
    geom = "errorbar",
    width = 0.12,
    linewidth = 0.5,
    color = base_col
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2.2,
    color = base_col
  ) +
  geom_text(
    data = rq3_group_stats,
    aes(
      x = Group,
      y = cat_label_y,
      label = label
    ),
    size = 2.8,
    color = base_col,
    lineheight = 1,
    inherit.aes = FALSE
  ) +
  facet_wrap(
    ~Construct,
    scales = "free_x",
    ncol = 2
  ) +
  scale_fill_manual(
    values = cat_cols
  ) +
  scale_y_continuous(
    expand = expansion(
      mult = c(0.05, 0.20)
    )
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  common_theme +
  theme(
    legend.position = "none"
  )


# -------------------------------------------------------------------------
# 14. Combine plots
# -------------------------------------------------------------------------

p <- (p_cont / p_cat ) +
  plot_layout(
    heights = c(2, 1)
  ) +
  plot_annotation(
    title = paste(
      "Average Daily Smartphone Use, Well-Being,",
      "Gender, and Day Type"
    ),
    subtitle = NULL,
    theme = theme(
      plot.background = element_rect(
        fill = bg_col,
        color = NA
      )
    )
  )


# Display plot
p


# Save plot
dir.create(
  "Figures",
  showWarnings = FALSE
)

ggsave(
  filename = "Figures/smartphone_use_wellbeing.jpg",
  plot = p,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300,
  bg = bg_col
)




p <- (p_cont / p_cat / pa) +
  plot_layout(
    heights = c(1, 1.2, 1.5)
  ) +
  plot_annotation(
    title = paste(
      "Average Daily Smartphone Use, Well-Being,",
      "Gender, and Day Type"
    ),
    subtitle = NULL,
    theme = theme(
      plot.background = element_rect(
        fill = bg_col,
        color = NA
      )
    )
  )


ggsave(
  filename = "Figures/smartphone_use_wellbeing_2.jpg",
  plot = p,
  width = 9,
  height = 8,
  units = "in",
  dpi = 300,
  bg = bg_col
)
