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
    title = "Average Daily Smartphone Use",
    x = "Person-specific mean daily use (hours)",
    y = "Number of participants"
  ) +
  theme_ipsum(base_size = 10)

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
      "Participants' smartphone use\nchanged by about ",
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
  theme_ipsum(base_size = 10)

p_rq1 <- p_mean + p_sd 

p_rq1

ggsave(
  filename = "RQ1_daily_smartphone_use_distribution.jpg",
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
    title = "Average Number of Apps Used per Day",
    x = "Person-specific mean number of apps",
    y = "Number of participants"
  ) +
  theme_ipsum(base_size = 10)

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
  theme_ipsum(base_size = 10)

p_rq1_apps <- p_mean + p_sd
p_rq1_apps

ggsave(
  filename = "RQ2_daily_app_diversity_distribution.jpg",
  plot = p_rq1_apps,
  width = 9,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "white"
)

############################################################
################## Research Question 3 #####################
############################################################
## Descreptive plots

rq3_overview <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    study_average_daily_app_usage,
    PANAS_NEG,
    PANAS_POS,
    STRESS,
    SWLS,
    PHQ
  ) %>%
  group_by(Dataset) %>%
  summarise(
    n = n(),
    
    app_usage_min = min(study_average_daily_app_usage, na.rm = TRUE),
    app_usage_max = max(study_average_daily_app_usage, na.rm = TRUE),
    
    PANAS_NEG_min = min(PANAS_NEG, na.rm = TRUE),
    PANAS_NEG_max = max(PANAS_NEG, na.rm = TRUE),
    
    PANAS_POS_min = min(PANAS_POS, na.rm = TRUE),
    PANAS_POS_max = max(PANAS_POS, na.rm = TRUE),
    
    STRESS_min = min(STRESS, na.rm = TRUE),
    STRESS_max = max(STRESS, na.rm = TRUE),
    
    SWLS_min = min(SWLS, na.rm = TRUE),
    SWLS_max = max(SWLS, na.rm = TRUE),
    
    PHQ_min = min(PHQ, na.rm = TRUE),
    PHQ_max = max(PHQ, na.rm = TRUE)
  )
library(tidyverse)
library(hrbrthemes)
library(patchwork)
library(lubridate)
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

# 1. Participant-level data: well-being + gender
rq3_participant <- manyapps_hourly_noapp %>%
  arrange(unique_participant_number, day) %>%
  distinct(unique_participant_number, .keep_all = TRUE) %>%
  mutate(
    smartphone_hours = study_average_daily_app_usage / 3600,
    Gender = factor(gender)
  )

# 2. Participant-level weekday vs weekend averages
rq3_day <- manyapps_hourly_noapp %>%
  distinct(unique_participant_number, day, total_daily_app_usage) %>%
  mutate(
    day = as.Date(day),
    smartphone_hours = total_daily_app_usage / 3600,
    Day = if_else(
      wday(day, week_start = 1) %in% 1:5,
      "Weekday",
      "Weekend"
    )
  ) %>%
  group_by(unique_participant_number, Day) %>%
  summarise(
    smartphone_hours = mean(smartphone_hours, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Day = factor(Day, levels = c("Weekday", "Weekend"))
  )

# 3. Continuous well-being panels
rq3_cont <- rq3_participant %>%
  pivot_longer(
    cols = c(PANAS_POS, PANAS_NEG, STRESS, SWLS, PHQ),
    names_to = "Construct",
    values_to = "Score"
  ) %>%
  mutate(
    Construct = factor(
      Construct,
      levels = c("PANAS_POS", "PANAS_NEG", "STRESS", "SWLS", "PHQ"),
      labels = c(
        "Positive Affect",
        "Negative Affect",
        "Stress",
        "Life Satisfaction",
        "Depressive Symptoms"
      )
    )
  )

# 4. Gender panel
rq3_cat_gender <- rq3_participant %>%
  transmute(
    Construct = "Gender",
    Group = Gender,
    smartphone_hours
  )

# 5. Weekday/weekend panel
rq3_cat_day <- rq3_day %>%
  transmute(
    Construct = "Weekday vs. Weekend",
    Group = Day,
    smartphone_hours
  )

rq3_cat <- bind_rows(rq3_cat_gender, rq3_cat_day) %>%
  filter(!is.na(Group), !is.na(smartphone_hours)) %>%
  mutate(
    Construct = factor(
      Construct,
      levels = c("Gender", "Weekday vs. Weekend")
    )
  )

common_theme <- theme_ipsum(base_size = 13, axis_title_size = 12) +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = grid_col, linewidth = 0.35),
    strip.text = element_text(face = "bold", size = 11, margin = margin(6, 0, 6, 0)),
    strip.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = base_col),
    axis.title = element_text(color = base_col),
    plot.title = element_text(face = "bold", size = 18, color = base_col),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    plot.margin = margin(10, 12, 10, 12)
  )

# 6. Continuous plots
p_cont <- ggplot(rq3_cont, aes(x = Score, y = smartphone_hours)) +
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
  facet_wrap(~ Construct, scales = "free_x", ncol = 3) +
  labs(
    x = NULL,
    y = "Average daily smartphone use (hours)"
  ) +
  common_theme

# 7. Categorical plots with mean + 95% CI
p_cat <- ggplot(rq3_cat, aes(x = Group, y = smartphone_hours, fill = Group)) +
  geom_violin(
    alpha = 0.75,
    color = NA,
    trim = FALSE,
    width = 0.9
  ) +
  stat_summary(
    fun.data = mean_cl_normal,
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
  facet_wrap(~ Construct, scales = "free_x", ncol = 2) +
  scale_fill_manual(values = cat_cols) +
  labs(
    x = NULL,
    y = NULL
  ) +
  common_theme +
  theme(
    legend.position = "none"
  )

# 8. Combine
p <- (p_cont / p_cat) +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    title = "Average Daily Smartphone Use, Well-Being, Gender, and Day Type",
    subtitle = NULL,
    theme = theme(
      plot.background = element_rect(fill = bg_col, color = NA)
    )
  )

ggsave(
  filename = "smartphone_use_wellbeing.jpg",
  plot = p,
  width = 8,
  height = 7,
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

make_app_heatmap <- function(dat, x_var, title) {
  
  plot_dat <- dat %>%
    filter(n_people_app >= 20, rank <= 10)
  
  app_order <- plot_dat %>%
    group_by(Package_name) %>%
    summarise(
      overall_share = sum(mean_share_group, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(overall_share)) %>%
    pull(Package_name)
  
  plot_dat <- plot_dat %>%
    mutate(
      Package_name = factor(Package_name, levels = rev(app_order))
    )
  
  ggplot(
    plot_dat,
    aes(
      x = {{ x_var }},
      y = Package_name,
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
    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    theme_ipsum(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", color = base_col),
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

min(top_apps_by_sample$mean_share_group[top_apps_by_sample$Package_name == "com.whatsapp"])
max(top_apps_by_sample$mean_share_group[top_apps_by_sample$Package_name == "com.whatsapp"])


ggsave(
  "top_apps_agegroup.png",
  p_age,
  width = 8,
  height = 4,
  dpi = 300,
  bg = "white"
)


ggsave(
  "top_apps_gender.png",
  p_gender,
  width = 8,
  height = 4,
  dpi = 300,
  bg = "white"
)

ggsave(
  "top_apps_sample.png",
  p_sample,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)


###################



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
    color = "#457B9D",
    size = 3
  ) +
  
  # saturday
  geom_point(
    data = subset(plot_df, day_group == "Saturday"),
    color = "#2A9D8F",
    size = 3
  ) +
  
  # sunday
  geom_point(
    data = subset(plot_df, day_group == "Sunday"),
    color = "#E76F51",
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

pb

library(patchwork)

pa / pb + 
  plot_layout(heights = c(2, 1))








library(dplyr)
library(ggplot2)
library(hrbrthemes)

####################### Differences Vizualizations ##################





##### STRESS
rq3 <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    study_average_daily_app_usage,
    STRESS,
  ) 

ggplot(rq3 , aes(x = STRESS, y = study_average_daily_app_usage/60/60)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Average Daily Smartphone Use",
    y = "Average daily smartphone use (hours)"
  ) +
  theme_ipsum()

######## SWLS
rq3 <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    study_average_daily_app_usage,
    SWLS,
  ) 

ggplot(rq3 , aes(x = SWLS, y = study_average_daily_app_usage/60/60)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Average Daily Smartphone Use",
    y = "Average daily smartphone use (hours)"
  ) +
  theme_ipsum()

#### Depression
rq3 <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    study_average_daily_app_usage,
    PHQ,
  ) 

ggplot(rq3 , aes(x = PHQ, y = study_average_daily_app_usage/60/60)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Average Daily Smartphone Use",
    y = "Average daily smartphone use (hours)"
  ) +
  theme_ipsum()


# Higher depressive symptoms -> more smartphone use


ggplot(manyapps_daily, aes(x = Dataset, y = PHQ, fill = Dataset)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.12, outlier.alpha = 0.2) +
  labs(
    title = "PHQ Scores by Dataset",
    x = "Dataset",
    y = "PHQ Score"
  ) +
  theme_ipsum() +
  theme(legend.position = "none")

ggplot(manyapps_daily, aes(x = Dataset, y = PHQ, color = Dataset)) +
  geom_jitter(width = 0.15, alpha = 0.15) +
  geom_boxplot(width = 0.2, alpha = 0.4, outlier.shape = NA) +
  labs(
    title = "PHQ Scores by Dataset",
    x = "Dataset",
    y = "PHQ Score"
  ) +
  theme_ipsum() +
  theme(legend.position = "none")
##### Location Scale Model Try out
library(brms)
library(dplyr)

library(brms)
library(dplyr)

manyapps_hourly_noapp <- manyapps_hourly_noapp %>%
  mutate(
    unique_participant_number = factor(unique_participant_number),
    Dataset = factor(Dataset),
    gender = factor(gender),
    hour_start = factor(hour_start),
    weekend = as.integer(as.POSIXlt(as.Date(day))$wday %in% c(0, 6)),
    
    hourly_usage_minutes = total_hourly_app_usage / 60,
    hourly_usage_log = log1p(hourly_usage_minutes),
    
    daily_usage_minutes = total_daily_app_usage / 60,
    daily_usage_log = log1p(daily_usage_minutes),
    
    unique_apps_day_log = log1p(unique_apps_day),
    unique_apps_hour_log = log1p(unique_apps_hour)
  )

manyapps_daily <- manyapps_hourly_noapp %>%
  distinct(
    unique_participant_number, day, Dataset, age, gender, PHQ,
    weekend, total_daily_app_usage, study_average_daily_app_usage,
    unique_apps_day
  ) %>%
  mutate(
    daily_usage_minutes = total_daily_app_usage / 60,
    daily_usage_log = log1p(daily_usage_minutes),
    unique_apps_day_log = log1p(unique_apps_day)
  )

m_daily_usage_locscale <- brm(
  bf(
    daily_usage_log ~ weekend + age + gender + PHQ +
      (1 | unique_participant_number),
    
    sigma ~ age + gender + PHQ  + Dataset
  ),
  data = manyapps_daily,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 4000,
  seed = 123
)

summary(m_daily_usage_locscale)


#########


# Participant-level averages for PHQ
rq3 <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    study_average_daily_app_usage,
    age,
  ) 

ggplot(rq3 , aes(x = age, y = study_average_daily_app_usage/60/60)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Average Daily Smartphone Use",
    y = "Average daily smartphone use (hours)"
  ) +
  theme_ipsum()



# Participant-level averages for PHQ
rq3 <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    study_average_daily_app_usage,
    gender,
  ) 

ggplot(rq3 , aes(x = gender, y = study_average_daily_app_usage/60/60)) +
  geom_boxplot(alpha = 0.35) +
  #geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Average Daily Smartphone Use",
    y = "Average daily smartphone use (hours)"
  ) +
  theme_ipsum()


# Participant-level averages for PHQ
rq3 <- manyapps_hourly_noapp %>%
  distinct(
    Dataset,
    unique_participant_number,
    study_average_daily_app_usage,
    age_group,
  ) 

ggplot(rq3 , aes(x = age_group, y = study_average_daily_app_usage/60/60)) +
  geom_boxplot(alpha = 0.35) +
  #geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Average Daily Smartphone Use",
    y = "Average daily smartphone use (hours)"
  ) +
  theme_ipsum()




#  For categorical variables (b,e), including gender and weekday versus weekend,
# violin plots were used to visualize the distribution of daily smartphone use, with means and 95% confidence intervals displayed. 




library(dplyr)
library(lubridate)
library(ggplot2)

daily <- manyapps_hourly_noapp %>%
  mutate(day = as.Date(day)) %>%
  group_by(Dataset, unique_participant_number, day) %>%
  summarise(
    total_daily_app_usage = first(total_daily_app_usage),
    total_daily_app_usage_hr = first(total_daily_app_usage) / 60 / 60,
    .groups = "drop"
  )

usage_by_year <- daily %>%
  mutate(year = year(day)) %>%
  group_by(year) %>%
  summarise(
    mean_daily_usage_hr = mean(total_daily_app_usage_hr, na.rm = TRUE),
    median_daily_usage_hr = median(total_daily_app_usage_hr, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  )

ggplot(usage_by_year, aes(x = year, y = mean_daily_usage_hr)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Mean daily app usage (hours)",
    title = "Average daily app usage over years"
  ) +
  theme_minimal()




usage_by_year_dataset <- daily %>%
  mutate(year = year(day)) %>%
  group_by(Dataset, year) %>%
  summarise(
    mean_daily_usage_hr = mean(total_daily_app_usage_hr, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(usage_by_year_dataset, aes(x = year, y = mean_daily_usage_hr, group = Dataset)) +
  geom_line(aes(color = Dataset)) +
  geom_point(aes(color = Dataset)) +
  labs(
    x = "Year",
    y = "Mean daily app usage (hours)",
    title = "Average daily app usage over years by dataset"
  ) +
  theme_minimal()
