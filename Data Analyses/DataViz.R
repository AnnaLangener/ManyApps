


library(dplyr)
library(ggplot2)
library(lubridate)




top_apps_by_agegroup <- read.csv("/Users/f007qrc/projects/ManyApps_Data/top_apps_by_agegroup.csv")
top_apps_by_gender <- read.csv("/Users/f007qrc/projects/ManyApps_Data/top_apps_by_gender.csv")
top_apps_by_sample <- read.csv("/Users/f007qrc/projects/ManyApps_Data/top_apps_by_sample.csv")

manyapps_hourly_noapp <- read.csv("/Users/f007qrc/projects/ManyApps_Data/Final_noapp_overview.csv")


###########

length(unique(manyapps_hourly_noapp$unique_participant_number))


###########

daily <- manyapps_hourly_noapp %>%
  group_by(Dataset, unique_participant_number, day) %>%
  summarise(
    total_daily_app_usage = first(total_daily_app_usage),
    .groups = "drop"
  )

# median minutes of application time across all applications per hour were 15.2 (per day = 364; daily range = 0 to 1,440)

mean(daily$total_daily_app_usage)/60/60
median(daily$total_daily_app_usage)/60/60

test = daily %>%
  group_by(Dataset) %>%
  summarise(
    mean_daily_usage = mean(total_daily_app_usage, na.rm = TRUE)/60/60,
    median_daily_usage = median(total_daily_app_usage, na.rm = TRUE)/60/60,
    .groups = "drop"
  )


#############

mean(na.omit(manyapps_hourly_noapp$total_hourly_app_usage))/60
median(na.omit(manyapps_hourly_noapp$total_hourly_app_usage))/60


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

plot_hour(manyapps_all_apps,NULL)

plot_hour <- function(manyapps_all_apps, dataset){

data = unique(manyapps_all_apps$Dataset)
# One plot: 7 weekday lines across 24 hours (with SD ribbon)
plot_df <- #manyapps_hourly_noapp[manyapps_all_apps$Dataset == dataset,] %>%
  manyapps_hourly_noapp%>%
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
    median_min = median(total_hourly_app_usage, na.rm = TRUE)/60,
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
  summarise(median_min = sum(median_min, na.rm = TRUE), .groups = "drop") %>%
  slice_max(median_min, n = 1)

min_hour_df <- plot_df %>%
  group_by(hour) %>%
  summarise(median_min = sum(median_min, na.rm = TRUE), .groups = "drop") %>%
  slice_min(median_min, n = 1)

plot_df <- plot_df %>% mutate( day_group = case_when( wday == "Friday" ~ "Friday", wday == "Saturday" ~ "Saturday", wday == "Sunday" ~ "Sunday", TRUE ~ "Weekday" ) )



pa  = ggplot(plot_df,
             aes(x = hour, y = median_min, group = wday)) +
  
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
}

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
    median_min = median(total_daily_app_usage, na.rm = TRUE)/60/60,
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
            aes(x = wday, y = median_min, group = 1)) +
  
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
    aes(ymin = median_min - sd_min,
        ymax = median_min + sd_min),
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
