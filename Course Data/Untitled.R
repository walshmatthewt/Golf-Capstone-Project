## Golf Capstone Project
setwd("/Users/Chris/Desktop/Data Science Capstone") 
# Loading the Libraries

library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(skimr)

# Loading the DataFrame

majors = c("Masters Tournament", "PGA Championship", 
           "U.S. Open", "The Open Championship")

data_pga = read.csv("ASA All PGA Raw Data - Tourn Level.csv")
data_players_of_interest = read.csv("Book1.csv")

# getting data
majors_data <- data_pga %>%
  filter(tournament.name %in% majors)

colnames(majors_data)

# make a working copy
majors_df <- majors_data

# check pos field
class(majors_df$pos)
summary(majors_df$pos)

# convert pos to numeric
# this handles cases like "T5" by turning them into 5
majors_df <- majors_df %>%
  mutate(pos_num = parse_number(as.character(pos)))

summary(majors_df$pos_num)

# keep only rows with valid position and SG data
majors_df <- majors_df %>%
  filter(!is.na(pos_num)) %>%
  filter(!is.na(sg_ott), !is.na(sg_app), !is.na(sg_arg), !is.na(sg_putt))


# fix the player filter using Player_initial_last
players_majors <- majors_df %>%
  filter(Player_initial_last %in% player_list) %>%
  select(Player_initial_last, tournament.name, season,
         sg_ott, sg_app, sg_arg, sg_putt, sg_total, pos) %>%
  rename(player = Player_initial_last) %>%
  arrange(player, season)

# check it worked
nrow(players_majors)
unique(players_majors$player)

# pivot longer for plotting
players_majors_long <- players_majors %>%
  pivot_longer(
    cols = c(sg_ott, sg_app, sg_arg, sg_putt, sg_total),
    names_to = "sg_type",
    values_to = "sg_value"
  )

# fix the player filter using Player_initial_last
players_majors <- majors_df %>%
  filter(Player_initial_last %in% player_list) %>%
  select(Player_initial_last, tournament.name, season,
         sg_ott, sg_app, sg_arg, sg_putt, sg_total, pos) %>%
  rename(player = Player_initial_last) %>%
  arrange(player, season) %>%
  mutate(pos_num = parse_number(as.character(pos))) %>%
  filter(!is.na(pos_num))

player_consistency <- players_majors %>%
  group_by(player) %>%
  summarise(
    avg_pos      = mean(pos_num, na.rm = TRUE),
    sd_pos       = sd(pos_num, na.rm = TRUE),
    avg_sg_total = mean(sg_total, na.rm = TRUE),
    sd_sg_total  = sd(sg_total, na.rm = TRUE),
    n_majors     = n(),
    .groups = "drop"
  ) %>%
  arrange(avg_pos)

# check it worked
nrow(players_majors)
unique(players_majors$player)

# pivot longer for plotting
players_majors_long <- players_majors %>%
  pivot_longer(
    cols = c(sg_ott, sg_app, sg_arg, sg_putt, sg_total),
    names_to = "sg_type",
    values_to = "sg_value"
  )

# plot SG trends over time per player
ggplot(players_majors_long, aes(x = season, y = sg_value, color = sg_type)) +
  geom_point(size = 2) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ player, scales = "free_y") +
  labs(
    title = "SG Metric Trends Across Majors Over Time",
    x = "Season",
    y = "Strokes Gained",
    color = "SG Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# convert pos to numeric first
players_majors <- players_majors %>%
  mutate(pos_num = parse_number(as.character(pos))) %>%
  filter(!is.na(pos_num))

# run the regression model
sg_pos_model <- lm(pos_num ~ sg_ott + sg_app + sg_arg + sg_putt, 
                   data = players_majors)
summary(sg_pos_model)

# plot each SG component vs finish position
players_majors_long_pos <- players_majors %>%
  pivot_longer(
    cols = c(sg_ott, sg_app, sg_arg, sg_putt),
    names_to = "sg_type",
    values_to = "sg_value"
  )

ggplot(players_majors_long_pos, aes(x = sg_value, y = pos_num)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "tomato") +
  scale_y_reverse() +
  facet_wrap(~ sg_type, scales = "free_x") +
  labs(
    title = "Which SG Metric Best Predicts Finish Position in Majors?",
    x = "Strokes Gained",
    y = "Finish Position (lower is better)"
  ) +
  theme_minimal()

# calculate mean and variance per player
player_consistency <- players_majors %>%
  group_by(player) %>%
  summarise(
    avg_pos     = mean(pos_num, na.rm = TRUE),
    sd_pos      = sd(pos_num, na.rm = TRUE),   # higher = less consistent
    avg_sg_total = mean(sg_total, na.rm = TRUE),
    sd_sg_total  = sd(sg_total, na.rm = TRUE),
    n_majors    = n(),
    .groups = "drop"
  ) %>%
  arrange(avg_pos)

print(player_consistency)

# plot consistency: average finish vs variability
ggplot(player_consistency, aes(x = sd_pos, y = avg_pos, label = player)) +
  geom_point(aes(size = n_majors), color = "steelblue", alpha = 0.8) +
  geom_text(vjust = -0.8, size = 3) +
  scale_y_reverse() +
  labs(
    title = "Major Consistency: Average Finish vs Variability",
    x = "Std Dev of Finish Position (higher = less consistent)",
    y = "Average Finish Position (lower is better)",
    size = "Majors Played"
  ) +
  theme_minimal()

# plot SG total consistency
ggplot(player_consistency, aes(x = reorder(player, sd_sg_total), y = sd_sg_total)) +
  geom_col(fill = "steelblue") +
  geom_col(aes(y = avg_sg_total), fill = "tomato", alpha = 0.6) +
  coord_flip() +
  labs(
    title = "SG Total: Average (red) vs Variability (blue) per Player",
    x = "Player",
    y = "Strokes Gained"
  ) +
  theme_minimal()
scale_color_manual(values = c(
  "S. Scheffler" = "gold",
  "J. Rahm"      = "steelblue",
  "R. McIlroy"   = "tomato",
  "C. Morikawa"  = "forestgreen",
  "T. Fleetwood" = "purple",
  "M. Fitzpatrick" = "orange",
  "C. Young"     = "hotpink",
  "X. Schauffele" = "cyan4"
))