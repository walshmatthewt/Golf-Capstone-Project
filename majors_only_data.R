setwd("~/Documents/GitHub/Golf-Capstone-Project")
data = read.csv("ASA All PGA Raw Data - Tourn Level.csv")

majors = c("Masters Tournament", "PGA Championship", 
            "U.S. Open", "The Open Championship")

# getting data
majors_data <- data %>%
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

# selecting only needed columns
majors_long <- majors_df %>%
  select(player, pos_num, sg_ott, sg_app, sg_arg, sg_putt) %>%
  pivot_longer(
    cols = c(sg_ott, sg_app, sg_arg, sg_putt),
    names_to = "sg_type",
    values_to = "sg_value"
  )

# strokes gained to finish position comparison
ggplot(majors_long, aes(x = sg_value, y = pos_num)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_reverse() +
  facet_wrap(~ sg_type, scales = "free_x") +
  labs(
    title = "Strokes Gained Components vs Finish Position in Majors",
    x = "Strokes Gained",
    y = "Finish Position (lower is better)"
  )

# player level graph checks
nrow(majors_df)
summary(majors_df$player)
table(is.na(majors_df$player))

player_summary <- majors_df %>%
  filter(!is.na(player), !is.na(pos_num), !is.na(sg_app)) %>%
  group_by(player) %>%
  summarise(
    avg_pos = mean(pos_num, na.rm = TRUE),
    sg_ott = mean(sg_ott, na.rm = TRUE),
    sg_app = mean(sg_app, na.rm = TRUE),
    sg_arg = mean(sg_arg, na.rm = TRUE),
    sg_putt = mean(sg_putt, na.rm = TRUE),
    sg_t2g = mean(sg_t2g, na.rm = TRUE),
    sg_total = mean(sg_total, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# stroke type positions gained vs finish position
ggplot(player_summary, aes(x = sg_t2g, y = avg_pos)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_reverse() +
  labs(
    title = "Average SG: Tee to Green vs Average Finish Position in Majors",
    x = "Average SG: Tee to Green",
    y = "Average Finish Position"
  )
