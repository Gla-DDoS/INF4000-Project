# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
team_stats <- read.csv("team_stats.csv")

# Chart 1: Distribution of Team Statistics (Home and Away)

chart1_data <- team_stats %>%
  group_by(IsHomeTeam) %>%
  summarise(
    Goals = sum(Goals, na.rm = TRUE),
    AttemptsOnTarget = sum(Attempts.on.target, na.rm = TRUE),
    BallPossession = mean(Ball.Possession, na.rm = TRUE),
    PassAccuracy = mean(Passes.accuracy, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Goals, AttemptsOnTarget, BallPossession, PassAccuracy),
               names_to = "Statistic", values_to = "Value")


chart1 <- ggplot(chart1_data, aes(x = Statistic, y = Value, fill = IsHomeTeam)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparison of Home vs. Away Team Statistics",
    x = "Statistic",
    y = "Value",
    fill = "Home Team"
  ) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                    labels = c("Away", "Home")) +
  theme_minimal()

print(chart1)

# Chart 2 Ball Possession and Goals Scored
chart2_data <- team_stats %>%
  group_by(MatchID, IsHomeTeam) %>%
  summarise(BallPossession = mean(Ball.Possession, na.rm = TRUE),
            Goals = sum(Goals, na.rm = TRUE))

chart2 <- ggplot(chart2_data, aes(x = BallPossession, y = Goals, colour = IsHomeTeam)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Ball Possession and Goals Scored", x = "Ball Possession (%)", y = "Goals Scored") +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
  theme_minimal()

print(chart2)

# Chart 3: Proportion of Matches Won (Home vs. Away Teams)
chart3_data <- team_stats %>%
  group_by(MatchID) %>%
  summarise(
    HomeGoals = sum(Goals[IsHomeTeam == TRUE], na.rm = TRUE),
    AwayGoals = sum(Goals[IsHomeTeam == FALSE], na.rm = TRUE)
  ) %>%
  mutate(Winner = case_when(
    HomeGoals > AwayGoals ~ "Home Win",
    AwayGoals > HomeGoals ~ "Away Win",
    TRUE ~ "Draw"
  )) %>%
  count(Winner) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the pie chart
chart3 <- ggplot(match_outcomes, aes(x = "", y = Percentage, fill = Winner)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(
    title = "Proportion of Matches Won (Home vs. Away Teams)",
    fill = "Match Outcome"
  ) +
  theme_void() + # Clean layout
  scale_fill_manual(values = c("Home Win" = "blue", "Away Win" = "red", "Draw" = "gray")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))


print(chart3)

# Chart 4: Box plot for Red and Yellow Cards by Home vs. Away Teams

chart4 <- team_stats %>%
  select(IsHomeTeam, Yellow.Cards, Red.Cards) %>%
  pivot_longer(cols = c(Yellow.Cards, Red.Cards), names_to = "CardType", values_to = "Count")


boxplot_cards <- ggplot(chart4, aes(x = CardType, y = Count, fill = IsHomeTeam)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Distribution of Yellow and Red Cards (Home vs. Away Teams)",
    x = "Card Type",
    y = "Number of Cards",
    fill = "Home Team"
  ) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), labels = c("Away", "Home")) +
  theme_minimal()



print(chart4)
