## IPL DATA ANALYSIS ASSIGNMENT ##
# Set CRAN mirror to avoid install error when rendering
options(repos = c(CRAN = "https://cran.rstudio.com"))
# 1. Load Required Libraries ####

install.packages(c("dplyr", "ggplot2", "readr", "fitdistrplus", "ggpubr", "stringr"))
library(dplyr)
library(ggplot2)
library(readr)
library(fitdistrplus)
library(ggpubr)
library(stringr)

# 2. Load the IPL Dataset ####
ipl_data <- read_csv("C:/Users/sraho/OneDrive/Desktop/A2/Data/IPL_ball_by_ball_updated till 2024.csv")

# Clean column names and filter NA rows
ipl_data <- ipl_data %>% filter(!is.na(Striker), !is.na(Bowler))
ipl_data <- ipl_data %>%
  mutate(season_year = as.numeric(str_sub(as.character(Season), 1, 4)))

# 3. Arrange the data round-wise: runs and wickets per player per match ####
match_summary <- ipl_data %>%
  group_by(Season, `Match id`, Striker, Bowler) %>%
  summarise(
    runs = sum(runs_scored, na.rm = TRUE),
    wickets = sum(wicket_confirmation, na.rm = TRUE),
    .groups = 'drop'
  )

# 4. Top 3 Run-Getters and Wicket-Takers Per IPL Round ####
top_batsmen <- ipl_data %>%
  group_by(Season, Striker) %>%
  summarise(total_runs = sum(runs_scored, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Season) %>%
  top_n(3, total_runs)

top_bowlers <- ipl_data %>%
  filter(wicket_confirmation == 1) %>%
  group_by(Season, Bowler) %>%
  summarise(total_wickets = n(), .groups = 'drop') %>%
  group_by(Season) %>%
  top_n(3, total_wickets)

# 5. Fit Best Distribution for Top Performers (Poisson) ####

top_batsmen_data <- ipl_data %>% filter(Striker %in% top_batsmen$Striker)
fit_runs <- fitdist(top_batsmen_data$runs_scored, "pois")
plot(fit_runs)

top_bowlers_data <- ipl_data %>%
  filter(Bowler %in% top_bowlers$Bowler & wicket_confirmation == 1)

wickets_per_match <- top_bowlers_data %>%
  group_by(Bowler, `Match id`) %>%
  summarise(wickets = n(), .groups = 'drop')

fit_wickets <- fitdist(wickets_per_match$wickets, "pois")
plot(fit_wickets)

# 6. Best Fit Distribution for Assigned Player: Shubman Gill (2022–2024) ####

gill_data <- ipl_data %>%
  filter(Striker == "Shubman Gill", season_year %in% c(2022, 2023, 2024)) %>%
  group_by(`Match id`) %>%
  summarise(runs = sum(runs_scored), .groups = 'drop')

if (nrow(gill_data) >= 5 && all(!is.na(gill_data$runs)) && var(gill_data$runs) > 0) {
  fit_gill_pois <- fitdist(gill_data$runs, "pois")
  fit_gill_norm <- fitdist(gill_data$runs, "norm")
  fit_gill_gamma <- fitdist(gill_data$runs, "gamma")
  
  # Compare and plot ####
  
  print(gofstat(list(Poisson = fit_gill_pois, Normal = fit_gill_norm, Gamma = fit_gill_gamma)))
  plot(fit_gill_pois)
  plot(fit_gill_norm)
  plot(fit_gill_gamma)
} else {
  cat("⚠️ Not enough data to fit distributions for Shubman Gill.\n")
}

# 7. Correlation Between Player’s Performance and Salary ####

# Mock salary data (replace with real data if available)
set.seed(123)
salary_data <- data.frame(
  Player = unique(c(top_batsmen$Striker, top_bowlers$Bowler)),
  Salary = sample(50:300, length(unique(c(top_batsmen$Striker, top_bowlers$Bowler))), replace = TRUE)
)

# Combine performance data
performance_salary <- top_batsmen %>%
  rename(Player = Striker, Performance = total_runs) %>%
  bind_rows(top_bowlers %>% rename(Player = Bowler, Performance = total_wickets)) %>%
  left_join(salary_data, by = "Player")

# Correlation plot and result
ggplot(performance_salary, aes(x = Performance, y = Salary)) +
  geom_point() +  # just the points
  geom_text(aes(label = Player), vjust = -1, size = 3) +  # labels
  geom_smooth(method = "lm", color = "blue") +  # regression line
  ggtitle("Player Performance vs Salary")


# Print correlation value ####

cor_value <- cor(performance_salary$Performance, performance_salary$Salary, use = "complete.obs")
cat("\n📊 Correlation between Performance and Salary:", cor_value, "\n")

