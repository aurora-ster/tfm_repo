library(tidyverse)
library(lubridate)
library(scales)

# Load datasets
comments <- read_csv("comments_temporal.csv")
femicides <- read_csv("femicides_per_day.csv")

# Prepare sentiment data: aggregate by month
sentiment_monthly <- comments %>%
  mutate(date = ymd(date),
         month = floor_date(date, "month")) %>%
  count(month, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(total = positive + negative,
         positive = positive / total,
         negative = negative / total) %>%
  select(month, positive, negative)

# Prepare femicide data: aggregate by month
femicides_monthly <- femicides %>%
  mutate(date = ymd(date),
         month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(femicides = sum(femicides), .groups = "drop")

# Merge datasets
merged_monthly <- left_join(sentiment_monthly, femicides_monthly, by = "month") %>%
  replace_na(list(femicides = 0)) %>%
  mutate(femicides_scaled = femicides / 30)


# Plot
ggplot(merged_monthly, aes(x = month)) +
  geom_line(aes(y = negative, color = "Negative Sentiment"), size = 1.2) +
  geom_point(aes(y = femicides_scaled, shape = "Femicides"), color = "purple", size = 3) +
  scale_y_continuous(
    name = "Negative Sentiment Proportion",
    sec.axis = sec_axis(
      trans = ~ . * 30,
      name = "Number of Femicides",
      breaks = seq(0, 30, 5))
  ) +
  scale_color_manual(values = c("Negative Sentiment" = "blue")) +
  labs(title = "Monthly Negative Sentiment and Femicides in Italy (2024)",
       x = "Month", color = "Legend", shape = "") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
