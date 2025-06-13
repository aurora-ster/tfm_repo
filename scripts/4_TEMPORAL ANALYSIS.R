## ---- TEMPORAL ANALYSIS ----

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(topicmodels)
library(broom)
library(ggplot2)

# # Load your dataset
# comments <- read_csv("youtube_comments_with_sentiment.csv")

# Convert the 'published_at' column to a date format
comments$date <- as.Date(comments$published_at)

# Temporal Analysis: Aggregate sentiment by date
temporal_sentiment <- comments %>%
  group_by(date, sentiment) %>%
  count() %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

# Plot temporal trends in sentiment
ggplot(temporal_sentiment, aes(x = date)) +
  geom_line(aes(y = negative, color = "Negative")) +
  geom_line(aes(y = positive, color = "Positive")) +
  labs(title = "Temporal Trends in Sentiment", x = "Date", y = "Number of Comments", color = "Sentiment") +
  theme_minimal()


##########
# Convert published_at to proper datetime format
comments <- comments %>%
  mutate(
    published_at = as.POSIXct(published_at, format = "%Y-%m-%dT%H:%M:%SZ"),
    date = as.Date(published_at),
    hour = lubridate::hour(published_at),
    day_of_week = lubridate::wday(published_at, label = TRUE, week_start = 1),
    week = lubridate::week(published_at),
    month = lubridate::floor_date(date, "month")
  )

# 1. Time Series of Comment Volume
ggplot(comments, aes(x = date)) +
  geom_bar(stat = "count", fill = "steelblue") +
  labs(title = "Daily Comment Volume Over Time",
       x = "Date", y = "Number of Comments") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Sentiment Over Time (Daily)
daily_sentiment <- comments %>%
  count(date, sentiment) %>%
  group_by(date) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(daily_sentiment, aes(x = date, y = prop, color = sentiment)) +
  geom_line(size = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Daily Sentiment Proportions Over Time",
       x = "Date", y = "Proportion of Comments") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Sentiment Over Time (Rolling Average - 7 days)
rolling_sentiment <- daily_sentiment %>%
  group_by(sentiment) %>%
  mutate(rolling_avg = zoo::rollmean(prop, k = 7, fill = NA)) %>%
  ungroup()

ggplot(rolling_sentiment, aes(x = date, y = rolling_avg, color = sentiment)) +
  geom_line(size = 1) +
  labs(title = "7-Day Rolling Average of Sentiment Proportions",
       x = "Date", y = "Proportion (7-day avg)") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# 4. Hourly Patterns in Sentiment
hourly_sentiment <- comments %>%
  count(hour, sentiment) %>%
  group_by(hour) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(hourly_sentiment, aes(x = hour, y = prop, fill = sentiment)) +
  geom_col(position = "stack") +
  labs(title = "Hourly Distribution of Sentiment",
       x = "Hour of Day", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# 5. Day of Week Patterns
dow_sentiment <- comments %>%
  count(day_of_week, sentiment) %>%
  group_by(day_of_week) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

ggplot(dow_sentiment, aes(x = day_of_week, y = prop, fill = sentiment)) +
  geom_col(position = "stack") +
  labs(title = "Day of Week Distribution of Sentiment",
       x = "Day of Week", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# 6. Topic Prevalence Over Time (Monthly)
# (This builds on your existing topic modeling results)
topic_trends <- dominant_topics %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  count(month, sentiment, topic) %>%
  group_by(month, sentiment) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Plot topic trends by sentiment
ggplot(topic_trends, aes(x = month, y = prop, color = factor(topic))) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ sentiment, ncol = 1) +
  labs(title = "Topic Prevalence Over Time",
       x = "Month", y = "Proportion of Comments",
       color = "Topic") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Event Detection - Identify days with unusual sentiment patterns
sentiment_zscore <- daily_sentiment %>%
  group_by(sentiment) %>%
  mutate(
    z_score = (n - mean(n)) / sd(n),
    is_outlier = abs(z_score) > 2
  ) %>%
  ungroup()

# Plot with outliers highlighted
ggplot(sentiment_zscore, aes(x = date, y = n, color = sentiment)) +
  geom_line(alpha = 0.5) +
  geom_point(data = filter(sentiment_zscore, is_outlier), 
             aes(size = abs(z_score))) +
  labs(title = "Daily Comment Volume with Outliers Highlighted",
       x = "Date", y = "Number of Comments",
       size = "Z-Score") +
  facet_wrap(~ sentiment, ncol = 1, scales = "free_y") +
  theme_minimal()

# Show top outlier days
sentiment_outliers <- sentiment_zscore %>%
  filter(is_outlier) %>%
  arrange(desc(abs(z_score))) %>%
  select(date, sentiment, n, z_score)

print(sentiment_outliers)
