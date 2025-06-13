# Awareness Detection

# Load libraries
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# ---- STEP 1: Prepare your comments data ----

# Load your dataset
comments <- read_csv("youtube_comments_with_sentiment.csv")

# Ensure date format
comments <- comments %>%
  mutate(
    published_at = as.Date(published_at),
    doc_id = as.character(row_number())  # unique ID for each comment
  )

# ---- STEP 2: Define time period around Nov 11, 2023 ----
key_date <- as.Date("2023-11-11")

# Filter 2 weeks before and 1 month after
awareness_set <- comments %>%
  filter(published_at >= (key_date - days(14)) & published_at <= (key_date %m+% months(1))) %>%
  mutate(period = case_when(
    published_at < key_date ~ "before",
    TRUE ~ "after"
  ))

# ---- STEP 3: Define awareness-related keywords ----
awareness_keywords <- c(
  "violenza di genere", "femminicidio", "patriarcato",
  "omicidio", "sistema", "cultura dello stupro", "colpa dell'uomo",
  "colpa della società", "denunciare", "rispetto per le donne",
  "è colpa del patriarcato", "dobbiamo cambiare", "problema serio",
  "è inaccettabile", "non è un caso isolato", "basta violenza",
  "le donne non sono al sicuro", "la società deve cambiare"
)

# Build regex pattern
pattern <- str_c(awareness_keywords, collapse = "|")

# ---- STEP 4: Detect Awareness (Rule-Based) ----
awareness_set <- awareness_set %>%
  mutate(
    text_lower = str_to_lower(comment),
    awareness = if_else(str_detect(text_lower, pattern), 1, 0)
  )

# ---- STEP 5: Plot Awareness Over Time ----
awareness_trend <- awareness_set %>%
  group_by(published_at, awareness) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(published_at) %>%
  mutate(pct = n / sum(n)) %>%
  filter(awareness == 1)

ggplot(awareness_trend, aes(x = published_at, y = pct)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_vline(xintercept = key_date, linetype = "dashed", color = "red") +
  labs(
    title = "Awareness in Comments Over Time",
    subtitle = "Based on Keyword Detection — 2 Weeks Before and 1 Month After Nov 11, 2023",
    x = "Date", y = "Proportion of Aware Comments"
  ) +
  theme_minimal()



######
# 🧪 Compare With Total Comments
daily_total <- awareness_set %>%
  group_by(published_at) %>%
  summarise(total_comments = n())

daily_aware <- awareness_set %>%
  filter(awareness == 1) %>%
  group_by(published_at) %>%
  summarise(aware_comments = n())

awareness_counts <- left_join(daily_total, daily_aware, by = "published_at") %>%
  mutate(aware_comments = coalesce(aware_comments, 0))

ggplot(awareness_counts, aes(x = published_at)) +
  geom_line(aes(y = total_comments), color = "dark blue", linetype = "dotted") +
  geom_line(aes(y = aware_comments), color = "darkgreen") +
  labs(title = "Total vs. Aware Comments per Day", y = "Comment Count", x = "Date") +
  theme_minimal()


###########
# ✅ 1. View Sample Comments on Peak Awareness Days
# First, identify the days with highest proportions of aware comments, then print samples to interpret the nature of awareness.

# Load necessary packages
library(dplyr)
library(lubridate)

# Assuming `comments_awareness` has: comment, published_at, and is_aware columns
# Convert to Date if not already
comments_awareness <- awareness_set %>%
  mutate(date = as.Date(published_at))

# Calculate daily proportions
daily_awareness <- comments_awareness %>%
  group_by(date) %>%
  summarise(
    total = n(),
    aware = sum(awareness),
    proportion_aware = aware / total
  ) %>%
  arrange(desc(proportion_aware))

# 🟢 Identify the top 3 days with highest awareness proportion
top_days <- daily_awareness %>% slice_max(proportion_aware, n = 3) %>% pull(date)
print(top_days)

# 🔍 Sample aware comments from each top day
for (day in top_days) {
  cat("\n\n🔸 Sample Aware Comments from", day, "\n")
  sample_comments <- comments_awareness %>%
    filter(awareness == TRUE, date == day) %>%
    pull(comment) %>%
    sample(min(5, length(.)))  # safe sampling
  
  print(sample_comments)
}


# ✅ 2. Topic Modeling on Aware Comments Only
# This helps uncover themes in how awareness is articulated.

# Load necessary libraries
library(tidytext)
library(topicmodels)
library(tm)

# Use doc_id as identifier for each comment
aware_dtm <- comments_awareness %>%
  select(doc_id, comment) %>%
  unnest_tokens(word, comment) %>%
  anti_join(get_stopwords(language = "it"), by = "word") %>%
  count(doc_id, word, sort = TRUE) %>%
  cast_dtm(document = doc_id, term = word, value = n)

# Fit LDA model (choose k as desired)
lda_aware <- LDA(aware_dtm, k = 2, control = list(seed = 1234))

# Inspect topics
comments_awareness <- tidy(lda_aware, matrix = "beta")

top_terms <- comments_awareness %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# View top terms per topic
top_terms

library(ggplot2)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ topic, scales = "free") +
  labs(
    title = "Top Terms in Aware Comments",
    x = "Probability (β)", y = NULL
  )
