# ANALYSIS 

## ---- STEP 1: Python: Assign Sentiment (already done)----

# 📦 Load required libraries
library(tidyverse)
library(readr)
library(tidytext)
library(stopwords)
library(udpipe)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(tm)
library(broom)
library(textmineR)
library(zoo)


## ---- STEP 2 ----
# 🔁 Load Data and Explore Sentiment Distribution

# Read the CSV file
comments <- read_csv("youtube_comments_with_sentiment.csv")

# View structure
glimpse(comments)

# 📊 Visualize Sentiment Distribution
comments %>%
  count(sentiment) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  labs(title = "Distribution of Sentiment in Comments",
       x = "Sentiment", y = "Number of Comments") +
  theme_minimal()

# comments %>%
#   count(sentiment) %>%
#   ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
#   geom_col() +
#   scale_y_continuous(
#     name = "Number of Comments",
#     breaks = seq(0, 20000, 5000),  # Adjust based on your data range
#     labels = scales::comma
#   ) +
#   labs(title = "Distribution of Sentiment in Comments",
#        x = "Sentiment") +
#   theme_minimal()



## ---- STEP 3 ----
# 🧹3.1 - Text Cleaning, Tokenization, Stopword Removal

# Ensure comment is character
comments$comment <- as.character(comments$comment)

# Add unique ID to each comment
comments <- comments %>%
  mutate(doc_id = as.character(row_number()))

# Tokenize
comments_tokens <- comments %>%
  unnest_tokens(word, comment)

# 🧹 3.2 - Stopwords Removal

# Load Italian stopwords from `stopwords` package
stopwords_it <- data.frame(word = stopwords("it"))

# Add custom stopwords relevant to your dataset
custom_stopwords <- data.frame(
  word = c("fare", "essere", "cosa", "video", "solo", 
           "può", "puoi", "anni", "molto", "sempre",
           "poi", "fa", "quando", "de", "te", "no", 
           "que", "così", "delle", "quello", 
           "cosa", "vedere", "anno", "es", "como")
)

# Combine both stopword lists
combined_stopwords <- bind_rows(stopwords_it, custom_stopwords)

# Remove stopwords and short words
filtered_data <- comments_tokens %>%
  anti_join(combined_stopwords, by = "word") %>%
  filter(nchar(word) > 3) 

## ---- STEP 4 ----
# 🧠 Lemmatization and POS Tagging with UDPipe

# Download and load the Italian UDPipe model
ud_model <- udpipe_download_model(language = "italian")
ud_model <- udpipe_load_model(ud_model$file_model)

# Annotate original comments
anno <- udpipe_annotate(ud_model, x = comments$comment, doc_id = comments$doc_id)
anno_df <- as.data.frame(anno)

# Merge sentiment info back in
anno_df <- anno_df %>%
  left_join(comments %>% select(doc_id, sentiment), by = "doc_id")

# Filter for meaningful POS and use lemmatized form + Remove any missing lemmas or blank words
lemmatized_data <- anno_df %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>%
  rename(word = lemma) %>%
  filter(!is.na(word), word != "")


## ---- STEP 5 ----
# 🏷 Gendered Language Analysis

# Define a custom gender-related keyword list
gender_keywords <- c("donna", "donnae", "uomo", "femminicidio", "violenza", 
                     "maschio", "femmina", "colpa", "madre", "padre", 
                     "ragazza", "ragazzo", "colpevole", "vittima", "killer", "giustizia")

gender_mentions <- lemmatized_data %>%
  filter(word %in% gender_keywords)

# Frequency by sentiment
gender_mentions %>%
  count(sentiment, word, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Gender-Related Words by Sentiment",
       x = "Keyword", y = "Frequency") +
  theme_minimal()


## ---- STEP 6 (OPTIONAL) ----
# # 💡 Word Frequency and Word Clouds
# set.seed(123)
# 
# # Make wordcloud for Negative comments only
# lemmatized_data %>%
#   filter(sentiment == "negative") %>%
#   count(word, sort = TRUE) %>%
#   with(wordcloud(word, n, max.words = 100, colors = brewer.pal(8, "Reds")))
# 
# # Make wordcloud for Positive comments only
# lemmatized_data %>%
#   filter(sentiment == "positive") %>%
#   count(word, sort = TRUE) %>%
#   with(wordcloud(word, n, max.words = 100, colors = brewer.pal(8, "Blues")))


## ---- STEP 7 ----
# 🤖 Topic Modeling by Sentiment

# Check the frequency of top words before LDA:
lemmatized_data %>%
  count(word, sort = TRUE) %>%
  head(30)

# 7.1 - Prepare DTM for Each Sentiment

# Negative
neg_comments <- lemmatized_data %>%
  filter(sentiment == "negative") %>%
  mutate(document = doc_id)

dtm_neg <- neg_comments %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

# Positive
pos_comments <- lemmatized_data %>%
  filter(sentiment == "positive") %>%
  mutate(document = doc_id)

dtm_pos <- pos_comments %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

# 7.2 - Run LDA
k <- 4

lda_neg <- LDA(dtm_neg, k = k, control = list(seed = 1234))
lda_pos <- LDA(dtm_pos, k = k, control = list(seed = 1234))

# 7.3 - Visualize Top Terms Per Topic

# Negative
top_terms_neg <- tidy(lda_neg, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

ggplot(top_terms_neg, aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Terms per Topic (Negative Sentiment)",
    x = NULL, y = "Probability (Beta)"
  )

# Positive
top_terms_pos <- tidy(lda_pos, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

ggplot(top_terms_pos, aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Terms per Topic (Positive Sentiment)",
    x = NULL, y = "Probability (Beta)"
  )

# 7.4 - Which K (# of topics) is best?

# Create a text vector where each element is a document (one lemmatized comment)
text_vector <- lemmatized_data %>%
  group_by(doc_id) %>%
  summarise(text = paste(word, collapse = " ")) %>%
  arrange(as.numeric(doc_id)) %>%  # Ensure correct order
  pull(text)

# Create DTM or use your cleaned DTM
dtm <- CreateDtm(doc_vec = text_vector,
                 doc_names = paste0("doc", 1:length(text_vector)),
                 ngram_window = c(1, 2))

# Fit LDA models with varying k
k_seq <- 2:6
models <- lapply(k_seq, function(k) FitLdaModel(dtm, k = k, iterations = 100))

# Coherence scores
coherences <- sapply(models, function(m) mean(m$coherence))

# Plot
plot(k_seq, coherences, type = "b", col = "blue",
     main = "Topic Coherence vs Number of Topics (k)",
     xlab = "Number of Topics", ylab = "Coherence")

# (Choose the k with the highest or plateauing coherence score.)

# 7.5 - we re-run the LDA with k = 2 (separately for both negative and positive sentiments).
k <- 2

lda_neg_k2 <- LDA(dtm_neg, k = k, control = list(seed = 1234))
lda_pos_k2 <- LDA(dtm_pos, k = k, control = list(seed = 1234))

# Negative
top_terms_neg_k2 <- tidy(lda_neg_k2, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

ggplot(top_terms_neg_k2, aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Terms per Topic (Negative Sentiment), k=2",
    x = NULL, y = "Probability (Beta)"
  )

# Positive
top_terms_pos_k2 <- tidy(lda_pos_k2, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic))

ggplot(top_terms_pos_k2, aes(x = term, y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Terms per Topic (Positive Sentiment), k=2",
    x = NULL, y = "Probability (Beta)"
  )

## ---- STEP 8 ----
# 🤖 Assign Dominant Topic per Document and Temporal Analysis

# Ensure doc_id is assigned and used consistently
comments <- comments %>%
  mutate(doc_id = as.character(row_number()))

# Negative
gamma_neg <- tidy(lda_neg, matrix = "gamma") %>%
  mutate(sentiment = "negative")

docs_neg <- comments %>%
  select(doc_id, published_at) %>%
  rename(document = doc_id)

gamma_doc_neg <- gamma_neg %>%
  inner_join(docs_neg, by = "document")

dominant_neg <- gamma_doc_neg %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

# Positive
gamma_pos <- tidy(lda_pos, matrix = "gamma") %>%
  mutate(sentiment = "positive")

docs_pos <- comments %>%
  select(doc_id, published_at) %>%
  rename(document = doc_id)

gamma_doc_pos <- gamma_pos %>%
  inner_join(docs_pos, by = "document")

dominant_pos <- gamma_doc_pos %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

# Make sure 'document' is the same type in both frames
dominant_pos <- dominant_pos %>% mutate(document = as.character(document))
dominant_neg <- dominant_neg %>% mutate(document = as.character(document))
comments <- comments %>% mutate(document = as.character(row_number()))

# Combine and join
dominant_topics <- bind_rows(dominant_pos, dominant_neg)

# Join with comments to get text and metadata (avoid duplicate sentiment columns)
dominant_topics <- dominant_topics %>%
  left_join(comments %>% select(doc_id, comment), by = c("document" = "doc_id"))

# Rename and clean
dominant_topics <- dominant_topics %>%
  rename(date = published_at)

# # Parse and aggregate time
# dominant_topics <- dominant_topics %>%
#   mutate(
#     date = as.Date(date),
#     month = floor_date(date, unit = "quarter")  # or "2 months" if using lubridate > v1.8
#   )
# 
# # Summarize topic frequency by month
# temporal_topics <- dominant_topics %>%
#   group_by(month, topic, sentiment) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   filter(count >= 5)  # threshold based on your data scale
# 
# # Plot heatmap
# ggplot(temporal_topics, aes(x = as.Date(month), y = factor(topic), fill = count)) +
#   geom_tile() +
#   facet_wrap(~ sentiment) +
#   scale_fill_gradientn(
#     colours = c("white", "pink", "red", "black"),
#     trans = "log",
#     na.value = "white"
#   ) +
#   labs(title = "Topic Prevalence Over Time by Sentiment",
#        x = "Month", y = "Topic") +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Parse and aggregate time by 6-month semester
dominant_topics <- dominant_topics %>%
  mutate(
    date = as.Date(date),
    semester = floor_date(date, unit = "6 months")  # Aggregates by semester
  )

# Summarize topic frequency by semester
temporal_topics <- dominant_topics %>%
  group_by(semester, topic, sentiment) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count >= 5)

# Plot heatmap (by semester instead of month)
ggplot(temporal_topics, aes(x = as.Date(semester), y = factor(topic), fill = count)) +
  geom_tile() +
  facet_wrap(~ sentiment) +
  scale_fill_gradientn(
    colours = c("white", "pink", "red", "black"),
    trans = "log",
    na.value = "white"
  ) +
  labs(title = "Topic Prevalence Over Time by Sentiment",
       x = "Semester", y = "Topic") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ---- STEP 9 ----
# Assume lda_neg and lda_pos are your fitted LDA models for negative and positive sentiments

# 9.1: NEGATIVE 

# Step 1: Extract topic probabilities (gamma) for negative sentiment
gamma_neg <- tidy(lda_neg, matrix = "gamma") %>%
  mutate(document = as.character(document), sentiment = "negative")

# Step 2: Extract dominant topic per document (highest gamma)
dominant_topic_neg <- gamma_neg %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

# Step 3: Prepare original comments with matching document IDs
comments_neg <- comments %>%
  filter(sentiment == "negative") %>%
  mutate(document = as.character(row_number())) %>%
  select(document, comment)

# Step 4: Join dominant topic info with comments
dominant_comments_neg <- dominant_topic_neg %>%
  inner_join(comments_neg, by = "document")

# Step 5: For each topic, get top example comments by gamma
example_comments_neg <- dominant_comments_neg %>%
  group_by(topic) %>%
  slice_max(gamma, n = 3) %>%
  select(sentiment, topic, comment) %>%
  arrange(topic)

print(example_comments_neg)


# 9.2 POSITIVE
gamma_pos <- tidy(lda_pos, matrix = "gamma") %>%
  mutate(document = as.character(document), sentiment = "positive")

dominant_topic_pos <- gamma_pos %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  ungroup()

comments_pos <- comments %>%
  filter(sentiment == "positive") %>%
  mutate(document = as.character(row_number())) %>%
  select(document, comment)

dominant_comments_pos <- dominant_topic_pos %>%
  inner_join(comments_pos, by = "document")

example_comments_pos <- dominant_comments_pos %>%
  group_by(topic) %>%
  slice_max(gamma, n = 3) %>%
  select(sentiment, topic, comment) %>%
  arrange(topic)

print(example_comments_pos)

