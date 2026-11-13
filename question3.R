library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(lsa)

url <- "https://en.wikipedia.org/wiki/Artificial_intelligence"

webpage <- read_html(url)

raw_text <- webpage %>%
  html_nodes(".mw-parser-output p") %>%
  html_text()

web_data <- data.frame(
  id = 1,
  content = paste(raw_text, collapse = " "),
  stringsAsFactors = FALSE
)

corpus <- VCorpus(VectorSource(web_data$content))

clean_corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

clean_data <- data.frame(
  text = sapply(clean_corpus, as.character),
  stringsAsFactors = FALSE
)

sentences_raw <- str_split(web_data$content, "(?<=[.!?])\\s+")[[1]]

sentences_df <- data.frame(
  sentence_id = 1:length(sentences_raw),
  sentence = sentences_raw,
  stringsAsFactors = FALSE
) %>%
  filter(str_count(sentence, "\\w+") >= 10)

tf_idf <- sentences_df %>%
  unnest_tokens(word, sentence) %>%
  count(sentence_id, word, sort = TRUE) %>%
  bind_tf_idf(word, sentence_id, n)

tfidf_summary <- tf_idf %>%
  filter(!word %in% c("can", "use", "used", "also", "may", "one", "two", "first")) %>%
  group_by(sentence_id) %>%
  summarise(total_tfidf = sum(tf_idf), n_words = n()) %>%
  inner_join(sentences_df, by = "sentence_id") %>%
  filter(n_words >= 15) %>%
  arrange(desc(total_tfidf)) %>%
  slice(1:3) %>%
  pull(sentence)

cat("=== TF-IDF EXTRACTIVE SUMMARY ===\n")
cat(paste(tfidf_summary, collapse = "\n\n"), "\n\n")

sentence_corpus <- VCorpus(VectorSource(sentences_df$sentence))

sentence_corpus <- sentence_corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace)

non_empty <- sapply(sentence_corpus, function(x) nchar(content(x)) > 0)
sentence_corpus <- sentence_corpus[non_empty]

tdm <- TermDocumentMatrix(sentence_corpus)
tdm_matrix <- as.matrix(tdm)

tdm_matrix <- tdm_matrix[rowSums(tdm_matrix) > 0, ]

lsa_space <- suppressWarnings(lsa(tdm_matrix))

top_terms_lsa <- rownames(lsa_space$tk)[order(lsa_space$tk[,1], decreasing = TRUE)][1:10]

cat("=== LSA TOP CONCEPTS ===\n")
cat(paste(top_terms_lsa, collapse = ", "), "\n\n")

word_freq <- clean_data %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  filter(n > 20)

wordcloud(words = word_freq$word, freq = word_freq$n, min.freq = 1,
          max.words = 100, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

top_terms_bar <- tf_idf %>%
  group_by(word) %>%
  summarise(total_tf_idf = sum(tf_idf)) %>%
  arrange(desc(total_tf_idf)) %>%
  slice(1:15)

ggplot(top_terms_bar, aes(x = reorder(word, total_tf_idf), y = total_tf_idf, fill = word)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 15 Important Terms (by TF-IDF)",
    subtitle = "Terms that uniquely identify the Wikipedia 'Artificial Intelligence' article",
    x = "Terms",
    y = "TF-IDF Score"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))