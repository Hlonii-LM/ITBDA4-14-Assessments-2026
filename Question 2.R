required_pkgs <- c("readtext", "quanteda", "topicmodels",
                   "ggplot2", "wordcloud", "dplyr", "tidyr", 
                   "tibble", "forcats", "RColorBrewer", "tidytext")

installed <- rownames(installed.packages())
for (p in required_pkgs) {
  if (!(p %in% installed)) install.packages(p, dependencies = TRUE)
}

library(readtext)
library(quanteda)
library(topicmodels)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(RColorBrewer)
library(tidytext)

# 2.1
news_path <- "C:/Users/user/Downloads/news/sa_news"

raw_data <- readtext(paste0(news_path, "/*.txt"), docvarsfrom = "filenames")

df_news <- data.frame(
  file_name = raw_data$doc_id,
  content   = raw_data$text,
  stringsAsFactors = FALSE
)

cat("Number of news files loaded:", nrow(df_news), "\n")

df_news <- df_news[nchar(trimws(df_news$content)) > 0, ]

tokens_news <- tokens(df_news$content,
                      remove_punct   = TRUE,
                      remove_numbers = TRUE,
                      remove_symbols = TRUE,
                      remove_url     = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"), padding = FALSE) %>%
  tokens_wordstem()

dfm_news <- dfm(tokens_news)

dfm_news <- dfm_trim(dfm_news, min_docfreq = 0.005, docfreq_type = "prop")

dfm_news <- dfm_subset(dfm_news, ntoken(dfm_news) > 0)
cat("Documents remaining after cleaning:", ndoc(dfm_news), "\n")
cat("Unique terms in vocabulary:", nfeat(dfm_news), "\n")

# 2.2
set.seed(1234)
k_topics <- 5

lda_model <- LDA(dfm_news, k = k_topics, method = "Gibbs",
                 control = list(seed = 1234, burnin = 1000, iter = 2000, thin = 100))

top_terms <- terms(lda_model, 10)
beta_matrix <- posterior(lda_model)$terms

cat("\n============================================================\n")
cat("       LDA TOPIC REPORT: TOP 10 WORDS & PROBABILITIES\n")
cat("============================================================\n")
for (i in 1:k_topics) {
  cat(sprintf("\n--- Topic %d ---\n", i))
  words_i <- top_terms[, i]
  probs_i <- beta_matrix[i, words_i]
  report  <- data.frame(Rank = 1:length(words_i),
                        Word = words_i,
                        Probability = round(as.numeric(probs_i), 5))
  print(report, row.names = FALSE)
}

report_list <- lapply(1:k_topics, function(i) {
  words_i <- top_terms[, i]
  data.frame(Topic = i, Rank = 1:length(words_i), Word = words_i,
             Probability = round(as.numeric(beta_matrix[i, words_i]), 5))
})
lda_report <- do.call(rbind, report_list)
write.csv(lda_report, file = file.path(news_path, "LDA_topic_report.csv"), row.names = FALSE)
cat("\nTopic report saved to:", file.path(news_path, "LDA_topic_report.csv"), "\n")

# 2.3
beta_tidy <- as.data.frame(beta_matrix) %>%
  rownames_to_column(var = "Topic_ID") %>% 
  pivot_longer(cols = -Topic_ID, names_to = "word", values_to = "beta") %>%
  group_by(Topic_ID) %>%
  slice_max(order_by = beta, n = 10) %>%
  ungroup() %>%
  mutate(Topic_ID = paste("Topic", gsub("X\\.|X", "", Topic_ID))) 

p_bar <- beta_tidy %>%
  mutate(word = reorder_within(word, beta, Topic_ID)) %>%
  ggplot(aes(x = beta, y = word, fill = Topic_ID)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Topic_ID, scales = "free_y", ncol = 2) +
  scale_y_reordered() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "LDA – Top 10 Keywords per Topic",
       x = "Word probability (beta)", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

print(p_bar)
ggsave(file.path(news_path, "LDA_bar_chart.png"), plot = p_bar, width = 10, height = 8, dpi = 300)

png(file.path(news_path, "LDA_wordclouds.png"), width = 1600, height = 1200, res = 150)
par(mfrow = c(2, 3), mar = c(1, 1, 2, 1))
for (i in 1:k_topics) {
  wc_words <- terms(lda_model, 50)[, i]
  wc_probs <- as.numeric(beta_matrix[i, wc_words])
  wordcloud(words = wc_words, freq = wc_probs, scale = c(3, 0.5), max.words = 50,
            random.order = FALSE, colors = brewer.pal(8, "Dark2"),
            main = paste("Topic", i), main.size = 1.2)
}
par(mfrow = c(1, 1))
dev.off()

cat("Word cloud saved to:", file.path(news_path, "LDA_wordclouds.png"), "\n")
cat("\nAll tasks completed successfully!\n")