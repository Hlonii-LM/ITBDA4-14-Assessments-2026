###############################################################################
# QUESTION 4 — COMPLETE ETL PIPELINE (All in R)
###############################################################################

library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(DBI)
library(RPostgres)

# =============================================================================
# 4.1 WEB SCRAPING
# =============================================================================
cat("\n--- 4.1: Scraping Books Data ---\n")

base_url <- "http://books.toscrape.com/catalogue/page-%d.html"

scrape_page <- function(page_num) {
  page <- read_html(sprintf(base_url, page_num))
  tibble(
    title     = page %>% html_elements(".product_pod h3 a") %>% html_attr("title"),
    price_raw = page %>% html_elements(".product_pod .price_color") %>% html_text2(),
    rating    = page %>% html_elements(".product_pod .star-rating") %>%
      html_attr("class") %>% str_remove("star-rating "),
    page      = page_num
  )
}

raw_books <- map_dfr(1:3, scrape_page)
cat("Scraped", nrow(raw_books), "raw records.\n\n")

# =============================================================================
# 4.2 DATA CLEANING & IMPUTATION
# =============================================================================
cat("--- 4.2: Cleaning and Imputing Data ---\n")

clean_books <- raw_books %>%
  mutate(
    price = price_raw %>% str_remove_all("£") %>% 
      str_remove_all("[[:space:]]") %>% as.numeric(),
    rating_num = case_when(
      rating == "One" ~ 1, rating == "Two" ~ 2, rating == "Three" ~ 3,
      rating == "Four" ~ 4, rating == "Five" ~ 5, TRUE ~ NA_real_
    )
  ) %>%
  mutate(rating_num = ifelse(is.na(rating_num),
                             as.numeric(names(sort(table(rating_num), decreasing = TRUE))[1]), rating_num)) %>%
  group_by(rating_num) %>%
  mutate(price = ifelse(is.na(price), median(price, na.rm = TRUE), price)) %>%
  ungroup() %>%
  mutate(price = ifelse(is.na(price), median(price, na.rm = TRUE), price)) %>%
  distinct(title, .keep_all = TRUE) %>%
  select(title, price, rating = rating_num, page)

write_csv(clean_books, "books_cleaned.csv")
cat("Data cleaned. Exported to 'books_cleaned.csv'.\n\n")

# =============================================================================
# 4.3 POSTGRESQL IMPORT & SQL INVESTIGATIONS
# =============================================================================
cat("--- 4.3: Loading to PostgreSQL & Running SQL ---\n")

db_name <- "postgres"       
db_host <- "localhost" 
db_port <- 5432
db_user <- "postgres"       
db_pass <- "admin123"       

tryCatch({
  con <- dbConnect(RPostgres::Postgres(),
                   dbname   = db_name,
                   host     = db_host,
                   port     = db_port,
                   user     = db_user,
                   password = db_pass)
  
  cat("Connected to PostgreSQL successfully.\n")
  
  dbExecute(con, "DROP TABLE IF EXISTS books;")
  dbWriteTable(con, "books", clean_books, overwrite = TRUE)
  cat("Table 'books' created in PostgreSQL.\n\n")
  
  # ---------------------------------------------------------------------------
  # SQL Investigation 1
  # ---------------------------------------------------------------------------
  cat("--- SQL Investigation 1: Average Price by Rating ---\n")
  sql_query_1 <- "
    SELECT rating,
           COUNT(*)                          AS book_count,
           ROUND(AVG(price)::numeric, 2)     AS avg_price,
           ROUND(MIN(price)::numeric, 2)     AS min_price,
           ROUND(MAX(price)::numeric, 2)     AS max_price
    FROM   books
    GROUP  BY rating
    ORDER  BY rating;
  "
  result_1 <- dbGetQuery(con, sql_query_1)
  print(result_1)
  
  # ---------------------------------------------------------------------------
  # SQL Investigation 2
  # ---------------------------------------------------------------------------
  cat("\n--- SQL Investigation 2: Price-Band Distribution ---\n")
  sql_query_2 <- "
    SELECT CASE
             WHEN price < 20 THEN 'Budget (<£20)'
             WHEN price < 40 THEN 'Mid-range (£20-£39)'
             ELSE                 'Premium (£40+)'
           END                               AS price_band,
           COUNT(*)                          AS book_count,
           ROUND(AVG(rating)::numeric, 2)    AS avg_rating
    FROM   books
    GROUP  BY price_band
    ORDER  BY MIN(price);
  "
  result_2 <- dbGetQuery(con, sql_query_2)
  print(result_2)
  
  dbDisconnect(con)
  cat("\nETL Pipeline complete. Disconnected from PostgreSQL.\n")
  
}, error = function(e) {
  cat("\n=== POSTGRESQL CONNECTION ERROR ===\n")
  cat("Error details:", conditionMessage(e), "\n")
})