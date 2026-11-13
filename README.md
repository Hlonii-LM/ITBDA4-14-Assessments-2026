# ITBDA4-14-Assessments-2026
The repository is my ITBDA4-14 Assessments 

# ETL Books Pricing Intelligence — R & PostgreSQL

## Overview
End-to-end ETL: scrape 60 book records from `books.toscrape.com`,
clean and impute missing values in R, load into PostgreSQL, and run
two analytical investigations into price–rating relationships.

## Questions
| Q | Topic | Notebook | Language |
|---|-------|----------|----------|
| 1 | Hadoop MapReduce keyword search | `notebooks/q1_mapreduce.ipynb` | Java |
| 2 | LDA topic modelling on news | `notebooks/q2_lda_topics.ipynb` | R |
| 3 | WebText scraping + summarisation | `notebooks/q3_webtext_api.ipynb` | R |
| 4 | ETL with R & PostgreSQL | `notebooks/q4_etl_postgres.ipynb` | R + SQL |

## Data
- Q1: `map_reduce_test_docs.zip`
- Q2: `news.zip`
- Q3: LADAL tutorial page
- Q4: `http://books.toscrape.com` (public scraping sandbox)

## Reproducibility
1. Clone the repository.
2. Q1 requires Hadoop 3.x — see the notebook header for run instructions.
3. Q2–Q4 run locally; install R packages listed at the top of each notebook.
4. For Q4, create the `etl_demo` database in PostgreSQL, then run the notebook
   top-to-bottom (credentials are read from environment variables).

## Author
[Your Name] — [Date]
