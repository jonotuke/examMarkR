# libs ----
pacman::p_load(tidyverse, targets, randomNames)

# data ----
example_grade_book <- read_csv("data-raw/example-grade-book.csv")
example_grade_book

usethis::use_data(example_grade_book, overwrite = TRUE)
