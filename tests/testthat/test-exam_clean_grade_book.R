pacman::p_load(tidyverse)
test_that("exam_clean_grade_book", {
  df <- tibble(
    ID = 1:3,
    A1 = c(12, 12, NA),
    A2 = c("EX", 12, NA),
    test1 = c(12, 12, "EX")
  )
  df1 <- df |> exam_clean_grade_book(A1)
  df2 <- df |> exam_clean_grade_book(A1:A2)
  df3 <- df |> exam_clean_grade_book(starts_with("A"))
  expect_equal(df1$A1, c(12, 12, 0))
  expect_equal(df1$A2, c("EX", 12, NA))
  expect_equal(df2$A2, c(NA, 12, 0))
  expect_equal(df3$A2, c(NA, 12, 0))
})
