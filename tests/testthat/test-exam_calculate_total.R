test_that("exam_calculate_total", {
  pacman::p_load(tidyverse)
  df <- tibble(
    A1 = c(1, 0.5, 0),
    A2 = c(0, NA, 1),
    OQ1 = c(1, 1, NA)
  )
  weights <- tibble(
    assessment = colnames(df),
    weight = c(40, 40, 10)
  )
  expect_error(exam_calculate_total(df, weights))
  weights <- tibble(
    weight = c(40, 40, 20)
  )
  expect_error(exam_calculate_total(df, weights))
  weights <- tibble(
    assessment = colnames(df)
  )
  expect_error(exam_calculate_total(df, weights))
  weights <- tibble(
    assessment = "Test 1",
    weight = 100
  )
  expect_error(exam_calculate_total(df, weights))
  weights <- tibble(
    assessment = colnames(df),
    weight = c(40, 40, 20)
  )
  expect_equal(
    exam_calculate_total(df, weights), c(60, 66.667, 50),
    tolerance = 0.001)
})
