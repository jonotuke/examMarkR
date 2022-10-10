test_that("multiplication works", {
  pacman::p_load(tidyverse)
  df <- tibble(
    A1 = c(10, 1, 2),
    A2 = c(10, 2, 1)
  )
  df1 <- df |> exam_convert_prop(A1, 1)
  df2 <- df |> exam_convert_prop(A1:A2, 1)

  expect_equal(df1$A1, c(1, 0.1, 0.2))
  expect_equal(df1$A2, c(10, 2, 1))
  expect_equal(df2$A2, c(1, 0.2, 0.1))
})
