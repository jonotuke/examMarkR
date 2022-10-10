test_that("exam_clean_marks", {
  x <- c(0, "EX", NA)
  expect_equal(exam_clean_marks(x), c(0, NA, 0))
})
