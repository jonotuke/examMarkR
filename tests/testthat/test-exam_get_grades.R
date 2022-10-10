test_that("exam_get_grade", {
  x <- rep(
    c("FNS", "FNA", "FA", "P", "C", "D", "HD"),
    c(1, 39, 10, 15, 10, 10, 16)
  )
  y <- rep(
    c("FNS", "FNA", "FA", "P", "C", "D", "HD"),
    c(1, 44, 5, 15, 10, 10, 16)
  )
  expect_equal(as.character(exam_get_grade(0:100, service = TRUE)), x)
  expect_equal(as.character(exam_get_grade(0:100, service = FALSE)), y)
})
