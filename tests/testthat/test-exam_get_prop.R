test_that("exam_get_prop", {
  x <- c(10, 1, 2)
  p <- exam_get_prop(x, 1)
  expect_equal(p[1], 1)
  expect_equal(p[2], 0.1)
  expect_equal(p[3], 0.2)
})
