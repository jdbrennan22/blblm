test_that("Class of `lm1` is that of list", {
  model <- lm1(mpg ~ wt * hp, data = mtcars, freqs = NULL)
  expect_that(lm1(mpg ~ wt * hp, data = mtcars, freqs = NULL), is_a("list"))
})
