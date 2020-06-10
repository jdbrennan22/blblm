test_that("blblm_mod has correct dimensions", {
  n = 4
  m = 1
  fit <- blblm_mod(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  expect_s3_class(fit, "blblm")
  co <- coef(fit) %>% as.data.frame()
  expect_equal(dim(co), c(n,m))
})
