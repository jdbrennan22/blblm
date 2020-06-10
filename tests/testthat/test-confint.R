test_that("`print.blblm` prints output", {
  n = 2
  m = 2
  fit <- blblm_mod(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, parallel = TRUE)
  co <- confint(fit, c("wt", "hp"))
  expect_equal(dim(co), c(n,m))
})
