# blblm

## Introduction
This package contains the bag of little bootstraps for linear regression algorithm that assesses the quality of estimators. Blblm uses both bootstrapping and subsampling methods, and works well on large datasets because it uses multinomial sampling instead of repeated sampling. Blblm is well-suited for parallel and distributed computing. Blblm is compuationally friendly, flexible, and maintains most of the statistical properties of regular bootstrapping. 

## Abstract 
This vignette is produced in order to aid in the understanding of the `blblm` package. This package serves three main functions that compute the bag of little bootstraps confidence intervals for linear regression coefficients, sigma, and prediction. Within these main functions are helper functions that are used to develop the confidence intervals. These helper functions include sampling, splitting, computing estimates, and mapping functions that are used in the main functions. This package also incorporates parallel computing in order to increase computational power for faster processing speed and efficiency. If having difficulties using package, use `?blblm::func` for more information.


<!-- badges: start -->
<!-- badges: end -->

## Examples

``` r
library(blblm)
fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
coef(fit)
#> (Intercept)          wt          hp       wt:hp 
#> 48.88428523 -7.88702986 -0.11576659  0.02600976
confint(fit, c("wt", "hp"))
#>           2.5%       97.5%
#> wt -10.7902240 -5.61586271
#> hp  -0.1960903 -0.07049867
sigma(fit)
#> [1] 1.838911
sigma(fit, confidence = TRUE, level = .95)
#>    sigma      lwr      upr 
#> 1.838911 1.350269 2.276347
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
#>        1        2 
#> 21.55538 18.80785
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
#>        fit      lwr      upr
#> 1 21.55538 20.02457 22.48764
#> 2 18.80785 17.50654 19.71772
```
