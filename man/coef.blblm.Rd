% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blbglm.R, R/blblm.R
\name{coef.blblm}
\alias{coef.blblm}
\title{Obtaining coefficients}
\usage{
\method{coef}{blblm}(object, ...)

\method{coef}{blblm}(object, ...)
}
\arguments{
\item{object}{the fit}

\item{...}{additional arguments to be passed to the low level regression fitting functions}
}
\value{
Returns the coefficients of the fit, intercept, mean of each variable, and correlation of defined variables.

Returns the coefficients of the fit, intercept, mean of each variable, and correlation of defined variables.
}
\description{
Obtaining coefficients

Obtaining coefficients
}
\examples{
fit <- blblm_mod(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
coef(fit)
}
