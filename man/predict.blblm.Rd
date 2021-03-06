% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blbglm.R, R/blblm.R
\name{predict.blblm}
\alias{predict.blblm}
\title{Making predictions}
\usage{
\method{predict}{blblm}(object, new_data, confidence = FALSE, level = 0.95, ...)

\method{predict}{blblm}(object, new_data, confidence = FALSE, level = 0.95, ...)
}
\arguments{
\item{object}{the fit}

\item{new_data}{new data to be used for prediction}

\item{confidence}{TRUE/FALSE for confidence interval}

\item{level}{significance level for confidence interval}

\item{...}{additional arguments to be passed to the low level regression fitting functions}
}
\value{
Returns the predicted fit for the new data and confidence interval if designated.

Returns the predicted fit for the new data and confidence interval if designated.
}
\description{
Making predictions

Making predictions
}
\examples{
fit <- blblm_mod(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
}
