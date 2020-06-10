#' @title Bag of Little Bootstraps Logistic Regression Algorithm
#'
#' @description To run calculations parallelly for faster speed, first detect num of cores by running the following command:
#' `future::availableCores()`
#' Then specify number of cores you want use in the following line:
#' `plan(multisession, worker = #_cores)`
#'
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param m number of subsets for the data
#' @param B number of bootstraps
#' @param parallel TRUE/FALSE for parallel computing
#'
#' @return Returns the bag of little bootstrap confidence intervals for sigma for each boot of the logistic model
#' @export
blbglm <- function(formula, data, m = 2, B = 100, parallel = FALSE) {
  data_list <- split_data(data, m)
  if (parallel) {estimates <- furrr::future_map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  } else {estimates <- map(
      data_list,
      ~ glm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  }
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blbglm"
  invisible(res)
}

#' split data into m parts of approximated equal sizes
#' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param m number of subsets for data
split_data <- function(data, m) {
  idx <- sample.int(m, nrow(data), replace = TRUE)
  data %>% split(idx)
}

#' compute the estimates
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param n number of subsets for the data
#' @param B number of bootstraps
glm_each_subsample <- function(formula, data, n, B) {
  replicate(B, glm_each_boot(formula, data, n), simplify = FALSE)
}

#' compute the regression estimates for a blb dataset
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param n number of samples
glm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  glm1(formula, data, freqs)
}


#' estimate the regression estimates based on given the number of repetitions
#' @param formula an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted
#' @param data data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model
#' @param freqs number of frequencies
glm1 <- function(formula, data, freqs) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment(formula) <- environment()
  fit <- glm(formula, data, weights = freqs, family = binomial("logit"))
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}


#' compute the coefficients from fit
#' @param fit the fit of the model
blbcoef <- function(fit) {
  coef(fit)
}


#' compute sigma from fit
#' @param fit the fit of the model
blbsigma <- function(fit) {
  p <- fit$rank
  y <- model.extract(fit$model, "response")
  e <- fitted(fit) - y
  w <- fit$weights
  sqrt(sum(e^2*w)/(p-1))
}


#' @title Printing the model
#' @description `print.blblm` is used to print out 'blbglm model:' followed by the logistic regression formula
#'
#' @param x the fit
#' @param ... additional arguments to be passed to the low level logistic regression fitting functions
#'
#' @return A string of the blbglm model with regression formula.
#' print(fit)
#' # blbglm model: mpg ~ wt * hp
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blbglm model:", capture.output(x$formula))
  cat("\n")
}


#' @title Obtaining sigma
#'
#' @param object the fit
#' @param confidence TRUE/FALSE for confidence interval for sigma
#' @param level significance level for confidence interval
#' @param ... additional arguments to be passed to the low level regression fitting functions
#'
#' @return Returns the sigma value and/or confidence interval of the fit.
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' @title Obtaining coefficients
#'
#' @param object the fit
#' @param ... additional arguments to be passed to the low level regression fitting functions
#'
#' @return Returns the coefficients of the fit, intercept, mean of each variable, and correlation of defined variables.
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}


#' @title Obtaining confidence intervals
#'
#' @param object the fit
#' @param parm indicate the target variables, leave as NULL for ones already specified in the fit
#' @param level significance level
#' @param ... additional arguments to be passed to the low level regression fitting functions
#'
#' @return Returns the confidence interval of the fit.
#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @title Making predictions
#'
#' @param object the fit
#' @param new_data new data to be used for prediction
#' @param confidence TRUE/FALSE for confidence interval
#' @param level significance level for confidence interval
#' @param ... additional arguments to be passed to the low level logistic regression fitting functions
#'
#' @return Returns the predicted fit for the new data and confidence interval if designated.
#' @export
#' @method predict blblm
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())
  }
  else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
