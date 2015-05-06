.df <- function(x) attr(logLik(x), "df")

#' Returns degrees of freedom, log likelihood, R-squared, AIC, BIC and
#' adjusted R-squared.
#'
#' @param object ensemble of models
#' @param ... other arguments ignored
#' @keywords regression
#' @export
summary.ensemble <- function(object, ...) {
  fits <- data.frame(t(sapply(object, broom::glance)))
  fits$model <- factor(names(object))
  rownames(fits) <- paste("m", fits$model, sep="")
  fits
}

#' Calculcate coefficients for all models in ensemble.
#' Returns raw, t-value, absolute t-value, and standardised coefficent values.
#'
#' @param object ensemble of models
#' @param ... other arguments ignored
#' @keywords regression
#' @export
coef.ensemble <- function(object, ...) {
  coefs <- plyr::ldply(object, broom::tidy, data = attr(object, "data"))
  names(coefs)[1] <- "model"
  coefs$model <- factor(coefs$model)

  all <- expand.grid(
      model = unique(coefs$model),
      term = unique(coefs$term))
  coefs <- join(all, coefs, by = c("model", "term"))
  coefs[is.na(coefs)] <- 0
  rownames(coefs) <- paste("m", coefs$model, "v", as.numeric(coefs$term),
    sep = "")
  class(coefs) <- c("variable_ensemble", class(coefs))

  coefs
}

#' Summarise variable ensemble.
#'
#' Provides variable level statistics.
#'
#' @param object ensemble of models
#' @param ... other arguments ignored
#' @keywords regression
#' @export
summary.variable_ensemble <- function(object, ...) {
  coefs <- subset(object, estimate != 0)

  plyr::ddply(coefs, "term", summarise,
     estimate_mean = mean(estimate),
     estimate_sd = sd(estimate),
     statistic_mean = mean(statistic),
     statistic_sd = sd(statistic),
     n = length(model))
}

# Calculcate standardised coefficients for a model
stdcoef <- function(model, data = model$model) {
  data[] <- lapply(data, scale)
  coef(update(model, . ~ ., data = data))
}

#' Calculate residuals for all models in ensemble.
#'
#' @return data.frame of class \code{resid_ensemble}
#' @seealso \code{\link{summary.resid_ensemble}}
#' @param object ensemble of models
#' @param ... other arguments ignored
#' @keywords regression
#' @export
residuals.ensemble <- function(object, ...) {
  resids <- do.call(rbind, mapply(function(mod, name) {
    data.frame(
      obs=gsub("\\.[0-9]+$", "", names(resid(mod))),
      resid=resid(mod),
      rstudent = rstudent(mod),
      fitted=fitted(mod),
      true=fitted(mod) + resid(mod),
      influence.measures(mod)$infmat[, c("dffit", "cov.r", "cook.d", "hat")],
      model=name)
  }, object, names(object), SIMPLIFY=FALSE))
  resids$model <- factor(resids$model)
  rownames(resids) <- 1:nrow(resids)

  scores <- tapply(resids$resid, resids$obs, mean)
  resids$obs <- factor(resids$obs, levels = names(scores)[order(scores)])
  class(resids) <- c("resid_ensemble", class(resids))
  attr(resids, "data") <- attr(object, "data")
  resids
}

#' Summarise residuals from ensemble.
#'
#' @param object model residuals from \code{\link{residuals.ensemble}}
#' @param data associated data set
#' @param ... other arguments ignored
#' @keywords regression
#' @export
summary.resid_ensemble <- function(object, data = attr(object, "data"), ...) {
  s <- ddply(object, "obs", summarise,
    mean = mean(rstudent),
    sd = sd(rstudent),
    n = length(rstudent))

  if (!is.null(data)) {
    data$obs <- rownames(data)
    rownames(data) <- NULL
    s <- join(s, data, by = "obs")
  }
  s
}
