.df <- function(x) attr(logLik(x), "df")

# Summarise ensemble of models with model fit statistics
# Returns degrees of freedom, log likelihood, R-squared, AIC, BIC and adjusted R-squared.
#
# @arguments ensemble of models
# @keyword regression
summary.ensemble <- function(object, ...) {
  fits <- data.frame(t(sapply(object, function(mod) {
    sum <- summary(mod)
    c(
      df = .df(mod),
      logL = logLik(mod),
      AIC = -AIC(mod),
      BIC = -AIC(mod, k=log(length(fitted(mod)))),
      R2 = sum$r.squared,
      adjR2 = sum$adj.r.squared,
      n = length(mod$residuals)
    )
  })))
  fits$model <- factor(names(object))
  rownames(fits) <- paste("m", fits$model, sep="")
  fits
}

# Calculcate coefficients for all models in ensemble
# Returns raw, t-value, absolute t-value, and standardised coefficent values.
# 
# @arguments ensemble of models
# @keyword regression
coef.ensemble <- function(object, ...) {  
  coefs <- ldply(object, coef_simple, data = attr(object, "data"))
  names(coefs)[1] <- "model"
  coefs$model <- factor(coefs$model)

  coefs <- add.all.combinations(coefs, vars = list("model","variable"))
  coefs[is.na(coefs)] <- 0
  rownames(coefs) <- str_c("m", coefs$model, "v", as.numeric(coefs$variable))
  class(coefs) <- c("variable_ensemble", class(coefs))
  
  coefs
}

# Coef simple
# Simple coefficient extractor for single model
#
# @keyword internal
coef_simple <- function(model, data) {
  trunc <- function(x, trunc) ifelse(abs(x) > trunc, sign(x) * trunc, x)

  coefs <- data.frame(
    names(coef(model))[-1], 
    summary(model)$coefficients[-1, c(1, 3), drop=FALSE]
  )
  names(coefs) <- c("variable", "raw", "t")
  transform(coefs,
    t = trunc(t,3),
    abst = abs(trunc(t, 3)),
    std = stdcoef(model, data)[-1]
  )
}

# Summarise variable ensemble
# Summary function of coef(ensemble)
# 
# Provides variable level statistics.
# 
# @keyword regression
summary.variable_ensemble <- function(object, ...) {
  coefs <- subset(rename(object, c(variable = "var")), raw != 0)
  coefm <- melt(coefs, 
    id=c("model","var"), 
    measure=c("raw","t","abst","std")
  )
  
  rename(cbind(
    cast(coefm, var ~ variable, c(mean, sd)),
    n = cast(coefm, var ~ variable, length)[,2]
  ), c(var = "variable"))
}

# Calculcate standardised coefficients for a model
# Refits the model with data standardised to mean 0, standard deviation 1.
# 
# @arguments model to refit with standardised data
# @keyword internal
stdcoef <- function(model, data = model$model) {
  coef(update(model, . ~ ., data=rescaler(data)))
}

# Residuals for model ensemble
# Calculate residuals for all models in ensemble
# 
# @arguments ensemble of models
# @returns data.frame of class \code{resid_ensemble}
# @seealso \code{\link{summary.resid_ensemble}}
# @keyword regression
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
  resids$obs <- factor(resids$obs, level = names(scores)[order(scores)])
  class(resids) <- c("resid_ensemble", class(resids))
  attr(resids, "data") <- attr(object, "data")
  resids
}

# Residual summary for model ensemble
# Summarise residuals from ensemble
# 
# @arguments model residuals from \code{\link{residuals.ensemble}}
# @keyword regression
summary.resid_ensemble <- function(object, data = attr(object, "data"), ...) {
  s <- ddply(object, "obs", summarise,
    mean = mean(rstudent),
    sd = sd(rstudent), 
    n = length(rstudent))
  
  if (!is.null(data)) s <- cbind(s, data)
  s
}
