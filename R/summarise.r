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
			adjR2 = sum$adj.r.squared
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
	coefs <- do.call(rbind, 
		mapply(coef_simple, object, names(object), SIMPLIFY=FALSE)
	)

	coefs <- add.all.combinations(coefs, vars=list("model","variable"))
  # coefs <- transform(coefs,
  #   varp = factor(ifelse(raw == 0, "", as.character(variable)))
  # )
  coefs[is.na(coefs)] <- 0
	rownames(coefs) <-  paste("m", coefs$model, "v", as.numeric(coefs$variable), sep="")

	class(coefs) <- c("variable_ensemble", class(coefs))
	
	coefs
}

# Coef simple
# Simple coefficient extractor for single model
#
# @keyword internal
coef_simple <- function(model, name="") {
	trunc <- function(x, trunc) ifelse(abs(x) > trunc, sign(x) * trunc, x)

	coefs <- data.frame(
		names(coef(model))[-1], 
		summary(model)$coefficients[-1, c(1, 3), drop=FALSE]
	)
	names(coefs) <- c("variable", "raw", "t")
	transform(coefs,
		raw = raw, 
		t = trunc(t,3),
		abst = abs(trunc(t, 3)),
		std = stdcoef(model)[-1], 
		model = name
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
stdcoef <- function(model) {
	coef(update(model, . ~ ., data=rescaler(model$model)))
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
	
	resids$obs <- reorder(resids$obs, resids$resid)
	class(resids) <- c("resid_ensemble", class(resids))
	resids
}

# Residual summary for model ensemble
# Summarise residuals from ensemble
# 
# @arguments model residuals from \code{\link{residuals.ensemble}}
# @keyword regression
summary.resid_ensemble <- function(object, data=NULL, ...) {
	s <- data.frame(
		mean = c(tapply(abs(object$rstudent), object$obs, mean)),
		sd = c(tapply(abs(object$rstudent), object$obs, sd)),
		n = c(tapply(object$resid, object$obs, length))
	)
	obs <- factor(unique(object$obs))
	s <- s[obs, ]
	s$obs <- obs

	if (!is.null(data)) s <- cbind(s, data)
	s
}
