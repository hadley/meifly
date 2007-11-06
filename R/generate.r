# Generate all models 
# Fit all combinations of x variables ($2^p$)
# 
# This technique generalises \code{\link{fitbest}}.  While it is much
# slower it will work for any type of model.
#
# @arguments vector y values
# @arguments matrix of x values
# @arguments method used to fit the model, eg \code{\link{lm}},\code{\link[MASS]{rlm}}
# @keyword regression
fitall <- function(y, x, method=lm, ...) {
	data <- cbind(y=y, x)

	combs <- do.call(expand.grid, rep(list(c(FALSE, TRUE)), ncol(x)))[-1, ]

	vars <- apply(combs, 1, function(i) names(x)[i])
	form <- paste("y ~ ", lapply(vars, paste, collapse=" + "), sep = "")
	form <- lapply(form, as.formula)

	models <- lapply(form, function(f) eval(substitute(method(f, data=data, ...), list(f=f, data=data, method=method))))
	names(models) <- 1:length(models)
	class(models) <- c("ensemble", class(models))
	models
}

# Generate best linear models
# Use the leaps package to generate the best subsets.
# 
# @arguments model formula
# @arguments data frame
# @arguments number of subsets of each size to record
# @arguments other arguments passed to \code{\link[leaps]{regsubsets}}
# @keyword regression
fitbest <- function(formula, data, nbest=10, ...) {
	b <- regsubsets(formula, data=data, nbest=nbest, ...)
	mat <- summary(b, matrix.logical = TRUE)$which

	intercept <- c("", "-1")[as.numeric(mat[,1])]
	vars <- apply(mat[,-1], 1, function(x) colnames(mat[, -1])[x])
	form <- paste(formula[[2]], " ~ ", lapply(vars, paste, collapse=" + "), sep = "")
	form <- lapply(form, as.formula)

	models <- lapply(form, function(f) eval(substitute(lm(f, data=data), list(f=f, data=data))))
	names(models) <- 1:length(models)
	class(models) <- c("ensemble", class(models))
	models
}

# General ensemble of models from models in global workspace
# 
# @arguments model type
# @arguments if specified, all models must use this dataset
# @arguments pattern of model object names to match
# @keyword regression
findmodels <- function(modeltype = "lm", dataset, pattern) {
	ls <- ls(".GlobalEnv", pattern=pattern)
	mods <- ls[sapply(ls, function(x) inherits(get(x), modeltype))]
	if (!missing(dataset)) {
		data.name <- function(x) as.character(x$call[["data"]])
		mods <- mods[sapply(mods, function(x) data.name == dataset)]
	}
	
	models <- lapply(mods, get)
	class(models) <- c("ensemble", class(models))
	models
}

# Generate linear models by bootstrapping observations
# 
# @arguments model formula
# @arguments data set
# @argument number of bootstrapped data sets to generate
# @keyword regression
lmboot <- function(formula, data, n=100) {
	# fix to add model
	models <- replicate(n, lm(formula, data=data[sample(nrow(data), replace=TRUE), ]), simplify=FALSE)
	names(models) <- 1:length(models)
	class(models) <- c("ensemble", class(models))
	models
}

