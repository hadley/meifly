#' Create a new ensemble of models.
#' 
#' @keywords internal
#' @S3method "[" ensemble
#' @param models list of models
#' @param data associated data frame
new_ensemble <- function(models, data) {
  structure(models, data = data, class = "ensemble")
}

"[.ensemble" <- function(x, i, ...) {
  structure(NextMethod(), class = "ensemble")
}