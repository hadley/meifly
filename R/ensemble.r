new_ensemble <- function(models, data) {
  structure(models, data = data, class = "ensemble")
}

#' @S3method ensemble
"[.ensemble" <- function(x, i, ...) {
  structure(NextMethod(), class = "ensemble")
}