#' @S3method ensemble
"[.ensemble" <- function(x, i, ...) {
  structure(NextMethod(), class = "ensemble")
}