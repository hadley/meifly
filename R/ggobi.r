#' Load model ensemble into GGobi with appropriate edge structure
#'
#' @keywords dynamic regression
#' @param data model ensemble object
#' @param ... other arguments ignored
#' @examples
#' y <- swiss$Fertility
#' x <- swiss[, -1]
#' mods <- fitall(y, x, "lm")
#' \dontrun{
#' library(rggobi)
#' ggobi(mods, swiss)
#' }
ggobi.ensemble <- function(data, ...) {
  if (!require("rggobi", quiet=TRUE)) 
    stop("rggobi required to visualise ensemble in GGobi.")
  g <- ggobi()
  g["model"] <- summary(data)
  
  c <- coef(data)
  g["model-variable"] <- c
  glyph_type(g["model-variable"]) <- ifelse(c$raw == 0, 1, 6)
  g <- ggobi_longitudinal(c, id = model, g = g)
  g["variable"] <- summary(c)
  
  r <- resid(data)
  g["model-data"] <- r
  g["data"] <- summary(r)
  
  invisible(g)
}