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
  g["model-estimate"] <- c
  glyph_type(g["model-estimate"]) <- ifelse(c$raw == 0, 1, 6)
  g <- ggobi_longitudinal(c, id = model, g = g)
  g["estimate"] <- summary(c)
  
  r <- resid(data)
  g["model-observation"] <- r
  g["observation"] <- summary(r)
  
  invisible(g)
}