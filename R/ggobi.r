# Explore a model ensemble with GGobi
# Load model ensemble into GGobi with appropriate edge structure
#
# @keyword dynamic
# @keyword regression
#X y <- swiss$Fertility
#X x <- swiss[, -1]
#X mods <- fitall(y, x, lm)
#X \dontrun{
#X library(rggobi)
#X ggobi(mods, swiss)
#X }
ggobi.ensemble <- function(data, original=NULL, ...) {
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
	g["data"] <- summary(r, original)
	
	invisible(g)
}