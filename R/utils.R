# helper function to coerce input to a character vector
as_character <- function(x) {
  if(is.character(x) && !is.object(x)) return(x)
  as.character(x)
}

# Different rules in order to find a decent number of bins for hexbinning
find_nbins <- function(x, rule = c("RR","sqrt", "Scott", "FD", "Sturges")) {
  rule <- match.arg(rule, c("RR","sqrt", "Scott", "FD", "Sturges"),
                    several.ok = FALSE)
  
  # Number of observations
  n <- length(x)
  
  # Different methods (defaults to Scott method)
  res <- switch(rule,
                "sqrt" = sqrt(n),
                "Scott" = (max(x) - min(x)) / (3.5 * sqrt(var(x)) / n^(1/3)),
                "FD" = (max(x) - min(x)) / (2 * IQR(x) / n^(1/3)),
                "Sturges" = log2(n) + 1,
                "RR"= 2*(n^(1/3)),
                (max(x) - min(x)) / (3.5 * sqrt(var(x)) / n^(1/3)))
  ceiling(res)
}
