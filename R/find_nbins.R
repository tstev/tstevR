#' Compute a reasonable default number of bins.
#'
#' Compute a reasonable default number of bins based on the data and different
#' types of rules.
#'
#' @param x a numeric vector.
#' @param rule a character string matching to a rule to use to determine
#'     number of bins.
#' @seealso The following
#'     [wiki](https://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width)
#'     for a discussion on different "rules-of-thumb". Additionally,
#'     [bigvis::find_width()] and [bigvis::find_origin()] for more reasonable
#'     defaults.
#' @return an integer with reasonable number of bins.
#'
#' @export
#' @examples
#' set.seed(42L)
#' find_nbins(rnorm(1000))
#' find_nbins(rnorm(1000), "Scott")
#' find_nbins(rnorm(1000), "sqrt")
find_nbins <- function(x, rule = c("FD", "RR", "Scott", "sqrt", "Sturges"),
                       na_rm = TRUE) {
  stopifnot(is.numeric(x))
  rule <- match.arg(rule)

  # Number of observations
  n <- length(x)
  min_x <- min(x, na.rm = na_rm)
  max_x <- max(x, na.rm = na_rm)
  sd_x <- sqrt(var(x, na.rm = na_rm))

  # Different methods (defaults to Scott method)
  res <- switch(rule,
                "sqrt" = sqrt(n),
                "Scott" = (max_x - min_x) / (3.5 * sd_x / n^(1/3)),
                "FD" = (max_x - min_x) / (2 * IQR(x) / n^(1/3)),
                "Sturges" = log2(n) + 1,
                "RR"= 2*(n^(1/3)),
                (max_x - min_x) / (3.5 * sd_x / n^(1/3)))
  ceiling(res)
}
