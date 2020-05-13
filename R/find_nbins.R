#' Compute a reasonable default number of bins.
#'
#' Compute a reasonable default number of bins based on the data and different
#' types of rules.
#'
#' @param x a numeric vector.
#' @param rule a character string matching to a rule to use to determine
#'     number of bins.
#' @param na_rm logical whether to remove (`TRUE`) missing values or not
#'     (`FALSE`)
#' @seealso The following
#'     [wiki](https://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width)
#'     for a discussion on different "rules-of-thumb". Additionally, the
#'     [bigvis](https://github.com/hadley/bigvis) package and sources therein.
#'
#' @return an integer with reasonable number of bins.
#'
#' @export find_nbins
#' @importFrom stats var IQR
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
  sd_x <- sqrt(stats::var(x, na.rm = na_rm))

  # Different methods (defaults to Scott method)
  res <- switch(rule,
                "sqrt" = sqrt(n),
                "Scott" = (max_x - min_x) / (3.5 * sd_x / n^(1/3)),
                "FD" = (max_x - min_x) / (2 * stats::IQR(x) / n^(1/3)),
                "Sturges" = log2(n) + 1,
                "RR"= 2*(n^(1/3)),
                (max_x - min_x) / (3.5 * sd_x / n^(1/3)))
  ceiling(res)
}
