#' @importFrom Rcpp sourceCpp
#' @import RcppEigen
#' @keywords internal
NULL

# helper function to coerce input to a character vector
as_character <- function(x) {
  if(is.character(x) && !is.object(x)) return(x)
  as.character(x)
}
