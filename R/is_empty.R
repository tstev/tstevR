#' Check if a \code{R} object is indeed empty
#'
#' A helper function to check whether a \code{R} object is indeed empty mostly
#' used internally.
#'
#' @param x a \code{R} object
#' @param trim a logical scalar which controls to trim leading and trailing
#'  whitespaces with a character input.
#'
#' @return a logical vector.
#'
#' @examples
#'
#' # Empty strings
#' is_empty(" ")               # TRUE
#' is_empty(character(0))      # TRUE
#' is_empty(" ", trim = FALSE) # FALSE
#'
#' # Case where it returns TRUE
#' is_empty(0)
#' is_empty(numeric(0))
#' @export is_empty
is_empty <- function(x, trim = TRUE) {
  if (length(x) <= 1L) {
    if (is.null(x))
      return (TRUE)
    if (length(x) == 0L)
      return (TRUE)
    if (is.na(x) || is.nan(x))
      return (TRUE)
    if (is.character(x) && str_count(ifelse(trim, str_trim(x), x)) == 0)
      return (TRUE)
    if (is.logical(x) && !isTRUE(x))
      return (TRUE)
    if (is.numeric(x) && x == 0)
      return (TRUE)
    return (FALSE)
  } else {
    sapply(x, is_empty, trim = trim, USE.NAMES = FALSE)
  }
}
