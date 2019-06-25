#' Coalesce
#'
#' This function returns the first non-`NA` argument similar to the `SQL`
#' function `COALESCE(...)`. Coalescing is done element by element.
#'
#' @param ... vectors with values to coalesce.
#'
#' @return a vector of same length as the first input.
#'
#' @seealso See the following
#'  \href{https://stackoverflow.com/a/19257945/4524755}{StackOverflow} answer
#'  for the source of this function.
#'
#' @examples
#' coalesce(c("A", "B", NA, "D"), c("A", "B", "C", "D"))
#' coalesce(NA, NA, 3)
#' coalesce(c(1,NA,NA), c(NA,2))
#'
#' @export coalesce
coalesce <- function (...) {
  if (missing(..1)) {
    stop("At least one argument must be supplied", call. = FALSE)
  }

  values <- list(...)[-1L]
  ans <- ..1
  for (x in values) {
    i <- which(is.na(ans))
    if (length(x) == 1L) {
      ans[i] <- x[[1L]]
    } else {
      ans[i] <- x[i]
    }
  }
  ans
}
