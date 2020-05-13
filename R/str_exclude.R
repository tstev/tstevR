#' Exclude strings matching a pattern, or find positions.
#'
#' `str_exclude()` is a wrapper function (much like [stringr::str_subset])
#' around `x[!str_detect(x, pattern)]`, and is equivalent to
#' `grep(pattern, x, value = TRUE, invert = TRUE)`. `str_which_not()` is a
#' wrapper around `which(str_detect(x, pattern))`, and is equivalent to
#' `grep(pattern, x, invert = TRUE)`.
#'
#' If the `stringr` package is installed it is vectorized over `string` and
#' `pattern`. Otherwise [base::grep()] is used which is not vectorized over
#' `pattern` and only the first element is used.
#'
#' As of `stringr >= 1.4.0` the function [stringr::str_subset()] gained a
#' `negate` argument, rendering this function obsolete, unnecessary and
#' convoluted. Well there you are - c'est la vie.
#'
#' @param string a input vector that is coercible to a character vector.
#' @param pattern pattern to look for and exclude.
#'
#' @return a character or integer vector for `str_exclude` and `str_which_not`,
#'    respectively.
#' @seealso [base::grep()] with argument `invert = TRUE` and `value = TRUE` and
#'    [stringr::str_detect()] for the function it wraps and for the underlying
#'    implementation.
#'
#' @importFrom stringr str_detect
#' @export str_exclude
#' @examples
#' fruits <- c("apple", "avocado", "banana")
#' str_exclude(fruits, "ap")
#' str_which_not(fruits, "^a")
str_exclude <- function(string, pattern) {
  if (requireNamespace("stringr", quietly = TRUE)) {
    string[!stringr::str_detect(string, pattern)]
  } else {
    grep(pattern, string, value = TRUE, invert = TRUE)
  }
}

#' @export str_which_not
#' @rdname str_exclude
str_which_not <- function(string, pattern) {
  if (requireNamespace("stringr", quietly = TRUE)) {
    which(!stringr::str_detect(string, pattern))
  } else {
    grep(pattern, string, value = TRUE, invert = TRUE)
  }
}
