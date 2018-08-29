#' Exclude strings matching a pattern, or find positions.
#'
#' `str_exclude()` is a wrapper function (much like [stringr::str_subset])
#' around `x[!str_detect(x, pattern)]`, and is equivalent to
#' `grep(pattern, x, value = TRUE, invert = TRUE)`. `str_which_not()` is a
#' wrapper around `which(str_detect(x, pattern))`, and is equivalent to
#' `grep(pattern, x, invert = TRUE)`.
#'
#' @param string a input vector that is coercible to a character vector.
#' @param pattern pattern to look for and exclude.
#'
#' @return a character or integer vector for `str_exclude` and `str_which_not`,
#'    respectively.
#' @seealso [grep()] with argument `invert = TRUE` and `value = TRUE` and
#'    [stringr::str_detect] for the function it wraps and for the underlying
#'    implementation.
#' @export
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

#' @export
#' @rdname str_exclude
str_which_not <- function(string, pattern) {
  if (requireNamespace("stringr", quietly = TRUE)) {
    which(!stringr::str_detect(string, pattern))
  } else {
    grep(pattern, string, value = TRUE, invert = TRUE)
  }
}

# x <- fruit[1:4]
# str_exclude(x, "ap")
# str_exclude(x, "^a")
# str_which_not(x, "ap")
# str_which_not(x, "^a")
#
#
# str_list <- function(..., sep = ",", last = " and ") {
#   y <- list(x, "blah")
#   lapply(y, str_c, sep = sep)
# }
#
# str_list(x, "blah")
