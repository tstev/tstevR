#' Pad a string (with better defaults).
#'
#' These functions just wrap around [stringr::str_pad()] with some better
#' defaults (imho). If `width` is missing it defaults to length of the
#' `string + 1` for `str_lpad()` and `str_rpad()` and `+ 2` for `str_bpad()`.
#'
#' @inheritParams stringr::str_pad
#'
#' @return a padded character vector.
#' @name str_padding
#'
#' @seealso [stringr::str_pad()] for more examples.
#'
#' @examples
#' rbind(
#'   str_lpad("hadley", 30, "_"),
#'   str_rpad("hadley", 30, "-"),
#'   str_bpad("hadley", 30, "'")
#' )
#' @importFrom stringr str_pad str_count
NULL

#' @rdname str_padding
#' @export str_lpad
str_lpad <- function(string, width, pad = " ") {
  if (missing(width)) width <- stringr::str_count(string) + 1L
  stringr::str_pad(string = string, width = width, side = "left", pad = pad)
}

#' @rdname str_padding
#' @export str_rpad
str_rpad <- function(string, width, pad = " ") {
  if (missing(width)) width <- stringr::str_count(string) + 1L
  stringr::str_pad(string = string, width = width, side = "right", pad = pad)
}

#' @rdname str_padding
#' @export str_bpad
str_bpad <- function(string, width, pad = " ") {
  if (missing(width)) width <- stringr::str_count(string) + 2L
  stringr::str_pad(string = string, width = width, side = "both", pad = pad)
}
