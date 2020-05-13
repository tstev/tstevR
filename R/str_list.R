#' List items
#'
#' `str_list()` wraps [stringr::str_c()] in order to turn a list of character
#' strings into a grammatically correct list without an
#' [Oxford Comma](https://en.wikipedia.org/wiki/Serial_comma) by default.
#' Sue me.
#'
#' @param string an object coercible to a character vector.
#' @param sep a string to seperate elements except last.
#' @param last a string to seperate last element.
#'
#' @return a character string.
#' @importFrom stringr str_c
#' @export str_list
#'
#' @examples
#' shopping_list <- c("bread", "milk", "eggs")
#' str_list(shopping_list)
str_list <- function(string, sep = ", ", last = " and ") {
  # Coerce to make sure it is a character vector
  x <- as.character(string)
  if(length(x) == 1L) return(x)
  stringr::str_c(c(stringr::str_c(x[1:length(x)-1L], collapse = sep),
                   x[length(x)]),
                 collapse = last)
}
