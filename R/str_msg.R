#' Message friendly string interpolation.
#'
#' This is a wrapper function around [stringr::str_interp()] and
#' [stringr::str_c()]. It aids in creating useful messages which can
#' be passed to [base:warning()] or [base::stop], for example, or other 
#' logging functions. 
#'
#' @param ... one or more character vectors which are passed to
#'    [stringr::str_c()]. Multiple strings are automatically joined
#'    with a space as seperator.
#' @param env the environment in which to evaluate the expressions.
#'
#' @return an interpolated character string.
#'
#' @seealso [stringr::str_interp()] and [stringr::str_c()] which this 
#'    function wraps for further documenation or examples on string
#'    interpolation or the joining of strings.
#' @import stringr
#' @export str_msg
#' @examples
#' user_name <- "World"
#' str_msg("Hello ${user_name}!", "Welcome to tidyverse.")
str_msg <- function(..., env = parent.frame()) {
  str_interp(str_c(..., sep = " ", collapse = " "), env = env)
}
