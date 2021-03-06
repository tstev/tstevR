% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coalesce.R
\name{coalesce}
\alias{coalesce}
\title{Coalesce}
\usage{
coalesce(...)
}
\arguments{
\item{...}{vectors with values to coalesce.}
}
\value{
a vector of same length as the first input.
}
\description{
This function returns the first non-\code{NA} argument similar to the \code{SQL}
function \code{COALESCE(...)}. Coalescing is done element by element.
}
\examples{
coalesce(c("A", "B", NA, "D"), c("A", "B", "C", "D"))
coalesce(NA, NA, 3)
coalesce(c(1,NA,NA), c(NA,2))

}
\seealso{
See the following
\href{https://stackoverflow.com/a/19257945/4524755}{StackOverflow} answer
for the source of this function.
}
