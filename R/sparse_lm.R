#' Fit Linear Model using sparse matrices
#'
#' `sparse_lm` uses sparse matrices and a iterative solver to fit linear models.
#' The sparse iterative solvers are from the `Eigen` C++ library.
#' Currently, only the Conjugate Gradient algorithm has been implemented with
#' a diagonal preconditioner.
#'
#' @param X a sparse matrix of the class \code{\link[Matrix]{dgCMatrix-class}}.
#' @param y a numeric vector with response variable.
#' @param x0 a numeric vector of length `ncol(X)` with an initial guess.
#'  If it's not provided it will default to a zero-vector.
#' @param maxiter an integer scalar with the maximum number of iterations.
#'  A good choice is usually `4 * ncol(X)`.
#' @param tol a numeric scalar with the relative error tolerance.
#'
#' @return A list with the elements
#'
#'   \item{coefficients}{The estimated coefficients from solver.}
#'   \item{itr}{A scalar denoting the number of iterations.}
#'   \item{error}{A scalar denoting the the relative error at the last iteration.}
#'   \item{fitted.values}{A vector with the fitted values.}
#'   \item{residuals}{The vector with the residuals.}
#'
#'
#' @seealso See
#' \href{http://eigen.tuxfamily.org}{Eigen C++ library} for documentation on
#' `Eigen` library and
#' \href{https://en.wikipedia.org/wiki/Conjugate_gradient_method}{Conjugate Gradient}
#' for more information about this method.
#'
#' @importMethodsFrom Matrix %*%
#' @importClassesFrom Matrix dgCMatrix
#' @importFrom Matrix crossprod
#' @importFrom methods as
#' @importFrom stats coef fitted
#'
#' @keywords regression models
#' @export sparse_lm
sparse_lm <- function(X, y, x0, maxiter = 4L * ncol(X),
                      tol = .Machine$double.eps) {
  # TODO: cpp implementation (fcrossprod1)

  # Check input paramters
  if (!inherits(X, "dgCMatrix")) X <- as(X, "dgCMatrix")

  # If no initial guess (i.e. old solutions) is provided start with zero-vector
  # of length equal to the number of columns (i.e. number of coefficients)
  m <- dim(X)[[1L]] # number of rows
  n <- dim(X)[[2L]] # number of cols

  # Check additional arguments
  if (missing(x0)) x0 <- vector("double", n)

  # Ensure input matrix is symmetric
  if (isTRUE(m != n)) {
    A <- as(Matrix::crossprod(X), "dgCMatrix")
    b <- as.vector(Matrix::crossprod(X, y))
    fit <- sparse_cg(A, b, x0 = x0, maxiter = maxiter, tol = tol)
  } else if (isTRUE(m == n)) {
    fit <- sparse_cg(X, y, x0 = x0, maxiter = maxiter, tol = tol)
  }

  # Check convergance
  if (isTRUE(fit[["itr"]] == maxiter)) {
    msg <- paste0(
      "Maximum iterations (", fit[["itr"]],
      ") reached with a relative",
      " error of ", fit[["error"]]
    )
    warning(msg, call. = FALSE)
  }

  # Calculate fitted values and residuals
  fit$fitted.values <- as.vector(X %*% coef(fit))
  fit$residuals <- y - fitted(fit)

  # Return list
  return(fit)
}
