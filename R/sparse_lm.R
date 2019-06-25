#' Fit Linear Model using sparse matrices
#'
#' \code{sparse_lm} uses sparse matrices and a iterative solver to fit linear
#'     models. The sparse iterative solvers are from the \code{Eigen} C++
#'     library. Currently, only the Conjugate Gradient algorithm has been
#'     implemented with a diagonal preconditioner.
#'
#' @param X a sparse matrix of the class \code{\link[Matrix]{dgCMatrix-class}}.
#' @param y a numeric vector with response variable.
#' @param x0 a numeric vector of length \code{ncol(X)} with an initial guess.
#'  If it's not provided it will default to a zero-vector.
#' @param maxiter an integer scalar with the maximum number of iterations.
#'  A good choice is usually \code{4 * ncol(X)}. Defaults to \code{10000L}.
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
#' \code{Eigen} library and
#' \href{https://en.wikipedia.org/wiki/Conjugate_gradient_method}{Conjugate Gradient}
#' for more information about this method.
#'
#' @importMethodsFrom Matrix %*%
#' @importClassesFrom Matrix dgCMatrix
#'
#' @keywords regression models
#' @export
sparse_lm <- function(X, y, x0, maxiter, tol) {
  # Check input paramters
  if (!inherits(X, "dgCMatrix")) X <- as(X, "dgCMatrix")

  # If no initial guess (i.e. old solutions) is provided start with zero-vector
  # of length equal to the number of column (i.e. number of coefficients)
  m <- dim(X)[[1L]] # number of rows
  n <- dim(X)[[2L]] # number of cols

  # Check additional arguments
  if (missing(x0)) x0 <- vector("double", n)
  if (missing(maxiter)) maxiter <- getOption("outlier.maxiter")
  if (missing(tol)) tol <- getOption("outlier.tol")

  # Ensure input matrix is symmetric
  if (isTRUE(m != n)) {
    A <- as(Matrix::crossprod(X), "dgCMatrix") # TODO: cpp implementation (fcrossprod1)
    b <- as.vector(Matrix::crossprod(X, y))    # TODO: idem
    fit <- sparse_cg(A, b, x0 = x0, maxiter = maxiter, tol = tol)
  } else if (isTRUE(m == n)) {
    fit <- sparse_cg(X, y, x0 = x0, maxiter = maxiter, tol = tol)
  }
  class(fit) <- "sparseLM"

  # Check convergance
  iters <- fit[["itr"]]
  error <- fit[["error"]]
  if (isTRUE(iters == maxiter))
    warning(str_msg("Maximum iterations (${iters}) reached",
                    "with a relative error of ${error}."))

  # Calculate fitted values and residuals
  fit$fitted.values <- as.vector(X %*% coef(fit)) # TODO: cpp implementation
  fit$residuals <- y - fitted(fit)                # TODO: idem
  fit
}
