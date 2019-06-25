#include <RcppEigen.h>
#include <Eigen/IterativeLinearSolvers>
// [[Rcpp::depends(RcppEigen)]]
//' Conjugate Gradient solver for Sparse Matrices.
//'
//' Using the iterative Conjugate Gradient Solver from the Eigen C++ library
//' it solves a linear system \code{A * x = b}. Currently, it uses a diagonal
//' preconditioner.
//'
//' @param A Sparse double matrix of class \linkS4class{dgCMatrix}.
//' @param b Numeric vector with length equal to number of rows of \code{A}.
//' @param x0 Numeric vector with same length as \code{b}. If no initial guess
//'     is known a vector of zeros with length of \code{b} can be passed.
//' @param maxiter Integer scalar that sets the maximum number of iterations
//'     allowed by the solver. Default is 10,000.
//' @param tol Sets the tolerance threshold (upper bound to the relative
//'     residual error) used by the stopping criteria. Default is \code{1e-6}.
//' @return \code{sparse_cg} returns a list with several components:
//'     \item{coefficients}{a vector with the estimated coefficients}
//'     \item{itr}{a scalar denoting the number of iterations at convergence}
//'     \item{err}{a scalar denoting the relative error at convergance}
//' @seealso See \href{http://www.eigen.tuxfamily.org/}{Eigen Library} for more
//'     documentation on solver used and other available solvers.
//' @export
// [[Rcpp::export]]
Rcpp::List sparse_cg(const Eigen::Map<Eigen::SparseMatrix<double> > &A,
                     const Eigen::Map<Eigen::VectorXd> &b,
                     const Eigen::Map<Eigen::VectorXd> &x0,
                     const int &maxiter = 10000L,
                     const double &tol = 1e-6) {

  if(A.rows() != A.cols()) {
    throw Rcpp::exception("The input matrix 'A' must be square.");
  }

  if(b.size() != x0.size()) {
    throw Rcpp::exception("The RHS vector 'b' and the initial guess 'x0' must \
                           be of the same lenghts.");
  }

  if(b.size() != A.rows()) {
    throw Rcpp::exception("The RHS vector 'b' must have same number of rows as\
                            input matrix 'B'.");
  }

  // Set-up the Conjugate Gradient solver
  Eigen::ConjugateGradient<Eigen::SparseMatrix<double>,
                           Eigen::Lower|Eigen::Upper,
                           Eigen::DiagonalPreconditioner<double> > cg;
  // Initialize solver
  cg.setMaxIterations(maxiter);
  cg.setTolerance(tol);
  cg.compute(A);

  // Solve system with guess (i.e. old solutions)
  Eigen::VectorXd coef = cg.solveWithGuess(b, x0);

  // Solver convergence stats
  int iter = cg.iterations();
  double err = cg.error();

  return Rcpp::List::create(Rcpp::Named("coefficients") = coef,
                            Rcpp::Named("itr") = iter,
                            Rcpp::Named("error") = err);
}
