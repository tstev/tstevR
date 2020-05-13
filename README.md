
# tstevR

<!-- badges: start -->
<!-- badges: end -->

The goal of `tstevR` was to learn how to write packages. Best start somewhere. At the same time it served as a collection of functions I have found useful over my short experience with `R`.

## Installation

This package will likely not be on [CRAN](https://CRAN.R-project.org) ever and will likely die slow and painful here on [GitHub](https://github.com/tstev/tstevR). Nonetheless if for some reason a wanderer wishes to install this package this can be done as follows:

``` r
# install.packages("devtools")
devtools::install_github("tstev/tstevR")
```

## Example

Perhaps the only thing I am proud of in this package is the sparse iterative solver. It uses the (diagonal, only implemented at the moment) preconditioned [conjugate gradient](https://en.wikipedia.org/wiki/Conjugate_gradient_method) solver from the [Eigen C++ library](http://eigen.tuxfamily.org) using [RcppEigen](https://github.com/RcppCore/RcppEigen) package.

``` r
# Same example from RcppEigen::fastLM
library(tstevR)
data(trees, package="datasets")
mm <- cbind(1, log(trees$Girth))   # model matrix
y  <- log(trees$Volume)            # response

coef(sparse_lm(mm, y))
coef(RcppEigen::fastLmPure(mm, y))
unname(coef(lm.fit(mm, y)))
```

## References

Head over to [Rcpp Gallery](https://gallery.rcpp.org/) for some awesome examples using the amazing `Rcpp` package that also served as a source for the `sparse_lm()` function in this package. In addition to the help on [StackOverflow](https://stackoverflow.com/questions/43378812/sparseqr-for-least-squares) from [Dirk Eddelbuettel](http://dirk.eddelbuettel.com) (author of many packages including `Rcpp`) and [The Coatless Professor](https://thecoatlessprofessor.com/about/) (aka James Balamuta).

## Code of Conduct

Please note that the tstevR project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

