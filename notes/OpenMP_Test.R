# This installation allows to compile programs that make use of multithreading

# https://stackoverflow.com/questions/70638118/configuring-compilers-on-mac-m1-big-sur-monterey-for-rcpp-and-other-tools

# Here is a link to an alternative approach by Simon Urbanek -- NOT USED HERE!
# https://mac.r-project.org/openmp/

if (!requireNamespace("RcppArmadillo", quietly = TRUE)) {
  install.packages("RcppArmadillo")
}

Rcpp::sourceCpp(code = '
#include <RcppArmadillo.h>
#ifdef _OPENMP
# include <omp.h>
#endif

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
void omp_test()
{
#ifdef _OPENMP
    Rprintf("OpenMP threads available: %d\\n", omp_get_max_threads());
#else
    Rprintf("OpenMP not supported\\n");
#endif
}
')
omp_test()

# OpenMP threads available: 10
