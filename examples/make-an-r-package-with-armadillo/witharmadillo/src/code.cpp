#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;
// Generated from create-witharmadillo.Rmd: do not edit by hand
// [[Rcpp::depends(RcppArmadillo)]]
using namespace arma;

//' Cholesky decomposition
//' 
//' @param X A positive definite matrix
//' @export
// [[Rcpp::export]]
arma::mat my_chol(arma::mat& X) {
  return arma::chol(X);
}
