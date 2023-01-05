#include <Rcpp.h>
using namespace Rcpp;
// Generated from create-withrcpp.Rmd: do not edit by hand

//' Alternately flip the signs of elements of a vector
//' 
//' @param v A numerical vector
//' @export
// [[Rcpp::export]]
NumericVector alternate_signs(NumericVector v) {
  int n = v.size();
  NumericVector out(n);
  for(int i = 0; i < n; i++) {
    if (i % 2 == 0) {
      out[i] = v[i];
    }
    else {
      out[i] = -v[i];
    }
  }
  return out;
}
