#include <Rcpp.h>
using namespace Rcpp;

// Euclidean distance among elements of two matrices
// @param x a numeric matrix with each element as a row and variables as columns
// @param y a second matrix for distance calculation
// @return matrix of pairwise distances
// [[Rcpp::export]]

NumericMatrix eucdist_mm(NumericMatrix x, NumericMatrix y) {
  int n1 = x.nrow(), n2 = y.nrow(), ncol = x.ncol(), i, j, k;
  NumericMatrix out(n1, n2);

  for(i = 0; i < n1; i++) {
    for(j = 0; j < n2; j++) {
      double sum = 0;
      for(k = 0; k < ncol; k++) {
        sum += pow(x(i, k) - y(j, k), 2);
      }
      out(i, j) = sqrt(sum);
    }
  }

  return out;
}
