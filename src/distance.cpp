#include <Rcpp.h>
using namespace Rcpp;

//' @export
 // [[Rcpp::export]]
 double calculate_euclidean(NumericVector x, NumericVector y) {
   int n = x.size();
   double sum = 0;
   for(int i = 0; i < n; i++) {
     sum += pow(x[i] - y[i], 2);
   }
   return sqrt(sum);
 }