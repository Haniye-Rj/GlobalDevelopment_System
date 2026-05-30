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

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calculate_priority_scores_cpp(NumericMatrix pca_scores) {
  
  int n = pca_scores.nrow();
  int p = pca_scores.ncol();
  
  NumericVector distances(n);
  
  // Target point:
  // high PC1 and low PC2 = reference development point
  double target_pc1 = max(pca_scores(_, 0));
  double target_pc2 = min(pca_scores(_, 1));
  
  for (int i = 0; i < n; i++) {
    
    double sum_sq = 0.0;
    
    for (int j = 0; j < p; j++) {
      
      double target_value;
      
      if (j == 0) {
        target_value = target_pc1;
      } else if (j == 1) {
        target_value = target_pc2;
      } else {
        target_value = 0.0;
      }
      
      double diff = pca_scores(i, j) - target_value;
      sum_sq += diff * diff;
    }
    
    distances[i] = sqrt(sum_sq);
  }
  
  // Normalize scores to 0–100
  double min_dist = min(distances);
  double max_dist = max(distances);
  
  NumericVector priority_scores(n);
  
  for (int i = 0; i < n; i++) {
    if (max_dist == min_dist) {
      priority_scores[i] = 0;
    } else {
      priority_scores[i] = 
        ((distances[i] - min_dist) / (max_dist - min_dist)) * 100;
    }
  }
  
  return priority_scores;
}