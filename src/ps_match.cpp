#include <Rcpp.h>
using namespace Rcpp;

// This is an example function we are creating as a part of PHS 7045 lab 7.
// Propensity score matching.


//' propensity score matching roxygen from Rcpp
//' @return A list with two vectors (`x`, `y`).
//' @param x Numeric scalar. A vector of values we want to find matches to.
//' @examples
//' #creates 10 random poisson variables to calculates propensity scores
//' x<-rpois(n=10,lambda=3)
//' ps_match(x)
//' @export
// [[Rcpp::export]]
List ps_match(const NumericVector & x) {

  int n = static_cast<int>(x.size());

  IntegerVector indices(n);
  NumericVector values(n);
  values.fill(std::numeric_limits< double >::max());

  for (int i = 0; i < n; ++i) {

    // Instead of allocating new memory, we can point by reference
    // (saves operations)
    double & cur_best = values[i];
    auto & cur_i    = indices[i];

    for (int j = 0; j < i; ++j) {

      // If it is lower, then update
      double d = std::abs(x[i] - x[j]);
      if (d < cur_best) {

        cur_best = d;
        cur_i    = j;

      }

      if (d < values[j]) {

        values[j] = d;
        indices[j] = i;

      }

    }

  }

  for (int i = 0; i < n; ++i)
    values[i] = x[indices[i]];

  return List::create(
    _["match_id"] = indices + 1, // We add one to match R's indices
    _["match_x"]  = values
  );

}



// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
