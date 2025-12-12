// This is the C++ core implementation of the American put option pricing
// algorithm using the Cox-Ross-Rubinstein binomial tree model.
// It is designed to be called from R via the ap_v3_rcpp() wrapper function.

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double ap_v3_cpp(double S0, double K, double r, double q,
                 double tt, double sigma, int steps) {
  // S0: Initial stock price
  // K: Strike price
  // r: Risk-free interest rate compounded continuously
  // q: Dividend yield compounded continuously
  // tt: Time to maturity in years
  // sigma: Volatility of the underlying stock
  // steps: Number of time steps in the binomial tree

  // Calculate binomial tree parameters
  double dt = tt / steps;                          // Time between steps
  double u = exp(sigma * sqrt(dt));                // Up factor
  double d = exp(-sigma * sqrt(dt));               // Down factor
  double p = (exp((r - q) * dt) - d) / (u - d);   // Risk-neutral probability
  double disc = exp(-r * dt);                      // Discount factor

  // Vector to store option values at each node
  NumericVector option_values(steps + 1);

  // Backward induction from maturity to root
  // Note: C++ uses 0-based indexing, so j goes from steps to 0
  for (int j = steps; j >= 0; j--) {
    // Number of nodes at this time step
    int num_nodes = (j == steps) ? (steps + 1) : (j + 1);

    for (int i = 0; i < num_nodes; i++) {
      // Calculate asset price at this node
      // Formula: S0 * u^(j-i) * d^i
      double asset_price = S0 * pow(u, j - i) * pow(d, i);

      if (j == steps) {
        // At maturity: option value is intrinsic value
        option_values[i] = std::max(K - asset_price, 0.0);
      } else {
        // Before maturity: calculate continuation value and check early exercise
        double continuation_value = (p * option_values[i] +
                                    (1 - p) * option_values[i + 1]) * disc;
        double intrinsic_value = K - asset_price;
        option_values[i] = std::max(continuation_value, intrinsic_value);
      }
    }
  }

  // Return the option price at the root node
  return option_values[0];
}
