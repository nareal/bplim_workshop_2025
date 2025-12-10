# This function implements an optimized binomial tree algorithm for
# valuing American put options using Cox-Ross-Rubinstein parameters.
# Optimization:
# Uses vectors instead of matrices and calculates asset prices on the fly
# and removes one loop in relation to ap_v1.

ap_v2 <- function(S0, K, r, q, tt, sigma, steps) {
  # S0: Initial stock price
  # K: Strike price
  # r: Risk-free interest rate compounded continuously
  # q: Dividend yield compounded continuously
  # tt: Time to maturity in years
  # sigma: Volatility of the underlying stock
  # steps: Number of time steps in the binomial tree

  dt <- tt / steps # time between steps
  u <- exp(sigma * sqrt(dt)) # Up factor
  d <- exp(-sigma * sqrt(dt)) # Down factor
  p <- (exp((r - q) * dt) - d) / (u - d) # Risk-neutral probability
  disc <- exp(-r * dt) # Discount factor

  # Vector to store option values
  option_values <- numeric(steps + 1)

  # Backward induction from maturity to root
  for (j in (steps + 1):1) {
    num_nodes <- if (j == steps + 1) steps + 1 else j

    for (i in 1:num_nodes) {
      # Calculate asset price at this node
      asset_price <- S0 * u^(j - i) * d^(i - 1)

      if (j == steps + 1) {
        # At maturity: option value is intrinsic value
        option_values[i] <- max(K - asset_price, 0)
      } else {
        # Before maturity: calculate continuation value and check early exercise
        continuation_value <- (p *
          option_values[i] +
          (1 - p) * option_values[i + 1]) *
          disc
        intrinsic_value <- K - asset_price
        option_values[i] <- max(continuation_value, intrinsic_value)
      }
    }
  }

  return(option_values[1]) # Option price at the root node
}
