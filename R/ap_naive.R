# This function implements a bionomial tree algorithm for
# valuing American put options using Cox-Ross-Rubinstein parameters.

ap_naive <- function(S0, K, r, q, tt, sigma, steps) {
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

  # Initialize matrices for asset prices and option values
  # Rows represent nodes (i), columns represent time steps (j)
  asset_prices <- matrix(NA, nrow = steps + 1, ncol = steps + 1)
  option_values <- matrix(NA, nrow = steps + 1, ncol = steps + 1)

  # Fill in asset prices for all nodes in the tree (lower triangular)
  for (j in 1:(steps + 1)) {
    for (i in (steps + 2 - j):(steps + 1)) {
      asset_prices[i, j] <- S0 * u^(steps + 1 - i) * d^(i + j - steps - 2)
    }
  }

  # Initialize option values at maturity (last column)
  for (i in 1:(steps + 1)) {
    option_values[i, steps + 1] <- max(K - asset_prices[i, steps + 1], 0)
  }

  # Backward induction to calculate option price at earlier nodes
  # i - node, j - time step
  for (j in steps:1) {
    for (i in (steps + 2 - j):(steps + 1)) {
      # Calculate continuation value
      option_values[i, j] <-
        (p * option_values[i - 1, j + 1] + (1 - p) * option_values[i, j + 1]) *
        exp(-r * dt)

      # Check for early exercise
      intrinsic_value <- K - asset_prices[i, j]
      option_values[i, j] <- max(option_values[i, j], intrinsic_value)
    }
  }

  return(option_values[steps + 1, 1]) # Option price at the root node
}
