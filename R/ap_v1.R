# This function implements an optimized binomial tree algorithm for
# valuing American put options using Cox-Ross-Rubinstein parameters.
# Optimization:
# Uses vectors instead of matrices and calculates asset prices on the fly.

ap_v1 <- function(S0, K, r, q, tt, sigma, steps) {
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

  # Initialize option values at maturity
  # Vector size is steps + 1 (for nodes 0 to steps)
  option_values <- numeric(steps + 1)

  # Calculate option values at maturity
  for (i in 1:(steps + 1)) {
    # At maturity, node i has (steps + 1 - i) up moves and (i - 1) down moves
    asset_price <- S0 * u^(steps + 1 - i) * d^(i - 1)
    option_values[i] <- max(K - asset_price, 0)
  }

  # Backward induction to calculate option price at earlier nodes
  for (j in steps:1) {
    # At time step j, we have j nodes
    for (i in 1:j) {
      # Calculate continuation value
      option_values[i] <- (p *
        option_values[i] +
        (1 - p) * option_values[i + 1]) *
        disc

      # Calculate asset price for early exercise check
      # At time step j-1, node i has (j - i) up moves and (i - 1) down moves
      asset_price <- S0 * u^(j - i) * d^(i - 1)

      # Check for early exercise
      intrinsic_value <- K - asset_price
      option_values[i] <- max(option_values[i], intrinsic_value)
    }
  }

  return(option_values[1]) # Option price at the root node
}
