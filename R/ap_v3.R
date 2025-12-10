# This function implements an optimized binomial tree algorithm for
# valuing American put options using Cox-Ross-Rubinstein parameters.
# In relation to ap_v2 includes defensive programming with input validation.

ap_v3 <- function(S0, K, r, q, tt, sigma, steps) {
  # S0: Initial stock price
  # K: Strike price
  # r: Risk-free interest rate compounded continuously
  # q: Dividend yield compounded continuously
  # tt: Time to maturity in years
  # sigma: Volatility of the underlying stock
  # steps: Number of time steps in the binomial tree

  # Input validation
  # Check for missing arguments
  if (
    missing(S0) ||
      missing(K) ||
      missing(r) ||
      missing(q) ||
      missing(tt) ||
      missing(sigma) ||
      missing(steps)
  ) {
    stop("All arguments must be provided")
  }

  # Check for NA or NULL values
  if (
    any(is.na(c(S0, K, r, q, tt, sigma, steps))) ||
      any(is.null(c(S0, K, r, q, tt, sigma, steps)))
  ) {
    stop("Arguments cannot be NA or NULL")
  }

  # Check that all inputs are numeric
  if (
    !is.numeric(S0) ||
      !is.numeric(K) ||
      !is.numeric(r) ||
      !is.numeric(q) ||
      !is.numeric(tt) ||
      !is.numeric(sigma) ||
      !is.numeric(steps)
  ) {
    stop("All arguments must be numeric")
  }

  # Check that inputs are scalar (length 1)
  if (
    length(S0) != 1 ||
      length(K) != 1 ||
      length(r) != 1 ||
      length(q) != 1 ||
      length(tt) != 1 ||
      length(sigma) != 1 ||
      length(steps) != 1
  ) {
    stop("All arguments must be scalar values")
  }

  # Validate parameter ranges
  if (S0 <= 0) {
    stop("Initial stock price (S0) must be positive")
  }

  if (K <= 0) {
    stop("Strike price (K) must be positive")
  }

  if (tt <= 0) {
    stop("Time to maturity (tt) must be positive")
  }

  if (sigma <= 0) {
    stop("Volatility (sigma) must be positive")
  }

  if (q < 0) {
    stop("Dividend yield (q) must be non-negative")
  }

  if (steps < 1) {
    stop("Number of steps must be at least 1")
  }

  # Check that steps is an integer
  if (steps != floor(steps)) {
    stop("Number of steps must be an integer")
  }

  # Calculate parameters
  dt <- tt / steps # time between steps
  u <- exp(sigma * sqrt(dt)) # Up factor
  d <- exp(-sigma * sqrt(dt)) # Down factor
  p <- (exp((r - q) * dt) - d) / (u - d) # Risk-neutral probability

  # Validate risk-neutral probability (arbitrage-free condition)
  if (p < 0 || p > 1) {
    stop(sprintf(
      "Risk-neutral probability p=%.4f is outside [0,1]. Model parameters lead to arbitrage.",
      p
    ))
  }

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
