# This function wraps the C++ implementation (ap_v3_cpp) with R-based input
# validation. It provides the same interface as ap_v3 with improved performance.
# The validation code is identical to ap_v3 to ensure the same error checking.

ap_v3_rcpp_par <- function(S0, K, r, q, tt, sigma, steps) {
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

  # Calculate risk-neutral probability for validation
  dt <- tt / steps
  u <- exp(sigma * sqrt(dt))
  d <- exp(-sigma * sqrt(dt))
  p <- (exp((r - q) * dt) - d) / (u - d)

  # Validate risk-neutral probability (arbitrage-free condition)
  if (p < 0 || p > 1) {
    stop(sprintf(
      "Risk-neutral probability p=%.4f is outside [0,1]. Model parameters lead to arbitrage.",
      p
    ))
  }

  # Check if C++ function is available in this process
  # If not, compile it (this happens once per parallel worker)
  if (!exists("ap_v3_cpp", mode = "function")) {
    # Suppress compilation messages to avoid cluttering parallel output
    suppressMessages({
      Rcpp::sourceCpp(here::here("R", "ap_v3_rcpp.cpp"))
    })
  }

  # Call C++ implementation
  return(ap_v3_cpp(S0, K, r, q, tt, sigma, steps))
}
