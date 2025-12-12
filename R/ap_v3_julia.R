# R wrapper for Julia implementation of American put option pricing
# Uses JuliaCall package to interface with Julia

# Load required package
if (!require("JuliaCall", quietly = TRUE)) {
  stop(
    "JuliaCall package is required. Install it with: install.packages('JuliaCall')"
  )
}

# Initialize Julia (only needs to be done once per session)
# This function will set up the Julia connection
init_julia_ap <- function(num_threads = "auto") {
  # Set the number of Julia threads before initialization
  # Options: "auto" for all cores, or a specific number like "8"
  Sys.setenv(JULIA_NUM_THREADS = as.character(num_threads))

  # Initialize Julia
  julia_setup()

  # Use here package to reliably find the project root
  if (!require("here", quietly = TRUE)) {
    stop("here package is required. Install it with: install.packages('here')")
  }

  # Source the Julia file from the R directory
  julia_file <- here::here("R", "ap_v3.jl")

  if (!file.exists(julia_file)) {
    stop(paste("Julia file not found:", julia_file))
  }

  julia_source(julia_file)

  # Report thread count
  nthreads <- julia_eval("Threads.nthreads()")
  message(sprintf("Julia initialized with %d threads and ap_v3.jl loaded successfully", nthreads))
}

# R function that calls the Julia implementation
ap_v3_julia <- function(S0, K, r, q, tt, sigma, steps) {
  # Basic input validation (Julia will do more thorough validation)
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

  # Call the Julia function
  result <- julia_call(
    "american_put_binomial",
    S0,
    K,
    r,
    q,
    tt,
    sigma,
    as.integer(steps)
  )

  return(result)
}

# Convenience function that combines initialization and execution
# Use this if you want a single function call
ap_v3_julia_auto <- function(
  S0,
  K,
  r,
  q,
  tt,
  sigma,
  steps,
  force_reinit = FALSE
) {
  # Check if Julia is already initialized
  if (!exists(".julia_initialized", envir = .GlobalEnv) || force_reinit) {
    init_julia_ap()
    assign(".julia_initialized", TRUE, envir = .GlobalEnv)
  }

  ap_v3_julia(S0, K, r, q, tt, sigma, steps)
}

# Vectorized batch function that processes multiple options in Julia using multi-threading
# This is much more efficient than calling ap_v3_julia in a loop or using R parallelization
ap_v3_julia_batch <- function(S0, K, r, q, tt, sigma, steps) {
  # Input validation - all vectors must have the same length
  n <- length(S0)

  if (length(K) != n || length(r) != n || length(q) != n ||
      length(tt) != n || length(sigma) != n) {
    stop("All parameter vectors must have the same length")
  }

  # Handle steps - can be a single value or a vector
  if (length(steps) == 1) {
    steps <- rep(steps, n)
  } else if (length(steps) != n) {
    stop("steps must be either a single value or a vector of the same length as other parameters")
  }

  # Call the Julia batch function
  result <- julia_call(
    "american_put_binomial_batch",
    S0,
    K,
    r,
    q,
    tt,
    sigma,
    as.integer(steps)
  )

  return(result)
}
