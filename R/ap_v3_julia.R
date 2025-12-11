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
init_julia_ap <- function() {
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

  message("Julia initialized and ap_v3.jl loaded successfully")
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
