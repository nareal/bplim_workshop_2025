# apopt: American Put Option Pricing

Fast implementation of American put option pricing using the Cox-Ross-Rubinstein binomial tree model with Rcpp.

## Installation

From within this project:

```r
# Install from local source
install.packages("apopt", repos = NULL, type = "source")

# Or using devtools
devtools::install("apopt")
```

## Usage

```r
library(apopt)

# Price an American put option
ap_v3_rcpp(
  S0 = 40,      # Initial stock price
  K = 40,       # Strike price
  r = 0.08,     # Risk-free rate
  q = 0,        # Dividend yield
  tt = 0.25,    # Time to maturity (years)
  sigma = 0.30, # Volatility
  steps = 1000  # Number of time steps
)
```

## Features

- **Fast C++ implementation** using Rcpp for optimal performance
- **Input validation** ensures all parameters are valid before computation
- **Arbitrage-free pricing** validates risk-neutral probability is in [0,1]
- **Parallel-safe** works seamlessly with `furrr` and other parallel packages

## Performance

The Rcpp implementation provides significant speedups:
- ~10-50x faster than pure R implementations
- Works efficiently in parallel without compilation overhead
- Ideal for pricing large portfolios of options

## Development

Package created for the BPLIM 2025 Workshop on "Speeding Up Empirical Research in R".
