# Load required package
if (!require("torch", quietly = TRUE)) {
  stop(
    "torch package is required. Install it with: install.packages('torch')"
  )
}

#' Price multiple American put options using a batched binomial tree on a GPU.
#'
#' This function extends the scalar CRR binomial tree implementation so that many
#' put option contracts can be priced simultaneously. The binomial time recursion
#' remains sequential, but each level operates on the entire batch, allowing the
#' GPU to process wide workloads efficiently. All contracts share the same
#' number of time steps \code{steps}; other parameters may vary by contract.
#'
#' @param S0 Numeric vector of initial stock prices.
#' @param K Numeric vector of strike prices.
#' @param r Numeric vector of continuously compounded risk-free rates.
#' @param q Numeric vector of dividend yields compounded continuously.
#' @param sigma Numeric vector of volatilities.
#' @param tt Numeric vector of times to maturity (years).
#' @param steps Integer scalar with the number of binomial steps (shared by batch).
#'
#' @return Numeric vector of American put option prices, one per contract.
ap_v3_torch <- function(S0, K, r, q, tt, sigma, steps) {
  # --- 1. Device and dtype selection ----------------------------------------------------------
  if (torch::cuda_is_available()) {
    device <- "cuda"
    float_dtype <- torch_double()
    message("CUDA device detected. Using torch_double() (float64).")
  } else if (torch::backends_mps_is_available()) {
    device <- "mps"
    float_dtype <- torch_float()
    message("MPS device detected. Using torch_float() (float32).")
  } else {
    stop("No compatible GPU (CUDA or MPS) available.")
  }

  # --- 2. Input validation and broadcasting ---------------------------------------------------
  if (length(steps) != 1) {
    stop("`steps` must be a single integer shared by the batch.")
  }
  steps <- as.integer(steps)
  if (steps <= 0) {
    stop("`steps` must be a positive integer.")
  }

  lengths <- c(
    length(S0),
    length(K),
    length(r),
    length(q),
    length(sigma),
    length(tt)
  )
  if (any(lengths == 0)) {
    stop("All parameters must have at least one value.")
  }
  batch <- max(lengths)

  broadcast_param <- function(x, name) {
    if (length(x) == 1) {
      rep(x, batch)
    } else if (length(x) == batch) {
      x
    } else {
      stop(sprintf(
        "`%s` must have length 1 or match the batch size (%d).",
        name,
        batch
      ))
    }
  }

  S0_vec <- as.numeric(broadcast_param(S0, "S0"))
  K_vec <- as.numeric(broadcast_param(K, "K"))
  r_vec <- as.numeric(broadcast_param(r, "r"))
  q_vec <- as.numeric(broadcast_param(q, "q"))
  sigma_vec <- as.numeric(broadcast_param(sigma, "sigma"))
  tt_vec <- as.numeric(broadcast_param(tt, "tt"))

  dt_vec <- tt_vec / steps
  if (any(dt_vec <= 0)) {
    stop("All maturities must be positive.")
  }

  u_vec <- exp(sigma_vec * sqrt(dt_vec))
  d_vec <- 1 / u_vec
  denom <- u_vec - d_vec
  if (any(abs(denom) < .Machine$double.eps)) {
    stop("Encountered zero denominator in probability computation.")
  }
  p_vec <- (exp((r_vec - q_vec) * dt_vec) - d_vec) / denom
  if (any(p_vec < 0 | p_vec > 1)) {
    stop("Risk-neutral probability out of bounds; check inputs.")
  }
  down_prob_vec <- 1 - p_vec
  df_vec <- exp(-r_vec * dt_vec)

  # --- 3. Promote scalars to batched GPU tensors ----------------------------------------------
  S0_t <- torch_tensor(S0_vec, device = device, dtype = float_dtype)$view(c(
    batch,
    1,
    1
  ))
  K_t <- torch_tensor(K_vec, device = device, dtype = float_dtype)$view(c(
    batch,
    1,
    1
  ))
  u_t <- torch_tensor(u_vec, device = device, dtype = float_dtype)$view(c(
    batch,
    1,
    1
  ))
  d_t <- torch_tensor(d_vec, device = device, dtype = float_dtype)$view(c(
    batch,
    1,
    1
  ))
  p_line <- torch_tensor(p_vec, device = device, dtype = float_dtype)$view(c(
    batch,
    1,
    1
  ))
  down_prob_line <- torch_tensor(
    down_prob_vec,
    device = device,
    dtype = float_dtype
  )$view(c(batch, 1, 1))
  df_line <- torch_tensor(df_vec, device = device, dtype = float_dtype)$view(c(
    batch,
    1,
    1
  ))
  zero_scalar <- torch_tensor(0, device = device, dtype = float_dtype)

  # --- 4. Precompute stock prices and exercise values (batched) -------------------------------
  j_seq <- torch_tensor(0:steps, device = device, dtype = float_dtype)
  j_indices <- j_seq$view(c(1, steps + 1, 1))
  n_seq <- torch_tensor(0:steps, device = device, dtype = float_dtype)
  n_indices <- n_seq$view(c(1, 1, steps + 1))

  S_all <- S0_t * (u_t$pow(j_indices)) * (d_t$pow(n_indices - j_indices))

  # Compute put payoffs: max(K - S, 0)
  ex_all <- torch_maximum(K_t - S_all, zero_scalar)

  # --- 5. Backward induction across the batch -------------------------------------------------
  V <- ex_all$narrow(3, steps + 1, 1)

  for (n in (steps - 1):0) {
    n_nodes <- n + 1

    V_up <- V$narrow(2, 2, n_nodes)
    V_down <- V$narrow(2, 1, n_nodes)
    V_hold <- df_line * (p_line * V_up + down_prob_line * V_down)

    V_ex <- ex_all$narrow(2, 1, n_nodes)$narrow(3, n + 1, 1)
    V <- torch_maximum(V_hold, V_ex)
  }

  # --- 6. Return one price per contract -------------------------------------------------------
  as.numeric(V$squeeze())
}
