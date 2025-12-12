# American Put Option Pricing using Binomial Tree
# Julia implementation optimized for performance

"""
    american_put_binomial(S0, K, r, q, tt, sigma, steps)

Calculate the price of an American put option using the Cox-Ross-Rubinstein binomial tree model.

# Arguments
- `S0::Real`: Initial stock price
- `K::Real`: Strike price
- `r::Real`: Risk-free interest rate (continuously compounded)
- `q::Real`: Dividend yield (continuously compounded)
- `tt::Real`: Time to maturity in years
- `sigma::Real`: Volatility of the underlying stock
- `steps::Integer`: Number of time steps in the binomial tree

# Returns
- `Float64`: Option price at the root node
"""
function american_put_binomial(S0::Real, K::Real, r::Real, q::Real,
                                tt::Real, sigma::Real, steps::Integer)

    # Input validation
    S0 > 0 || throw(ArgumentError("Initial stock price (S0) must be positive"))
    K > 0 || throw(ArgumentError("Strike price (K) must be positive"))
    tt > 0 || throw(ArgumentError("Time to maturity (tt) must be positive"))
    sigma > 0 || throw(ArgumentError("Volatility (sigma) must be positive"))
    q >= 0 || throw(ArgumentError("Dividend yield (q) must be non-negative"))
    steps >= 1 || throw(ArgumentError("Number of steps must be at least 1"))

    # Calculate parameters
    dt = tt / steps  # time between steps
    u = exp(sigma * sqrt(dt))  # Up factor
    d = exp(-sigma * sqrt(dt))  # Down factor
    p = (exp((r - q) * dt) - d) / (u - d)  # Risk-neutral probability

    # Validate risk-neutral probability (arbitrage-free condition)
    if p < 0 || p > 1
        throw(ArgumentError(
            "Risk-neutral probability p=$p is outside [0,1]. Model parameters lead to arbitrage."
        ))
    end

    disc = exp(-r * dt)  # Discount factor

    # Pre-allocate vector to store option values
    option_values = Vector{Float64}(undef, steps + 1)

    # Backward induction from maturity to root
    for j in (steps + 1):-1:1
        num_nodes = (j == steps + 1) ? steps + 1 : j

        for i in 1:num_nodes
            # Calculate asset price at this node
            asset_price = S0 * u^(j - i) * d^(i - 1)

            if j == steps + 1
                # At maturity: option value is intrinsic value
                option_values[i] = max(K - asset_price, 0.0)
            else
                # Before maturity: calculate continuation value and check early exercise
                continuation_value = (p * option_values[i] +
                                     (1 - p) * option_values[i + 1]) * disc
                intrinsic_value = K - asset_price
                option_values[i] = max(continuation_value, intrinsic_value)
            end
        end
    end

    return option_values[1]  # Option price at the root node
end

"""
    american_put_binomial_batch(S0_vec, K_vec, r_vec, q_vec, tt_vec, sigma_vec, steps_vec)

Calculate prices for multiple American put options using multi-threading.
This is much more efficient than calling american_put_binomial in a loop.

# Arguments
All arguments should be vectors of the same length:
- `S0_vec`: Vector of initial stock prices
- `K_vec`: Vector of strike prices
- `r_vec`: Vector of risk-free interest rates
- `q_vec`: Vector of dividend yields
- `tt_vec`: Vector of times to maturity
- `sigma_vec`: Vector of volatilities
- `steps_vec`: Vector of numbers of time steps

# Returns
- `Vector{Float64}`: Vector of option prices
"""
function american_put_binomial_batch(S0_vec::AbstractVector,
                                     K_vec::AbstractVector,
                                     r_vec::AbstractVector,
                                     q_vec::AbstractVector,
                                     tt_vec::AbstractVector,
                                     sigma_vec::AbstractVector,
                                     steps_vec::AbstractVector{<:Integer})
    # Validate that all vectors have the same length
    n = length(S0_vec)
    if length(K_vec) != n || length(r_vec) != n || length(q_vec) != n ||
       length(tt_vec) != n || length(sigma_vec) != n || length(steps_vec) != n
        throw(ArgumentError("All input vectors must have the same length"))
    end

    # Pre-allocate result vector
    results = Vector{Float64}(undef, n)

    # Use multi-threading to process options in parallel
    Threads.@threads for i in 1:n
        results[i] = american_put_binomial(
            S0_vec[i],
            K_vec[i],
            r_vec[i],
            q_vec[i],
            tt_vec[i],
            sigma_vec[i],
            steps_vec[i]
        )
    end

    return results
end
