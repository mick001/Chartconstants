#-------------------------------------------------------------------------------
# Functions for constants calculation
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# A xbar-r chart (mu and sigma given)
################################################################################
#' A constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
A <- function(n)
{
    return( 3 / sqrt(n) )
}

#-------------------------------------------------------------------------------
# A2 xbar-r chart
################################################################################
#' A2 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
A2 <- function(n)
{
    return( 3 / (d2_(n) * sqrt(n)) )
}

#-------------------------------------------------------------------------------
# A3 xbar-s chart
################################################################################
#' A3 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
A3 <- function(n)
{
    return( 3 / (c4(n) * sqrt(n)) )
}

#-------------------------------------------------------------------------------
# s chart lcl unknown sigma
################################################################################
#' B3 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
B3 <- function(n)
{
    return( 1 - 3 * sqrt( 1 - c4(n)^2 ) / c4(n) )
}

#-------------------------------------------------------------------------------
# s chart lcl unknown sigma
################################################################################
#' B4 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
B4 <- function(n)
{
    return( 1 + 3 * sqrt( 1 - c4(n)^2 ) / c4(n) )
}

#-------------------------------------------------------------------------------
# s chart lcl given sigma
################################################################################
#' B5 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
B5 <- function(n)
{
    return( c4(n) - 3 * sqrt( 1 - c4(n)^2 ) )
}

#-------------------------------------------------------------------------------
# s chart ucl given sigma
################################################################################
#' B6 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
B6 <- function(n)
{
    return( c4(n) + 3 * sqrt( 1 - c4(n)^2 ) )
}

#-------------------------------------------------------------------------------
# s chart
################################################################################
#' Bias correction c4 for estimating sigma
#'
#' @param n Number of samples in the current batch
#' @return Numeric bias correction
#'
c4 <- function(n)
{
    return(sqrt(2/(n-1))*gamma(n/2)/gamma((n-1)/2))
}

################################################################################
#' d2 constant
#'
#' d2 is the mean of the distribution of the relative range.
#'
#' @param n Number of samples in the current batch
#' @return Numeric
#'
d2_ <- function(n)
{
    # Constant vector
    cv <- c(n)
    # Initialize vector to be returned
    d2_value <- numeric()

    # Calculate the constants
    for(i in cv)
    {
        # f (auxiliary function set with sample numerosity equal to the group numerosity)
        f <- f1(i)
        d2_ <- integrate(f, lower = -Inf, upper = Inf)$value
        d2_value <- append(d2_value, d2_)
    }

    # Return
    return(d2_value)
}

################################################################################
#' d3 constant
#'
#' d3 is the standard deviation of the distribution of the relative range.
#'
#' @param n Number of samples in the current batch
#' @return Numeric
#'
d3 <- function (n)
{
    d3_ <- c(NA, 0.8525025, 0.8883680, 0.8798082, 0.8640819, 0.8480397, 0.8332053,
             0.8198311, 0.8078343, 0.7970507, 0.7873146, 0.7784783, 0.7704162,
             0.7630231, 0.7562115, 0.7499082, 0.7440519, 0.7385910, 0.7334816,
             0.7286865, 0.7241734, 0.7199149, 0.7158867, 0.7120681, 0.7084406)
    return(d3_[n])
}

#-------------------------------------------------------------------------------
# D1 ucl R chart with given sigma
################################################################################
#' D1 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
D1 <- function(n)
{
    D1_cost <- c(NA, 0, 0, 0, 0, 0,
                 0.205, 0.388, 0.547, 0.686, 0.811,
                 0.924, 1.026, 1.118, 1.204, 1.282,
                 1.356, 1.424, 1.489, 1.549, 1.606,
                 1.660, 1.711, 1.759, 1.805)
    return(D1_cost[n])
}

#-------------------------------------------------------------------------------
# D2 lcl R chart with given sigma
################################################################################
#' D2 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
D2 <- function(n)
{
    return( d2_(n) + 3 * d3(n) )
}

#-------------------------------------------------------------------------------
# D3 lcl R chart D3 = 1-d3/d2
################################################################################
#' D3 constant
#'
#' Note: an underscore is added for portability. Some machines may not make a distinction between 'd3' and 'D3'
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
D3_ <- function(n)
{
    D3_cost <- c(NA, 0, 0, 0, 0, 0,
                 0.076, 0.136, 0.184, 0.223, 0.256,
                 0.283, 0.307, 0.328, 0.347, 0.363,
                 0.378, 0.391, 0.403, 0.415, 0.425,
                 0.434, 0.443, 0.451, 0.459)
    return(D3_cost[n])
}

#-------------------------------------------------------------------------------
# D4 ucl R chart D4 = 1+d3/d2
################################################################################
#' D4 constant
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
D4 <- function(n)
{
    D4_cost <- c(NA, 3.267, 2.574, 2.282, 2.114, 2.004, 1.924,
                 1.864, 1.816, 1.777, 1.744, 1.717, 1.693,
                 1.672, 1.653, 1.637, 1.622, 1.608, 1.597,
                 1.585, 1.575, 1.566, 1.557, 1.548, 1.541)
    return(D4_cost[n])
}


#-------------------------------------------------------------------------------
# One
################################################################################
#' One
#'
#' This function simply returns 1 or a vector of length n with all values set to 1.
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
one <- function(n)
{
    return( rep(1, length(n)) )
}

#-------------------------------------------------------------------------------
# Numeric constant for LCL and UCL calculation in the I chart
################################################################################
#' i_chart_const
#'
#' This function returns 3/d2(n).
#'
#' @param n Number of samples in the current batch
#' @return Numeric constant
#'
i_chart_const <- function(n)
{
    return( 3 / d2_(n) )
}
