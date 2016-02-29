#-------------------------------------------------------------------------------
# Functions for constants calculation
#-------------------------------------------------------------------------------

################################################################################
#' Bias correction c4
#'
#' @param n Number of samples in the current batch
#' @return Numeric bias correction
#'
c4 <- function(n)
{
    return(sqrt(2/(n-1))*gamma(n/2)/gamma((n-1)/2))
}

################################################################################
#' Bias correction d2
#'
#' @param n Number of samples in the current batch
#' @return Numeric bias correction
#'
d2 <- function(n)
{
    # f (auxiliary function set with sample numerosity equal to n)
    f <- f1(n)
    d2_value <- integrate(f,lower=-Inf,upper=Inf)$value
    return(d2_value)
}

################################################################################
#' Bias correction d3
#'
#' @param n Number of samples in the current batch
#' @return Numeric bias correction
#'
d3 <- function (n)
{
    d3_ <- c(NA,0.8525025,0.8883680,0.8798082,0.8640819,0.8480397,0.8332053,0.8198311,0.8078343,0.7970507,0.7873146,0.7784783,0.7704162,0.7630231,0.7562115,0.7499082,0.7440519,0.7385910,0.7334816,0.7286865,0.7241734,0.7199149,0.7158867,0.7120681,0.7084406)
    return(d3_[n])
}

################################################################################
#' Bias correction D3
#'
#' Note: an underscore is added for portability. Some machines may not make a distinction between 'd3' and 'D3'
#'
#' @param n Number of samples in the current batch
#' @return Numeric bias correction
#'
D3_ <- function(n)
{
    D3_cost <- c(NA,0,0,0,0,0,0.076,0.136,0.184,0.223,0.256,0.283,0.307,0.328,0.347,0.363,0.378,0.391,0.403,0.415,0.425,0.434,0.443,0.451,0.459)
    return(D3_cost[n])
}

################################################################################
#' Bias correction D4
#'
#' @param n Number of samples in the current batch
#' @return Numeric bias correction
#'
D4 <- function(n)
{
    D4_cost <- c(NA,3.267,2.574,2.282,2.114,2.004,1.924,1.864,1.816,1.777,1.744,1.717,1.693,1.672,1.653,1.637,1.622,1.608,1.597,1.585,1.575,1.566,1.557,1.548,1.541)
    return(D4_cost[n])
}
