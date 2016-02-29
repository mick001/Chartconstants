#-------------------------------------------------------------------------------
# Auxiliary functions
#-------------------------------------------------------------------------------

################################################################################
#' Auxiliary function f1
#'
#' @param n Number of samples in the current batch
#' @return Function to be integrated in the d2 function
#'

f1 <- function(n)
{
    f <- function(z){1-(1-pnorm(z))^n-pnorm(z)^n}
    return(f)
}

################################################################################
#' Check n
#'
#' This function checks if the parameter n of the main function is within the admissible range
#'
#' @param n the parameter n given to the main function constant. Can be an integer of a vector of integers
#' @return Void
#'
check_n <- function(n){

    if ( any(n > 25) | any(n <= 1) | any(n %% 1 != 0) )
    {
        stop('Value of n is outside the admissible range or is not an integer:\nAdmissible range:\n', toString(c(2:25)), call.= T)
    }
}

################################################################################
#' Available constants
#'
#' This function prints out available constants
#'
#' @return Void
#' @export
#'
available_constants <- function()
{
    cat('Available constants:', c('c4','d2','d3','D3','D4'))
}
