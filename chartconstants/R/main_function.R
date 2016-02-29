#-------------------------------------------------------------------------------
# Main function
#-------------------------------------------------------------------------------

################################################################################
#' Constants for Shewhart quality control charts
#'
#' Calculates different constants using a single function
#'
#' @param n Numerosity of sample, must be an integer (numeric) within the [2,25] range.
#' @param cname Name of the function to be used, can also be of character class. Admissible values are c4, d2, d3, D3 and D4.
#' @return Unbiasing constant (numeric).
#' @export
#' @examples
#' constant(2, D3)
#' constant(2,'d2')
#' constant(2:10, d2)
#'
constant <- function(n, cname = c('','c4','d2','d3','D3','D4'))
{
    check_n(n)

    cname <- as.character(substitute(cname))

    cname <- match.arg(cname)

    switch(cname,
           c4 = c4(n),
           d2 = d2(n),
           d3 = d3(n),
           D3 = D3_(n),
           D4 = D4(n))
}

# ################################################################################
# #' OLD MAIN FUNCTION. REPLACED BY NSE version
# #'
# #' Constants for Shewhart quality control charts
# #'
# #' Calculates different constants using a single function
# #'
# #' @param n Numerosity of sample, must be an integer (numeric) within the [2,25] range.
# #' @param cname Name of the function to be used, must be of character class. Admissible values are 'c4','d2','d3','D3' and 'D4'.
# #'      if no cname parameter is provided, NULL is returned.
# #' @return Unbiasing constant (numeric).
# #' @export
# #' @examples
# #' constant(2,'c4')
# #' constant(2,'d2')
# #'
# constant <- function(n, cname = c('','c4','d2','d3','D3','D4'))
# {
#     check_n(n)
#
#     cname <- match.arg(cname)
#
#     switch(cname,
#            c4 = c4(n),
#            d2 = d2(n),
#            d3 = d3(n),
#            D3 = D3_(n),
#            D4 = D4(n))
# }
