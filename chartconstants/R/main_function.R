#-------------------------------------------------------------------------------
# Main function
#-------------------------------------------------------------------------------

################################################################################
#' Constants for Shewhart quality control charts
#'
#' Calculates different constants using a single function
#'
#' @param n Numerosity of sample, must be an integer (numeric) within the [2,25] range.
#' @param cname Name of the function to be used, can also be of character class. Admissible values are A2, A3, B3, B4, B5, B6, c4, d2, d3, D1, D2, D3 and D4.
#' @return Unbiasing constant (numeric).
#' @export
#' @examples
#' constant(2, D3)
#' constant(2,'d2')
#' constant(2:10, D4)
#'
constant <- function(n, cname = c('','A2','A3','B3','B4','B5','B6','c4','d2','d3','D1','D2','D3','D4'))
{
    check_n(n)

    cname <- as.character(substitute(cname))

    cname <- match.arg(cname)

    switch(cname,
           A2 = A2(n),
           A3 = A3(n),
           B3 = B3(n),
           B4 = B4(n),
           B5 = B5(n),
           B6 = B6(n),
           c4 = c4(n),
           d2 = d2_(n),
           d3 = d3(n),
           D1 = D1(n),
           D2 = D2(n),
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
# #' @param cname Name of the function to be used, can also be of character class. Admissible values are A2, A3, B3, B4, B5, B6, c4, d2, d3, D1, D2, D3 and D4.
# #'      if no cname parameter is provided, NULL is returned.
# #' @return Unbiasing constant (numeric).
# #' @export
# #' @examples
# #' constant(2,'c4')
# #' constant(2,'d2')
# #'
# constant <- function(n, cname = c('','A2','A3','B3','B4','B5','B6','c4','d2','d3','D1','D2','D3','D4'))
# {
#     check_n(n)
#
#     cname <- match.arg(cname)
#
# switch(cname,
#        A2 = A2(n),
#        A3 = A3(n),
#        B3 = B3(n),
#        B4 = B4(n),
#        B5 = B5(n),
#        B6 = B6(n),
#        c4 = c4(n),
#        d2 = d2(n),
#        d3 = d3(n),
#        D1 = D1(n),
#        D2 = D2(n),
#        D3 = D3_(n),
#        D4 = D4(n))
# }
