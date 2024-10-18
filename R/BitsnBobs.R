# BitsnBobs R Package
# Mark Eisler - Oct 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# BitsnBobs.R


# ========================================
#' Factor As Numeric
#'
#' Transform a factor to approximately its original numeric values.
#'
#' See \sQuote{Warning} section of [`factor`][base::factor]: \enc{–}{-}
#'
#' \dQuote{In particular, `as.numeric` applied to a factor is meaningless, and may happen by implicit
#' coercion. To transform a factor `f` to approximately its original numeric values,
#' `as.numeric(levels(f))[f]` is recommended and slightly more efficient than
#' `as.numeric(as.character(f))`.}
#'
#' Accordingly, `fct_to_num()` implements this method.
#'
#' @seealso  [`factor`][base::factor]
#'
#' @param f factor to be converted to numeric values
#'
#' @return Numeric
#'
#' @keywords category math
#' @export
#' @examples
#' ## Create sequence of numeric values as a factor
#' (f <- factor(2001:2020))
#'
#' ## Seemingly numeric levels are actually character strings!
#' levels(f) |> str()
#'
#' ## Returns codes for factor levels, not what was expected
#' f |> as.numeric()
#'
#' ## Returns numeric values equivalent to factor levels
#' f |> fct_to_num()
#'
#' rm(f)

fct_to_num <- function(f) as.numeric(levels(f))[f]


# ========================================
#' @title
#' Stars For Statistical Significance
#'
#' @description
#' Stars for statistical significance with levels as usual in R. A vectorised function.
#' 
#' @param p A numeric vector of probabilities.
#' 
#' @return A character vector, length of `p`.
#' 
#' @export
#' @examples
#' (test_seq <- round(10 ^ seq(-4, 0, 0.5), 4))
#' 
#' starsig(test_seq)
#' 
#' rbind(test_seq, as.character(starsig(test_seq)))
#' 
#' data.frame(val = test_seq, sig = starsig(test_seq))
#' 
#' rm(test_seq)

starsig <- function(p) {
    if (!is.numeric(p))
        stop("p must be numeric") 

    cut(p, c(0, 0.001, 0.01, 0.05, 0.1, 1), include.lowest = T, right = F) |>
    `levels<-`(c("***", "**", "*", ".", "NS"))
}


# ========================================
#' @title
#' Vectorised conditional sign change
#'
#' @description
#' Vectorised function that changes sign of elements of a numeric vector, dependent on values of a
#' logical argument of the same length.
#'
#' @param x A numeric vector.
#' 
#' @param negate A logical vector of the same length as `x`.
#' 
#' @return A numeric vector, length of `x`.
#' 
#' @keywords category math
#' @export
#' @examples
#' 
#' swapsign(1:10, rep(c(FALSE, TRUE), 5))
#' 
#' swapsign(1:10, rep(c(1, 0), 5))
#' 


swapsign <- function(x, negate) {
    stopifnot(length(x) == length(negate))
    stopifnot(is.logical(negate))
    ifelse(negate, -x, x)
}
