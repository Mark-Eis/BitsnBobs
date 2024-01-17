# BitsnBobs R Package
# Mark Eisler - Jan 2024
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
#' f <- factor(2001:2020) |> print()
#'
#' ## Seemingly numeric levels are actually character strings!
#' levels(f) |> str()
#'
#' ## Return codes for factor levels, not what was expected
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

