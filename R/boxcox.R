# BitsnBobs R Package
# Mark Eisler - Apr 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# boxcox.R

# ========================================
#' 'Function Factory' for Box-Cox Transformation of Data
#'
#' Function factory to create functions that take \eqn{\lambda} as an argument for performing the Box-Cox transformation
#' on a given dataset. Adapted from Wickham (2019)
#' \href{https://adv-r.hadley.nz/function-factories.html#exercises-34}{Section 10.4.4 Exercises}.
#'
#' A numeric vector containing the data to be transformed is provided as an argument to this 'function factory',
#' which returns a function performing the Box-Cox transformation on those data for any given value of \eqn{\lambda}.
#' The Box-Cox transformation takes the following form: -
#'
#' if \eqn{\lambda \ne 0}
#' \deqn{y(\lambda) = \displaystyle \frac{y^\lambda - 1}{\lambda}}{%
#'       y(&lambda;) = (y<sup>&lambda;</sup> – 1) / &lambda;}
#'
#' if \eqn{\lambda = 0}
#'  \deqn{y(\lambda) = \log(y)}{%
#'        y(&lambda;) = log(y)}
#'
#'
#' If `labile_data` is `TRUE`, `data` are represented in the `boxcox3()` function environment as a
#' [`quosure`][rlang::topic-quosure], and functions returned by will automatically refer to the current version of
#' `data` in its original [`environment`][base::environment], usually the calling environment i.e., typically but not
#' necessarily the global environment. If `labile_data` is `FALSE`, returned functions refer to a copy of `data`
#' saved in the function environment at the time of execution of `boxcox3()`, and will not reflect any subsequent
#' changes to the original `data`. 
#'
#' @references
#'   Wickham, Hadley (2019) \emph{Advanced R 2nd edition}. CRC Press.
#'   \href{https://adv-r.hadley.nz/index.html}{adv-r.hadley.nz}
#'
#' @family boxcox
#'
#' @param x a `numeric vector` containing the data to be transformed.
#'
#' @inheritParams retriever
#'
#' @return Returns a \code{\link[base]{function}} taking a single argument \eqn{\lambda} that performs the Box-Cox
#'   transformation on data \var{x}.
#'
#' @keywords regression models
#' @export
#' @examples
#' ## Create skewed data
#' (d <- rlnorm(20))
#' ## Calculate skewness using BitsnBobs::skew()
#' d |> skew()
#' ## Box-Cox function for these data
#' bc_func <- boxcox3(d)
#'
#' ## Box-Cox transform data with various values of lambda
#' bc_func(-1)
#' bc_func(0)
#' bc_func(1)
#' bc_func(2)
#' ## bc_func(0) same as log(d)
#' identical(bc_func(0), log(d))
#'
#' seq(-3, 3, 1) |>                         ## Create a sequence from -3 to 3
#'   set_names(\(x) paste("lambda", x)) |>  ## Name sequence vector using rlang::set_names()
#'   print_lf() |>                          ## Print with line feed
# #'   map(bc_func) |>                        ## Box-Cox transform data using each lambda value
#'   lapply(bc_func) |>                     ## Box-Cox transform data using each lambda value
#'   print_lf() |>                          ##   in sequence and print the named list
#'   map_dbl(skewness) |>                   ## Calculate skewness for each element of the list
#'   print_lf() |>                          ##   and print the numeric vector
#'   abs() |>                               ## Absolute skewness...
#'   which.min()                            ##   ...which lambda gives minimum?
#'
#' ## Usually, lambda 0 has least absolute skewness as data were sampled from lognormal distribution
#'
#' rm(d, bc_func)

boxcox3 <- function(x, labile_data = TRUE) {
    x <- {
        if (labile_data) enquo(x) else force(x)
    }

    function(lambda) {
        if (labile_data) x <- eval_tidy(x)
        if (lambda == 0) {
            log(x)
        } else {
            (x ^ lambda - 1) / lambda
        }
    }  
}


# ========================================
#' Optimise the Value of Lambda for a Box-Cox Transformation
#'
#' Optimise the value of \eqn{\lambda} for a Box-Cox transformation by minimising skewness and return the optimally
#' transformed data together with the optimal \eqn{\lambda} and corresponding skewness minimum as attributes.
#'
#' Uses the functions \code{\link{boxcox3}} to create a Box-Cox transformation function for the given dataset,
#' \code{\link{skewness}} to calculate the skewness of the transformed data and \code{\link[stats]{optimise}}
#' to search for the value of \eqn{\lambda} that minimises skewness.
#'
#' @seealso \code{\link{skewness}}
#' @family boxcox
#'
#' @param x a \code{numeric} vector of data to be transformed using optimised values.
#' @inheritParams stats::optimise
#'
#' @return A numeric vector the same length as \var{x} with \code{\link[base]{attributes}} \var{lambda} and
#'   \var{skew} representing the optimal value of \eqn{\lambda} and corresponding skewness minimum respectively.
#'
#' @keywords regression models optimize
#' @export
#' @examples
#' runif(20) |> opt_bc()
#' rlnorm(20) |> opt_bc()
#' rpois(100, 3) |> opt_bc()         ## Provokes warning
#' rpois(100, 3) |> opt_bc(c(-3, 5)) ## Redefining interval avoids warning

opt_bc <- function(x, interval = c(-5, 5), tol = 1e-10) {
  bcf <- boxcox3(x)
  opt <- optimise(function(y) (abs(skewness(bcf(y)))), interval = interval, tol = tol)
    structure(
      bcf(opt$minimum),
      skew = unname(opt$objective),
      lambda = opt$minimum
    )
}
