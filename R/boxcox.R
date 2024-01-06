# BitsnBobs R Package
# Mark Eisler - Jul 2023
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# boxcox.R

# ========================================
#' 'Function Factory' for Box-Cox Transformation of Data
#'
#' Function factory to create functions that take \eqn{\lambda} as an argument for performing the Box-Cox transformation
#' on a given dataset. Taken directly from Wickham (2019)
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
#' @references
#'   Wickham, Hadley (2019) \emph{Advanced R 2nd edition}. CRC Press.
#'   \href{https://adv-r.hadley.nz/index.html}{adv-r.hadley.nz}
#'
#' @family boxcox
#'
#' @param x a `numeric vector` containing the data to be transformed.
#'
#' @return Returns a \code{\link[base]{function}} performing Box-Cox transformations on the data \var{x} for
#' any given value of \eqn{\lambda}.
#'
#' @keywords regression models
#' @export
#' @examples
#' d <- rlnorm(20)              ## Create skewed data
#' d |> print_lf() |> skew()    ## Inspect data and calculate skewness using BitsnBobs::skew()
#'
#' bc_func <- boxcox3(d)        ## Create Box-Cox function for the data
#'
#' bc_func(-1)                  ## Use this function to Box-Cox transform data with various values of lambda
#' bc_func(0)
#' bc_func(1)
#' bc_func(2)
#' identical(bc_func(0), log(d))  ## bc_func(0) same as log(d)
#'
#' seq(-3, 3, 1) |>                         ## Create a sequence from -3 to 3
#'   set_names(\(x) paste("lambda", x)) |>  ## Name the sequence vector using rlang::set_names()
#'   print_lf() |>                          ## Print with line feed
#'   map(bc_func) |>                        ## Use purrr::map() to Box-Cox transform the data using each
#'   print_lf() |>                          ## lambda value in the sequence and return a named list
#'   map_dbl(skewness) |>                   ## Use purrr::map_dbl() and skewness() to calculate skewness
#'   print_lf() |>                          ## for each element of the list and return a numeric vector
#'   abs() |>                               ## Absolute skewness
#'   which.min()                            ## Which lambda gives minimum absolute skewness?
#'
#' ## lambda 0 (usually) has least absolute skewness, unsurprisingly given these data were lognormally distributed. 
#'
#' rm(d, bc_func)

boxcox3 <- function(x) {
  function(lambda) {
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
