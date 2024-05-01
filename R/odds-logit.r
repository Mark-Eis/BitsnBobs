# BitsnBobs R Package
# Mark Eisler - May 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# odds-logit.R


# ========================================
#' Convert Probability to Logit
#'
#' @description
#' \code{logit()} converts a probability to the corresponding logit value.
#'
#' \code{prob_from_logit()} converts a logit value to the corresponding probability.
#'
#' @details
#' The \code{logit} is defined as the log of the odds or \eqn{\log(p / q)}{%log(p / q)}, where
#' \eqn{q = 1 - p}{%q = 1 - p}.
#'
#' The probability may be calculated from the \code{logit} by the inverse transformation: -
#'
#'  \deqn{p = \exp(\displaystyle \frac{logit}{1 + logit})}{%
#'        p = exp(logit / (1 + logit))}  
#'
#' @seealso \code{\link{exp}}, \code{\link{log}}, [`plogis`][stats::plogis] and [`qlogis`][stats::qlogis]
#'   and the \emph{Note} section of [`Logistic`][stats::Logistic] in the \pkg{\link[stats]{stats}} package.
#' @family odds-logit
#'
#' @param x a probability from which to derive the logit, or a logit from which to derive the probability.
#'
#' @return \code{logit} returns the derived logit; \code{prob_from_logit} returns the derived probability.
#'
#' @keywords math
#' @export
#' @examples
#' logit(0.5)
#' prob_from_logit(0)
#'
#' seq(from = 0, to = 1, by = 0.25) |> print_lf() |> logit() |> print_lf() |> prob_from_logit()
#'
#' (s <- seq(-10, 10, by = 2) |> print_lf() |> prob_from_logit())
#' s[1:6] + s[11:6]
#' rm(s)

logit <- function(x)
{
    if (min(x) < 0 || 1 < max(x)) 
        stop("value must be between zero and one")
	log(x / (1 - x))
}       

# ========================================
# Convert logit to probability
#' @rdname logit
#' @export
prob_from_logit <- function(x) 1 / (1 + 1/exp(x))

# ========================================
#' Convert Probability to Odds
#'
#' @description
#' \code{odds()} converts a probability to the corresponding odds.
#'
#' \code{prob_from_odds()} converts odds to the corresponding proability.
#'
#' @details
#' The \code{odds} are defined as \eqn{\log(p / q)}{%log(p / q)} where \eqn{q = 1 - p}{%q = 1 - p}.
#'
#' The probability may be calculated from the odds by the inverse transformation: -
#'
#'  \deqn{p = \displaystyle \frac{odds}{1 + odds}}{%
#'        p = odds / (1 + odds)}  
#'
#' @family odds-logit
#'
#' @param x a probability from which to derive the odds, or odds from which to derive the probability.
#'
#' @return \code{odds} returns the derived odds; \code{prob_from_odds} returns the derived probability.
#'
#' @keywords math
#' @export
#' @examples
#' odds(0.5)
#' prob_from_odds(1)
#' seq(from = 0, to = 1, by = 0.125) |> print_lf() |> odds() |> print_lf() |> prob_from_odds()

odds <- function(x)
{
    if (min(x) < 0 || 1 < max(x)) 
		stop("value must be between zero and one")
	x / (1 - x)
}

# Convert odds to probability
# ========================================
#' @rdname odds
#' @export
prob_from_odds <- function(x) ifelse(is.infinite(x), 1, x / (1 + x))
