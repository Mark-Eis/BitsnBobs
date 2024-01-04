# BitsnBobs R Package
# Mark Eisler - April 2023
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# sample-size.R

# ========================================
#' Design Effect for Cluster Sampling and Cluster Randomised Trials
#'
#' @description
#' The design effect is the ratio of the total number of subjects required using cluster randomisation to the number
#' required using individual randomisation.
#'
#' @details
#' The design effect can be presented neatly in terms of the intracluster correlation and the number in a single cluster: -
#'
#' \deqn{D = 1 + (m − 1)r_{I}}{%
#'        D = 1 + (m − 1)r_{I}}
#'
#' If there is only one observation per cluster, \code{m = 1}, the design effect is 1.0 and the two designs are the same.
#' Otherwise, the larger the intracluster correlation — that is, the more important the variation between clusters is — the
#' bigger the design effect and the more subjects we will need to get the same power as a simply randomised study. Even a
#' small intracluster correlation will have an impact if the cluster size is large.
#'
#' @note \emph{Description} and \emph{Details} taken verbatim from the second reference.
#'
#' @references
#' Kerry, S.M. & Bland, J.M., 1998. Sample size in cluster randomisation. \emph{Brit Med J} \strong{316}: 5490.
#'   \href{https://doi.org/10.1136/bmj.316.7130.549}{\doi{10.1136/bmj.316.7130.549}}.
#'
#' Kerry, S.M. & Bland, J.M., 1998. The intracluster correlation coefficient in cluster randomisation. \emph{Brit Med J}
#'   \strong{316}: 1455-1460. \href{https://doi.org/10.1136/bmj.316.7142.1455}{\doi{10.1136/bmj.316.7142.1455}}.
#'
#' @family sample-size
#'
#' @param m integer representing cluster size.
#'
#' @param ri intracluster (=intraclass) correlation coefficient.
#'
#' @return Numeric value of design effect.
#'
#' @keywords cluster survey
#' @export
#' @examples
#' ## Example: x-ray guidelines study from second reference.
#' design_effect(m = 50L, ri = 0.019)
#'

design_effect <- function(m, ri)
{
    stopifnot(is.integer(m), m > 0, ri > 0, ri <= 1)
    1 + (m - 1) * ri
}


# ========================================
#' Sample Size Calculaton
#'
#' @description
#' Calculate the sample size required from a population to estimate the prevalence of a dichotomous variable to a given
#' degree of absolute precision and with a specified level of confidence.
#'
#' @details
#' For large study populations, the sample size, \var{n}, may be calculated as: -
#'
#'  \deqn{n = \displaystyle \frac{z^{2}P_{exp}(1-P_{exp})}{d^{2}}}{%
#'        n = z^{2}Pexp(1 - Pexp) / d^{2}}
#'
#'  where \eqn{P_{exp}} is the expected prevalence, \var{z} is the quantile of the normal distribution corresponding
#'  to the confidence level required and \var{d} is the desired absolute precision.
#'
#' For relatively small study populations of size \var{N}, the value of \var{n} may be adjusted thus: -
#'
#'  \deqn{n_{adj} = \displaystyle \frac{n.N}{n + N}}{%
#'        nadj = n.N / (n = N)}
#'
#' @note \emph{Sample Size Calculator} at \href{https://www.calculator.net}{www.calculator.net} has further useful
#'   \href{https://www.calculator.net/sample-size-calculator.html}{information on derivation}.
#'
#' @references
#' Thrusfield, M., Christley, R. In \emph{Veterinary Epidemiology} 4th Edn. John Wiley & Sons Ltd. 2018. (See Ch.13.)
#'   \href{https://doi.org/10.1002/9781118280249}{\doi{10.1002/9781118280249}}.
#'
#' @family sample-size
#'
#' @param Pexp numeric (or numeric vector with all values) between 0 and 1 representing the expected prevalence; default
#'   \var{0.5}.
#'
#' @param d a positive numeric (or numeric vector with all values) greater than zero representing the desired absolute
#'   precision.
#'
#' @param N either NULL for a large (theoretically infinite) population or a positive integer (or integer vector with all
#'   values) greater than zero representing the population size; default \code{NULL}.
#'
#' @param conf_level the confidence level(s) required; default \var{0.95}.
#'
#' @return A numeric (or numeric vector) giving the required sample size(s).
#'
#' @keywords survey
#' @export
#' @examples
#' ## Infinite population
#' sample_size(d = 0.05)
#' sample_size(d = 0.1)
#' 
#' ## Population = 500, 750 or 1000
#' sample_size(d = 0.05, N = c(500L, 750L, 1000L))
#' 
#' ## Expected prevalence = 0.125, 0.25, 0.50, 0.75 or 0.875
#' sample_size(Pexp = c(0.125, 0.25, 0.50, 0.75, 0.875), d = 0.05) ## Note symmetry of resulting sample sizes
#' 
#' ## Desired absolute precision = 1%, 5%, 10%, 20%
#' sample_size(d = c(0.01, 0.05, 0.1, 0.2))

sample_size <- function(Pexp = 0.5, d, N = NULL, conf_level = 0.95) {
    stopifnot(is.integer(N %||% 1L), N > 0, d > 0, Pexp > 0, Pexp <= 1, conf_level >= 0, conf_level <= 1) 
    n <- qnorm((1 + conf_level)/2) ^ 2 * Pexp * (1 - Pexp) / d ^ 2
    if (is.null(N))
        ceiling(n)
    else
        ceiling(n * N / (n + N))
}

