# BitsnBobs R Package
# Mark Eisler - Mar 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# correl-coef.R

# ========================================
#' @title
#' Phi Correlation Coefficient of Association Between Paired Binary Variables
#'
#' @description
#' The \emph{phi} correlation coefficient (or mean square contingency coefficient and denoted by \eqn{\phi} or \eqn{r\phi})
#' is a measure of association between two naturally dichotomous variables.
#'
#' @details
#' For a two-by-two contingency table \eqn{n_{11}} \eqn{n_{12}} \eqn{n_{21}} \eqn{n_{22}} the \eqn{\phi} correlation
#' coefficient is given by: -
#'
#'  \deqn{\displaystyle \phi = \frac{n_{11}n_{22} - n_{12}n_{21}}
#'    {\sqrt{(n_{11} + n_{21})(n_{12} + n_{22})(n_{11} + n_{12})(n_{21} + n_{22})}}}{%
#'    phi = (n(11).n(22) - n(12).n(21)) / \sqrt((n(11) + n(21)) * (n(12) + n(22)) * (n(11) + n(12)) * (n(21) + n(22)))}
#'
#' or equivalently, the determinant of the matrix divided by the (principal) square root of the product of its four
#' marginal sums.
#'
#' @references
#' Yule, G.U. (1912). On the Methods of Measuring Association Between Two Attributes. \emph{J Royal Stat Soc}.
#'   \strong{75} (6): 579–652. \href{https://doi.org/10.1177/10.2307/2340126}{\doi{10.2307/2340126}}.
#'
#' @seealso \code{\link[base]{matrix}}, \code{\link[stats]{mcnemar.test}}
#' @family correl_coef
#'
#' @param x a square \code{matrix} containing the observations of two binary variables as a two-by-two table of counts.
#'
#' @return A numeric between -1 and 1, the \eqn{\phi} correlation coefficient.
#'
#' @export
#' @examples
#' ## Example from Wikipedia
#' twobytwo <- matrix(c(6, 1, 2, 3), nrow = 2, dimnames = rep(list(c("Cat", "Dog")), 2) |>
#'               setNames(c("Actual", "Predicted")))
#' addmargins(twobytwo)
#'
#' phi_coef(twobytwo)
#'
#' ## Example from Statology
#' twobytwo <- matrix(c(4, 8, 9, 4), nrow = 2, dimnames =
#'               list(Gender = c("Male", "Female"), Party = c("Dem", "Rep")))
#' addmargins(twobytwo)
#'
#' phi_coef(twobytwo)
#'
#' rm(twobytwo)

phi_coef <- function(x) {
	stopifnot(is.matrix(x), identical(c(2L, 2L), dim(x)))
	det(x) / sqrt(prod(marginSums(x, 1)) * prod(marginSums(x, 2)))
}	


# ========================================
#' @title
#' Test For Association/Correlation Between Paired Binary Variables
#'
#' @description
#' This function calculates a p-value for the signifinace of a \emph{phi} correlation coefficient (or mean square
#' contingency coefficient, \eqn{\phi} or \eqn{r\phi}), the measure of association between two binary variables, and
#' calculates a confidence interval.
#'
#' @details
#' The \emph{phi} coefficient is calculated using \code{\link{phi_coef}}. For derivation of the standard error and
#' confidence interval, see Bishop \emph{et al.} (2003), and Bonett (2021). See also `ci.phi()`, the \emph{confidence 
#' interval for a phi correlation} in the
#' \href{https://cran.r-project.org/web/packages/statpsych/statpsych.pdf}{reference manual} for package
#' \CRANpkg{statpsych}.
#'
#' @references
#' Yule, G.U. (1912). On the Methods of Measuring Association Between Two Attributes. \emph{J Royal Stat Soc}.
#'   \strong{75} (6): 579–652. \href{https://doi.org/10.1177/10.2307/2340126}{\doi{10.2307/2340126}}.
#'
#' Bishop, Y.M.M., Fienberg, S.E., Holland, P.W. (1975). \emph{Discrete Multivariate Analysis.} MIT Press. (See Ch.11.)
#'   \href{https://download.e-bookshelf.de/download/0000/0020/95/L-G-0000002095-0002339867.pdf}{ISBN 978-0-387-72805-6}.
#'
#' Bonett, Douglas G. (2021). 
#'  \href{https://dgbonett.sites.ucsc.edu/statistical-methods-for-psychologists/}{Statistical Methods for Psychologists},
#'  Volume 3: Introduction to Introduction to Categorical Data Analysis. University of California, Santa Cruz. (See 3.4
#'  Measures of Association for 2 × 2 Tables.)
#'
#'
#' @seealso \code{\link[base]{matrix}}, \code{\link[stats]{mcnemar.test}}
#' @family correl_coef
#'
#' @param alternative a `character` string specifying the alternative hypothesis, must be one of `"two.sided"`
#'   (default), `"greater"` or `"less"`.  You can specify just the initial letter.
#'
#' @param conf.level numeric between 0 and 1, the confidence level required; default \var{0.95}.
#'
#' @inheritParams phi_coef
#'
#' @return A list with class \code{"htest"} containing the following components: -
#'
#' \item{statistic}{the value of the test statistic.}
#'
#' \item{parameter}{the number of (paired) observations.}
#'
#' \item{p.value}{the p-value of the test.}
#'
#' \item{conf.int}{confidence interval of the \eqn{\phi} correlation coefficient (95% or other specified level).}
#'
#' \item{estimate}{the \eqn{\phi} correlation coefficient.}
#'
#' \item{null.value}{the value of the association measure under the null hypothesis, always 0.}
#'
#' \item{alternative}{a character string describing the alternative hypothesis.}
#'
#' \item{method}{the character string "Phi correlation coefficient with confidence interval".}
#'
#' \item{data.name}{a character string giving the name of the data.}
#'
#' @keywords htest
#' @export
#' @examples
#' ## Example from Wikipedia
#' twobytwo <- matrix(c(6, 1, 2, 3), nrow = 2, dimnames = rep(list(c("Cat", "Dog")), 2) |>
#'               setNames(c("Actual", "Predicted")))
#' addmargins(twobytwo)
#'
#' phi_coef.test(twobytwo)
#' phi_coef.test(twobytwo, alternative = "less")
#' phi_coef.test(twobytwo, alternative = "greater")
#'
#' ## Example from Statology
#' twobytwo <- matrix(c(4, 8, 9, 4), nrow = 2, dimnames =
#'               list(Gender = c("Male", "Female"), Party = c("Dem", "Rep")))
#' addmargins(twobytwo)
#'
#' phi_coef.test(twobytwo)
#'
#' ## Setting confidence level to 1 - p-value gives upper bound of confidence interval close to zero
#' pval <- phi_coef.test(twobytwo)$p.value
#' phi_coef.test(twobytwo, conf.level = 1 - pval)
#' ## Similarly, with one-tailed tests setting confidence level to 1 - p-value/2 conserves the upper
#' ## or lower CI bound with alternative = "less" or alternative = "greater" respectively
#' phi_coef.test(twobytwo, alternative = "less", conf.level = 1 - pval/2)
#' phi_coef.test(twobytwo, alternative = "greater", conf.level = 1 - pval/2)
#'
#' ## Example from statpsych::ci.phi(), which should return: -
#' ##       Estimate         SE         LL        UL
#' ## [1,] 0.1229976 0.05746271 0.01037273 0.2356224
#'
#' twobytwo <- matrix(c(229, 28, 96, 24), nrow = 2, dimnames = rep(list(c("Zero", "One")), 2))
#' addmargins(twobytwo)
#'
#' phi_coef.test(twobytwo)
#' ## Check standard error as expected
#' with(phi_coef.test(twobytwo), c(stderr = estimate/statistic))
#'
#' rm(twobytwo, pval)

phi_coef.test <- function(x, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {
    alternative <- match.arg(alternative)
    stopifnot(conf.level <= 1, conf.level > 0)
    phi <- phi_coef(x)

    n <- sum(x)
    diffs <- map_int(list(x, t(x)), \(y) c(1, -1) %*% y %*% c(1, 1)) # differences betweeen row and col marginal sums
    prods <- map_int(1:2, \(y) prod(apply(x, y, sum)))               # products of row and col marginal sums

    v <- vector("numeric", 4)
    v[1] <- 1 - phi^2 
    v[2] <- phi + 0.5 * phi^3
    v[3] <- diffs[1] * diffs[2] / sqrt(prod(prods))
    v[4] <- (0.75 * phi^2) * (diffs[1]^2 / prods[1] + diffs[2]^ 2 / prods[2])

    se <- sqrt((v[1] + v[2] * v[3] + v[4]) / n)
    z <- phi / se
    PVAL <- switch(alternative,
        less = pnorm(z),
        greater = pnorm(z, lower.tail = FALSE),
        two.sided = 2 * pnorm(-abs(z))
    )
    CINT <- switch(alternative,
        less = c(-1, phi + se * qnorm(conf.level)),
        greater = c(phi - se * qnorm(conf.level), 1),
        two.sided = phi + c(-1, 1) * se * qnorm((1 + conf.level)/2)
    )
    attr(CINT, "conf.level") <- conf.level
    STATISTIC <- c(z = z)
    PARAMETER <- c(n = n)
    ESTIMATE <- c(phi = phi)
    NVAL <- c(phi = 0)

    structure(
        list(
            statistic = STATISTIC, 
            parameter = PARAMETER,
            p.value = PVAL,
            conf.int = CINT,
            estimate = ESTIMATE,
            null.value = NVAL,
            alternative = alternative,
            method = "Phi correlation coefficient with confidence interval",
            data.name = deparse(substitute(x))
        ),
        class = "htest"
    )
}


# ========================================
#' Confidence Interval and t-Test from Pearson's Correlation Coefficient
#'
#' @description
#' This function performs a \emph{t}-test for a given Pearson's product-moment correlation coefficient \var{r} derived
#' from a sample of size \var{n} from a bivariate normal population, and calculates the population confidence interval.
#'
#' @details
#' The t-statistic is given by the correlation coefficient \var{r} divided by its standard error calculated using: -
#'
#'   \deqn{SE = \displaystyle \sqrt{\frac{1 - r^2}{df}}}{%
#'     SE = sqrt((1 - r <sup>2<\sup>) / df)}
#'
#' To calculate the confidence interval, firstly a value \eqn{Z_r} is calculated from \var{r}
#' using the Fisher transformation (inverse hyperbolic tangent): -
#'
#'   \deqn{Z_r = \displaystyle \frac{1}{2}log_e\left(\frac{1 + r}{1 - r}\right)}{%
#'     Z<sub>r<\sub> = (log<sub>e<\sub>((1 + r)/(1 - r))) / 2}
#' 
#' Log upper and lower bounds (\var{L} and \var{U}) are then calculated using: -
#' 
#'   \deqn{L = \displaystyle Z_r - \frac{Z_{(1 - alpha/2)}}{\sqrt{n - 3}}}{%
#'     \emph{L} = Z<sub>r<\sub> - Z<sub>(1 - alpha/2)<\sub>/sqrt(n - 3)}
#' 
#'   \deqn{U = \displaystyle Z_r + \frac{Z_{(1 - alpha/2)}}{\sqrt{n - 3}}}{%
#'     \emph{U} = Z<sub>r<\sub> + Z<sub>(1 - alpha/2)<\sub>/sqrt(n - 3)}
#' 
#' The confidence interval is then calculated using the hyperbolic tangent: -
#' 
#'  \deqn{\displaystyle lower = \frac{e^{2L} - 1}{e^{2L} + 1}, upper = \frac{e^{2U} - 1}{e^{2U} + 1}}{%
#'	  \emph{lower} = (e<sup>2L<\sup> - 1)/(e<sup>2L<\sup> + 1), \emph{upper} = (e<sup>2U<\sup> - 1)/(e<sup>2U<\sup> + 1)}
#'
#' @note This function is much like \code{cor.test} and potentially useful if the correlation coefficient is available
#'   but not the original data.
#'
#' @seealso \code{\link[stats]{cor}}, \code{\link[stats]{cor.test}}, \code{\link[stats]{t.test}}
#' @family correl_coef
#'
#' @param r numeric providing the value of Pearson's product-moment correlation coefficient \var{r}.
#'
#' @param n integer providing the number of pairs of observations from which \var{r} was derived; minimum 4.
#'
#' @inheritParams phi_coef.test
#'
#' @inheritParams stats::cor.test
#'
#' @return A list with class \code{"htest"} containing the following components: -
#'
#' \item{statistic}{the value of the test statistic.}
#'
#' \item{parameter}{the degrees of freedom for the test statistic.}
#'
#' \item{p.value}{the p-value of the test.}
#'
#' \item{conf.int}{confidence interval of the correlation coefficient (95% or other specified level).}
#'
#' \item{estimate}{the correlation coefficient as provided in \var{r}.}
#'
#' \item{null.value}{the value of the association measure under the null hypothesis, always 0.}
#'
#' \item{alternative}{a character string describing the alternative hypothesis.}
#'
#' \item{method}{the character string "Pearson's product-moment correlation".}
#'
#' \item{data.name}{a character string giving the name of the data.}
#'
#' @keywords htest
#' @export
#' @examples
#' ## Data from cor.test() example (Hollander & Wolfe, 1973): -
#'  x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#'  y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#'
#'  (corxy <- cor(x, y))
#'
#'  cor_coef.test(corxy, 9)  ## alternative two-sided by default
#'  cor.test(x, y)  ## Result should be identical
#'
#'  cor_coef.test(corxy, 9, alternative = "less")
#'  cor.test(x, y, alternative = "less")  ## Result should be identical
#'
#'  cor_coef.test(corxy, 9, alternative = "greater")
#'  cor.test(x, y, alternative = "greater")  ## Result should be identical
#'
#'  rm(corxy, x, y)

cor_coef.test <- function(r, n, alternative = c("two.sided", "less", "greater"), conf.level = 0.95) {
  alternative <- match.arg(alternative)
  stopifnot(conf.level <= 1, conf.level > 0, n > 3)
  df <- n - 2L
  ESTIMATE <- c(cor = r)
  PARAMETER <- c(df = df)
  STATISTIC <- c(t = sqrt(df) * r/sqrt(1 - r^2))
  z <- atanh(r)
  sigma <- 1/sqrt(n - 3)
  cint <- switch(alternative,
    less = c(-Inf, z + sigma * qnorm(conf.level)),
    greater = c(z - sigma * qnorm(conf.level), Inf),
    two.sided = z + c(-1, 1) * sigma * qnorm((1 + conf.level)/2)
  )
  CINT <- tanh(cint)
  attr(CINT, "conf.level") <- conf.level
  PVAL <- switch(alternative,
    less = pt(STATISTIC, df), 
    greater = pt(STATISTIC, df, lower.tail = FALSE), 
    two.sided = 2 * min(pt(STATISTIC, df), pt(STATISTIC, df, lower.tail = FALSE))
  )
  NVAL <- c(correlation = 0)

  structure(
    list(
      statistic = STATISTIC,
      parameter = PARAMETER,
      p.value = PVAL,
      conf.int = CINT,
      estimate = ESTIMATE,
      null.value = NVAL,
      alternative = alternative,
      method = "Pearson's product-moment correlation",
      data.name = deparse(substitute(r))
    ),
    class = "htest"
  )
}
