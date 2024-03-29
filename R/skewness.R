# BitsnBobs R Package
# Mark Eisler - Nov 2023
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# skewness.R

# ========================================
#' Skewness
#'
#' @aliases skew
#'
#' @description
#' Computes the skewness, \eqn{\gamma_{1}}{&gamma}, of the values in \var{x} with optional adjustment to give
#' \eqn{G_{1}}{G<sub>1</sub>}, the expected populaton value of skewness from a sample.
#'
#' @details
#' Moments for samples of size \var{n} are given by: -
#'
#'   \deqn{m_{r} = \displaystyle \frac{\sum \left(x - \overline{x} \right)^{r}}{n}}{%
#'     m<sub>r</sub> = &sum;(x - x)<sup>r</sup>/n}
#'
#' The skewness (or skew) of a numeric variable \eqn{\gamma_{1}}{&gamma;<sub>1</sub>} is the third moment about the
#' mean rendered dimensionless by dividing by the cube of the square root of the second moment: -
#'
#'   \deqn{\gamma_{1} = \displaystyle \frac{m_3}{{\sqrt{m_{2}}}^3}}{%
#'     &gamma;<sub>1</sub> = m<sub>3</sub> / (m<sub>2</sub>)<sup>3/2</sup>}
#'
#' The expected population value of skewness \eqn{G_{1}}{G<sub>1</sub>} from a sample is obtained using: -
#'
#'   \deqn{G_{1} = \displaystyle \frac{\sqrt{n(n - 1)}}{n-2}\gamma_{1}}{%
#'     G<sub>1</sub> = (&radic;(n(n-1))/(n-2))&gamma;<sub>1</sub>}
#'
#' [`skew`][skew] is an alias for \code{skewness()}.
#'
#' (Adapted from Crawley, 2012, and Joanes and Gill, 1998.)
#'
#' @references
#'   Crawley, Michael J. (2012) \emph{The R Book}. John Wiley & Sons, Incorporated. ISBN:9780470973929. p.350-352.
#'     \href{https://onlinelibrary.wiley.com/doi/book/10.1002/9781118448908}{\doi{10.1002/9781118448908}}
#'
#'   Joanes, D.N., and Gill, C.A. (1998). Comparing measures of sample skewness and kurtosis.
#'   \emph{Journal of the Royal Statistical Society. Series D (The Statistician)} \strong{47}(1): 183–189.
#'   \href{https://doi.org/10.1111/1467-9884.00122}{\doi{10.1111/1467-9884.00122}}
#'
#' @seealso [`distributions`][stats::distributions]
#' @family skewness
#'
#' @param x a \code{numeric} vector.
#'
#' @param adjust \code{logical} indicating whether \var{x} is a sample from a population; default \code{TRUE}.
#'
#' @return A numeric containing the skewness value.
#'
#' @keywords univar
#' @export
#' @examples
#'  ## Heights of 100 randomly selected male university students, adapted from Spiegel and Stephens
#'  ## (Theory and Problems of Statistics. 4th edn. McGraw-Hill. 1999. ISBN 9780071755498).
#'  table(heights)
#'  hist(heights, seq(59.5, 74.5, 3))
#'  skewness(heights) 
#'  skewness(heights, adjust = FALSE)
#'
#'  ## Litter sizes in albino rats (n = 815), data from King (1924; Litter production and
#'  ## the sex ratio in various strains of rats. The Anatomical Record 27(5), 337-366).
#'  table(litter_sizes)
#'  hist(litter_sizes, 0:12)
#'  skewness(litter_sizes) 
#'  skewness(litter_sizes, adjust = FALSE)
#'

skewness <- function(x, adjust = TRUE) {
    n <- length(x)
    skw <- moment(x, 3) / sqrt(moment(x, 2))^3
    if (adjust)
        skw <- skw * sqrt(n * (n - 1)) / (n - 2)
    skw
}

# ========================================
# Moment - not exported
moment <- function(x, r)
    sum((x - mean(x))^r) / length(x)

# ========================================
# Skew is an alias for Skewness
#' @rdname skewness
#' @export
skew <- skewness

# ========================================
#' Test For Skewness of a Variable 
#'
#' @aliases skew.test
#'
#' @description
#' Computes \eqn{G_{1}}{G<sub>1</sub>}, the expected population skewness of the values in \var{x} using
#' \code{skewness()}, performs a \var{t}-test of its significance and calculates a confidence interval.
#'
#' @details
#' The \var{t}-statistic is given by the estimated population [`skewness`][skewness], \eqn{G_{1}}{G<sub>1</sub>},
#' divided by its standard error, \eqn{SE_{G_{1}}}{SE<sub>G1</sub>}, where: -
#'
#' \deqn{SE_{G_{1}} = \displaystyle \sqrt{\frac{6n(n - 1)}{(n-2)(n+1)(n+3)}}}{%
#'   SE<sub>G1</sub> = &radic;(((6n(n-1))/((n-2)(n+1)(n+3)))}
#'
#' (see e.g., Joanes and Gill, 1998; Wright and Herrington 2011), or alternatively its approximation,
#' \eqn{\sqrt (6 / n_x)}, and the associated probability is derived from the \var{t}-distribution with
#' \eqn{n_{x}-2}{n<sub>x</sub>-2} degrees of freedom. The \var{t}-test is conducted according to Crawley (2012),
#' except that the default here is a two-tailed test. The corresponding confidence interval is calculated similarly
#' from the quantiles of the \var{t}-distribution using both the \code{alternative} and \code{conf.level} arguments.
#'
#' [`skew.test()`][skew.test] is an alias for \code{skewness.test()}.
#'
#' @note The confidence interval is poorly described in the available literature, seems somewhat controversial and
#' should be used with caution.
#'
#' @references
#'   Crawley, Michael J. (2012) \emph{The R Book}. John Wiley & Sons, Incorporated. ISBN:9780470973929. p.350-352.
#'     \href{https://onlinelibrary.wiley.com/doi/book/10.1002/9781118448908}{\doi{10.1002/9781118448908}}
#'
#'   Joanes, D.N., and Gill, C.A. (1998). Comparing measures of sample skewness and kurtosis.
#'   \emph{Journal of the Royal Statistical Society. Series D (The Statistician)} \strong{47}(1): 183–189.
#'   \href{https://doi.org/10.1111/1467-9884.00122}{\doi{10.1111/1467-9884.00122}}
#'
#'   Wright, D.B., and Herrington, J.A. (2011). Problematic standard errors and confidence intervals for skewness and
#'   kurtosis. \emph{Behavior Research Methods} \strong{43}(1): 8-17.
#'   \href{https://doi.org/10.3758/s13428-010-0044-x}{\doi{10.3758/s13428-010-0044-x}}
#'
#' @seealso [`distributions`][stats::distributions], [`TDist`][stats::pt], [`t.test`][stats::t.test]
#' @family skewness
#'
#' @param x a \code{numeric} vector.
#'
#' @param se_method a character string specifying the method of calculating the standard error; must be one of
#'   \code{"Cramer"} (default), or \code{"simple"}. You can specify just the initial letter.
#'
#' @param conf.level the confidence level required; default \var{0.95}.
#'
#' @inheritParams stats::t.test
#'
#' @return A list with class \code{"htest"} containing the following components: -
#'
#' \item{statistic}{the value of the t-statistic.}
#'
#' \item{parameter}{the degrees of freedom for the t-statistic.}
#'
#' \item{p.value}{the p-value for the test.}
#'
#' \item{conf.int}{confidence interval of the skewness (95% or other specified level).}
#'
#' \item{estimate}{the estimate of skewness.}
#'
#' \item{alternative}{a character string describing the alternative hypothesis.}
#'
#' \item{method}{the character string "Skewness with t-test" and the standard error method used.}
#'
#' \item{data.name}{a character string giving the name of the data.}
#'
#' @keywords htest
#' @export
#' @examples
#'  ## Heights of 100 randomly selected male university students, adapted from Spiegel and Stephens
#'  ## (Theory and Problems of Statistics. 4th edn. McGraw-Hill. 1999. ISBN 9780071755498).
#'  table(heights)
#'  skewness.test(heights) 
#'  skewness.test(heights, se_method = "simple")
#'
#'  ## Litter sizes in albino rats (n = 815), data from King (1924; Litter production and
#'  ## the sex ratio in various strains of rats. The Anatomical Record 27(5), 337-366).
#'  table(litter_sizes)
#'  skewness.test(litter_sizes) 
#'  with(skewness.test(litter_sizes), c(stderr = estimate/statistic))
#'
#'  ## Compare a range of distributions, each with the three possible alternative hypotheses
#'  list(
#'    uniform = runif(30),
#'    normal = rnorm(30),
#'    lognormal = rlnorm(30),
#'    poisson = rpois(30, lambda = 10),
#'    negbinom = rnbinom(30, mu = 4, size = 2)
#'  ) |>
#'  map(\(distrib)
#'      c("less", "two.sided","greater") |>
#'      setNames(nm = _) |>
#'      map(\(altern)
#'          with(skewness.test(distrib, altern),
#'              data.frame(
#'                  Lower = conf.int[1],
#'                  Upper = conf.int[2],
#'                  Skewness = estimate,
#'                  t = statistic,
#'                  df = parameter,
#'                  p = p.value,
#'                  sig = starsig(p.value),
#'                  row.names = NULL
#'              )
#'          )
#'      ) |>
#'      bind_rows(.id = "Alternative")) |>
#'  bind_rows(.id = "Distribution")
#'

skewness.test <- function(x, alternative = c("two.sided", "less", "greater"), se_method = c("Cramer", "simple"),
    conf.level = 0.95) {
  alternative <- match.arg(alternative)
  se_method <- match.arg(se_method)
  stopifnot(length(x) > 4, conf.level <= 1, conf.level > 0)
  n <- length(x)
  estimate <- skewness(x)
  if(identical(se_method, "Cramer"))
      se <- sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3)))
  else
      se <- sqrt(6 / length(x))
  tstat <- estimate / se
  df <- n - 2
  pval <- switch(alternative,
    less = pt(tstat, df),
    greater = pt(tstat, df, lower.tail = FALSE),
    two.sided = 2 * pt(-abs(tstat), df)
  )
  cint <- switch(alternative,
    less = c(-Inf, estimate + se * qt(conf.level, df)),
    greater = c(estimate - se * qt(conf.level, df), Inf),
    two.sided = estimate + c(-1, 1) * se * qt((1 + conf.level)/2, df)
  )
  attr(tstat, "method") <- se_method
  attr(cint, "conf.level") <- conf.level
  names(tstat) <- "t"
  names(estimate) <- "skewness"
  names(df) <- "df"
  nval <- c(skewness = 0)

  structure(
    list(
      statistic = tstat,
      parameter = df,
      p.value = pval,
      conf.int = cint,
      estimate = estimate,
      null.value = nval,
      alternative = alternative,
      method = paste0("Skewness with t-test (", attr(tstat, "method"), " stderr)"),
      data.name = deparse(substitute(x))
    ),
    class = "htest"
  )
}

# ========================================
# Skew.test is an alias for Skewness.test
#' @rdname skewness.test
#' @export
skew.test <- skewness.test
