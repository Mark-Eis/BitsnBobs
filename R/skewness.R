# BitsnBobs R Package
# Mark Eisler - May 2024
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
#' Computes the skewness, \eqn{\gamma_{1}}{&gamma<sub>1</sub>}, of the values in `x` with optional adjustment to
#' give \eqn{G_{1}}{G<sub>1</sub>}, the expected populaton value of skewness from a sample.
#'
#' @details
#' Moments for samples of size `n` are given by: -
#'
#'   \deqn{m_{r} = \displaystyle \frac{\sum \left(x - \overline{x} \right)^{r}}{n}}{%
#'     m<sub>r</sub> = &sum;(x - x)<sup>r</sup>/n}
#'
#' The skewness (or skew) \eqn{\gamma_{1}}{&gamma;<sub>1</sub>} of a numeric variable is the third moment about the
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
#' [`skew`][skew] is an alias for `skewness()`.
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
#' @param x a `numeric` vector.
#'
#' @param adjust `logical`, indicating whether `x` is a sample from a population; default `TRUE`.
#'
#' @return A `numeric` containing the skewness value.
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
#' Test of Skewness
#'
#' @aliases skew.test
#'
#' @description
#' Computes \eqn{G_{1}}{G<sub>1</sub>}, the expected population skewness of the values in \var{x} using
#' `skewness()`, performs a \var{t}-test of its significance and calculates a confidence interval.
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
#' from the quantiles of the \var{t}-distribution using both the `alternative` and `conf.level` arguments.
#'
#' [`skew.test()`][skew.test] is an alias for `skewness.test()`.
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
#' @param se_method a `character` string specifying the method of calculating the standard error; must be one of
#'   `"Cramer"` (default), or `"simple"`. You can specify just the initial letter.
#'
#' @param conf.level the confidence level required; default \var{0.95}.
#'
#' @param n an `integer`, the number of observations.
#'
#' @inheritParams skewness
#'
#' @inheritParams stats::t.test
#'
#' @return A list with class `"htest"` containing the following components: -
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
#'  length(heights) |> stderr_skewness()
#'  skewness.test(heights, se_method = "simple")
#'  length(heights) |> stderr_skewness(se_method = "simple")
#'
#'  ## Litter sizes in albino rats (n = 815), data from King (1924; Litter production and
#'  ## the sex ratio in various strains of rats. The Anatomical Record 27(5), 337-366).
#'  table(litter_sizes)
#'  skewness.test(litter_sizes) 
#'  length(litter_sizes) |> stderr_skewness()
#'
#'  ## Compare a range of distributions, each with the three possible alternative hypotheses
#'  list(
#'    uniform = runif(30),
#'    normal = rnorm(30),
#'    lognormal = rlnorm(30),
#'    poisson = rpois(30, lambda = 10),
#'    negbinom = rnbinom(30, mu = 4, size = 2)
#'  ) |>
#'  lapply(\(distrib)
#'      c("less", "two.sided","greater") |>
#'      setNames(nm = _) |>
#'      lapply(\(altern)
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
    se <- stderr_skewness(n, se_method)
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
            data.name = deparse1(substitute(x))
        ),
        class = "htest"
    )
}

# ========================================
# Skew.test is an alias for Skewness.test
#' @rdname skewness.test
#' @export
skew.test <- skewness.test

# ========================================
# Standard Error of Skewness
#' @rdname skewness.test
#' @export

stderr_skewness <- function(n, se_method = c("Cramer", "simple")) {
    se_method <- match.arg(se_method)

    switch(se_method,
        "Cramer" = sqrt(6 * n * (n - 1) / ((n - 2) * (n + 1) * (n + 3))),
        "simple" = sqrt(6 / n),
        stop("Unknown se_method")
    )
}

# ========================================
#' Kurtosis
#'
#' @aliases kurt
#'
#' @description
#' Computes the kurtosis, \eqn{\gamma_{2}}{&gamma<sub>2</sub>}, of the values in `x` with optional adjustment to give
#' \eqn{G_{2}}{G<sub>2</sub>}, the expected populaton value of kurtosis from a sample.
#'
#' @details
#' Moments for samples of size `n` are given by: -
#'
#'   \deqn{m_{r} = \displaystyle \frac{\sum \left(x - \overline{x} \right)^{r}}{n}}{%
#'     m<sub>r</sub> = &sum;(x - x)<sup>r</sup>/n}
#'
#' The (excess) kurtosis \eqn{\gamma_{2}}{&gamma;<sub>2</sub>} of a numeric variable is the fourth moment
#' (\eqn{m_{4}}{m<sub>4</sub>}) about the mean rendered dimensionless by dividing by the square of the second moment
#' (\eqn{m_{2}}{m<sub>2</sub>}), from which 3, the value of
#' (\eqn{m_{4}/m_{2}^2}{m<sub>4</sub>/m<sub>2</sub><sup>2</sup>}) for the normal distribution is subtracted: -
#'
#'   \deqn{\gamma_{2} = \displaystyle \frac{m_4}{{m_{2}}^2} - 3}{%
#'     &gamma;<sub>2</sub> = m<sub>4</sub> / (m<sub>2</sub>)<sup>2</sup> - 3}
#'
#' The expected population value of (excess) kurtosis \eqn{G_{2}}{G<sub>2</sub>} from a sample is obtained using: -
#'
#'   \deqn{G_{2} = \displaystyle \frac{(n - 1)}{(n-2)(n-3)}[(n+1)\gamma_{2} + 6]}{%
#'     G<sub>2</sub> = ((n-1)/((n-2)(n-3)))[(n+1)&gamma;<sub>2</sub> + 6]}
#'
#' (Adapted from Crawley, 2012, and Joanes and Gill, 1998.)
#'
#' @references
#'   Crawley, Michael J. (2012) \emph{The R Book}. John Wiley & Sons, Incorporated. ISBN:9780470973929. p.350-352.
#'     \href{https://onlinelibrary.wiley.com/doi/book/10.1002/9781118448908}{\doi{10.1002/9781118448908}}
#'
#'   Joanes, D.N., and Gill, C.A. (1998). Comparing measures of sample kurtosis and kurtosis.
#'   \emph{Journal of the Royal Statistical Society. Series D (The Statistician)} \strong{47}(1): 183–189.
#'   \href{https://doi.org/10.1111/1467-9884.00122}{\doi{10.1111/1467-9884.00122}}
#'
#' @seealso [`distributions`][stats::distributions]
#' @family skewness
#'
#' @param xs `logical`, indicating whether to calculate excess kurtosis i.e., the difference from the kurtosis of the
#'   normal distribution; default `TRUE`.
#'
#' @inheritParams skewness
#'
#' @return A `numeric` containing the kurtosis value.
#'
#' @keywords univar
#' @export
#' @examples
#'  ## Heights of 100 randomly selected male university students, adapted from Spiegel and Stephens
#'  ## (Theory and Problems of Statistics. 4th edn. McGraw-Hill. 1999. ISBN 9780071755498).
#'  table(heights)
#'  hist(heights, seq(59.5, 74.5, 3))
#'  kurtosis(heights) 
#'  kurtosis(heights, adjust = FALSE)
#'
#'  ## Litter sizes in albino rats (n = 815), data from King (1924; Litter production and
#'  ## the sex ratio in various strains of rats. The Anatomical Record 27(5), 337-366).
#'  table(litter_sizes)
#'  hist(litter_sizes, 0:12)
#'  kurtosis(litter_sizes) 
#'  kurtosis(litter_sizes, adjust = FALSE)
#'

kurtosis <- function(x, xs = TRUE, adjust = TRUE) {
    n <- length(x)
    krt <- moment(x, 4) / moment(x, 2)^2

    if (adjust){ 
    	krt <- (n - 1) * ((n + 1) * (krt - 3) + 6) / ((n - 2) * (n - 3))
        if (xs) krt else krt + 3
    } else
    	if (xs) krt - 3 else krt
}


# ========================================
#' Test of Kurtosis 
#'
#' @aliases kurt.test
#'
#' @description
#' Computes \eqn{G_{2}}{G<sub>2</sub>}, the expected population kurtosis of the values in \var{x} using
#' `kurtosis()`, performs a \var{t}-test of its significance and calculates a confidence interval.
#'
#' @details
#' The \var{t}-statistic is given by the estimated population [`kurtosis`][kurtosis], \eqn{G_{2}}{G<sub>2</sub>},
#' divided by its standard error, \eqn{SE_{G_{2}}}{SE<sub>G2</sub>}, where: -
#'
#' \deqn{SE_{G_{2}} = \displaystyle 2(SE_{G_{1}}) \sqrt{\frac{(n^{2} - 1)}{(n-3)(n+5)}}}{%
#'   SE<sub>G2</sub> = 2(SE<sub>G1</sub>) &radic;((n<sup>2</sup>-1)/((n-3)(n+5)))}
#'
#' (see e.g., Joanes and Gill, 1998; Wright and Herrington 2011), or alternatively its approximation,
#' \eqn{\sqrt (24 / n_x)}, and the associated probability is derived from the \var{t}-distribution with
#' \eqn{n_{x}-2}{n<sub>x</sub>-2} degrees of freedom. The \var{t}-test is conducted according to Crawley (2012),
#' except that the default here is a two-tailed test. The corresponding confidence interval is calculated similarly
#' from the quantiles of the \var{t}-distribution using both the `alternative` and `conf.level` arguments.
#'
#' [`kurt.test()`][kurt.test] is an alias for `kurtosis.test()`.
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
#' @inheritParams skewness.test
#'
#' @inheritParams stats::t.test
#'
#' @return A list with class `"htest"` containing the following components: -
#'
#' \item{statistic}{the value of the t-statistic.}
#'
#' \item{parameter}{the degrees of freedom for the t-statistic.}
#'
#' \item{p.value}{the p-value for the test.}
#'
#' \item{conf.int}{confidence interval of the kurtosis (95% or other specified level).}
#'
#' \item{estimate}{the estimate of kurtosis.}
#'
#' \item{alternative}{a character string describing the alternative hypothesis.}
#'
#' \item{method}{the character string "Kurtosis with t-test" and the standard error method used.}
#'
#' \item{data.name}{a character string giving the name of the data.}
#'
#' @keywords htest
#' @export
#' @examples
#'  ## Heights of 100 randomly selected male university students, adapted from Spiegel and Stephens
#'  ## (Theory and Problems of Statistics. 4th edn. McGraw-Hill. 1999. ISBN 9780071755498).
#'  table(heights)
#'  kurtosis.test(heights) 
#'  length(heights) |> stderr_kurtosis()
#'  kurtosis.test(heights, se_method = "simple")
#'  length(heights) |> stderr_kurtosis(se_method = "simple")
#'
#'  ## Litter sizes in albino rats (n = 815), data from King (1924; Litter production and
#'  ## the sex ratio in various strains of rats. The Anatomical Record 27(5), 337-366).
#'  table(litter_sizes)
#'  kurtosis.test(litter_sizes) 
#'  length(litter_sizes) |> stderr_kurtosis()
#'

kurtosis.test <- function(x, alternative = c("two.sided", "less", "greater"), se_method = c("Cramer", "simple"),
    conf.level = 0.95) {
  alternative <- match.arg(alternative)
  se_method <- match.arg(se_method)
  stopifnot(length(x) > 4, conf.level <= 1, conf.level > 0)
  n <- length(x)
  estimate <- kurtosis(x)
  se <- stderr_kurtosis(n, se_method)
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
  names(estimate) <- "kurtosis"
  names(df) <- "df"
  nval <- c(kurtosis = 0)

  structure(
        list(
            statistic = tstat,
            parameter = df,
            p.value = pval,
            conf.int = cint,
            estimate = estimate,
            null.value = nval,
            alternative = alternative,
            method = paste0("Kurtosis with t-test (", attr(tstat, "method"), " stderr)"),
            data.name = deparse1(substitute(x))
        ),
        class = "htest"
  )
}

# ========================================
# Kurt.test is an alias for Kurtosis.test
#' @rdname kurtosis.test
#' @export
kurt.test <- kurtosis.test

# ========================================
# Standard Error of Kurtosis
#' @rdname kurtosis.test
#' @export

stderr_kurtosis <- function(n, se_method = c("Cramer", "simple")) {
    se_method <- match.arg(se_method)

    switch(se_method,
        "Cramer" = 2 * stderr_skewness(n, se_method) * sqrt((n^2 -1) / ((n - 3) * (n + 5))),
        "simple" = sqrt(24 / n),
        stop("Unknown se_method")
    )
}
