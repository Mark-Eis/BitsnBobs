# BitsnBobs R Package
# Mark Eisler - Mar 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# kappa-coef.R

# ========================================
#' @title
#' Cohen's Kappa Coefficient of Agreement for Nominal Scales
#'
#' @description
#' Cohen's \emph{kappa} measures the agreement between two raters (or diagnostic tests) who each classify \var{N} items
#' into \var{C} mutually exclusive categories, typically two categories in the case of diagnostic tests.
#'
#' `print_kappa()` first prints the standard \code{htest} output, then the matrices of observed and expected numbers with
#' their marginal sums, and lastly the numbers and proportions of observations agreeing.
#'
#' @details
#' Cohen's \emph{kappa} coefficient is given by: -
#'
#'  \deqn{\displaystyle \kappa = \frac{p_{o}-p_{e}}{1-p_{e}}}{%
#'        kappa = (po - pe) / (1 - pe)}
#'
#' where \eqn{p_{o}} is the proportion of observations in agreement and \eqn{p_{e}} is the proportion of observations
#' expected to agree by chance.
#'
#' Cohen's (1960) original approximation to the \emph{standard error} of \eqn{\kappa}{kappa} is given by: -
#'
#'  \deqn{\displaystyle se = \sqrt{\frac{p_{o}(1-p_{o})}{n(1-p_{e})^{2}}}}{%
#'        se = sqrt(po(1 - po) / n(1 - pe)^2)}
#'
#' Output returned by \code{cohens_kappa()} maybe printed using [`print_all()`][print_all] to provide additional
#'   information, see examples.
#'
#' @note
#' Professor Joseph Fleiss \emph{et al.} (1979) observed: \samp{Many human endeavors have been cursed with repeated
#' failures before final success is achieved. The scaling of Mount Everest is one example. The discovery of the Northwest
#' Passage is a second. The derivation of a correct standard error for kappa is a third.} Coding the Fleiss \emph{et al.}
#' (1979) standard error method in R was an endeavour similarly cursed!
#'
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal scales. \emph{Educ Psychol Meas}, \strong{20}, 37–46.
#'   \href{https://doi.org/10.1177/001316446002000104}{\doi{10.1177/001316446002000104}}.
#'
#' Fleiss, J.L., Nee, J.C., & Landis, J.R. (1979). Large sample variance of kappa in the case of different sets of raters.
#'   \emph{Psychol Bull}, \strong{86}(5), 974–977. https://doi.org/10.1037/0033-2909.86.5.974
#'   \href{https://psycnet.apa.org/record/1979-32706-001?doi=1}{\doi{10.1037/0033-2909.86.5.974}}.
#'
#' @seealso \code{\link[base]{matrix}}, \code{\link[stats]{mcnemar.test}}
#' @family kappa-phi
#'
#' @param x a square \code{matrix} containing the observations of two raters or results of two diagnostic tests.
#'
#' @param se_method a character string specifying the method of calculating the standard error; must be one of
#'   \code{"Fleiss"} (default), or \code{"Cohen"}. You can specify just the initial letter.
#'
#' @param conf.level the confidence level required; default \var{0.95}.
#'
#' @inheritParams base::print
#'
#' @return A list with classes `"cohens_kappa"` and `"htest"`, containing the following components: -
#'
#' \item{statistic}{\eqn{\kappa}{kappa}, the kappa coefficient.}
#'
#' \item{parameter}{the total number of observations.}
#'
#' \item{conf.int}{confidence interval of \eqn{\kappa}{kappa} (95% or other specified level).}
#'
#' \item{estimate}{the number of agreements observed and the number expected by chance.}
#'
#' \item{stderr}{the standard error of \eqn{\kappa}{kappa}.}
#'
#' \item{observed}{the observed counts.}
#'
#' \item{expected}{the expected counts under the null hypothesis of zero agreement.}
#'
#' \item{data.name}{a character string giving the name of the data.}
#'
#' \item{method}{the character string "Cohen's kappa coefficient of agreement" and the standard error method used.}
#'
#' @keywords htest
#' @export
#' @examples
#'  ## Two-by-two table for diagnostic test comparison
#'  (twobytwo <- matrix(c(31, 12, 4, 58), nrow = 2, dimnames = rep(list(c("+ve", "-ve")), 2) |>
#'                setNames(c("Test1", "Test2"))
#'              ))
#'
#'  (ck <- cohens_kappa(twobytwo))
#'
#'  ck |> print_all()   
#'
#'  ## Example from Altman et al. (Statistics with Confidence 2nd Edn. 2008. ISBN:978-0-727-91375-3,
#'  ## p.117), using, as they did, Cohen's approximation to the standard error. 
#'  (twobytwo <- matrix(c(32, 3, 6, 42), nrow = 2, dimnames = rep(list(c("Yes", "No")), 2) |>
#'                setNames(c("Parent", "Paediatrician"))
#'              ))
#'
#'  cohens_kappa(twobytwo, se_method = "Cohen") |> print_all()   
#'
#'  ## Confidence interval using Fleiss et al.'s standard error for comparison
#'  cohens_kappa(twobytwo, se_method = "Fleiss") |> _$conf.int
#'
#'  ## Example with three categories from Cohen (1960).
#'  (threebythree <- matrix(c(88, 10,  2, 14, 40,  6, 18, 10, 12), nrow = 3,
#'                    dimnames = rep(list(c("Cat1", "Cat2", "Cat3")), 2) |>
#'                        setNames(c("Judge_B", "Judge_A"))
#'                  ))
#'
#'  cohens_kappa(threebythree, se_method = "Cohen") |> print_all()
#'
#'  ## Using Fleiss et al.'s standard error for comparison
#'  cohens_kappa(threebythree, se_method = "Fleiss") |> _$conf.int
#'
#'  rm(ck, threebythree, twobytwo)

cohens_kappa <- function (x, se_method = c("Fleiss", "Cohen"), conf.level = 0.95) {
  stopifnot(is.matrix(x), length(dim(x)) == 2L, identical(dim(x)[1], dim(x)[2]), conf.level <= 1, conf.level > 0)
  se_method <- match.arg(se_method)
  N <- sum(x)
  AGREE <- sum(diag(x))
  EXPECTED <- tcrossprod(apply(x, 1, sum), apply(x, 2, sum)) / N
  EXP_AGREE <- sum(diag(EXPECTED))
  ESTIMATE <- (AGREE - EXP_AGREE)/(N - EXP_AGREE)

  p_agree <- AGREE / N
  if(identical(se_method, "Fleiss")) {
    p_exp_agree <- EXP_AGREE / N
    one_minus_kappa <- 1 - ESTIMATE
    rev_diag <- diag(revmat(x)) / N
    se <- sqrt(sum((\(x) x * (1 - (sum(rev_diag) + 2 * x) * one_minus_kappa)^2)(diag(x) / N)) +
               sum((\(x, y) x * (p_agree + 2 * y)^2 * one_minus_kappa^2)(rev_diag, rev(rev_diag))) -
               (ESTIMATE - p_exp_agree * one_minus_kappa)^2)/((1 - p_exp_agree) * sqrt(N))
  } else
    se <- sqrt(AGREE * (1 - p_agree))/(N - EXP_AGREE)
  attr(se, "method") <- se_method
  CINT <- ESTIMATE + c(-1, 1) * qnorm((1 + conf.level)/2) * se
  attr(CINT, "conf.level") <- conf.level
  AGREEMENTS <- c(observed = AGREE, expected = EXP_AGREE)
  names(ESTIMATE) <- "kappa"
  dimnames(EXPECTED) <- dimnames(x)

  structure(
    list(
      statistic = c(stderr = se), 
      parameter = c(`number of observations` = N),
      estimate = ESTIMATE, 
      conf.int = CINT,
      stderr = se,
      observed = x, 
      expected = EXPECTED,
      agreements = AGREEMENTS,
      data.name = deparse(substitute(x)),
      method = paste0("Cohen's kappa coefficient of agreement (", attr(se, "method"), " stderr)")
   ),
   # class = "htest"
   class = c("cohens_kappa", "htest")
  )
}

# ========================================
# Print observed and expected numbers with marginal sums,
# and numbers and proportions of observations agreeing
#  S3method print_all.cohens_kappa()
#'
#' @rdname cohens_kappa
#' @export

print_all.cohens_kappa <- function(x, ...) {
    with(NextMethod(),
    {
        cat("Observed: -\n")  
        addmargins(observed) |> print_lf()
        cat("Expected: -\n")  
        addmargins(expected) |> print_lf()
        cat("Number of agreements: -\n")  
        print_lf(agreements)
        cat("Proportion in agreement: -\n")  
        print_lf(agreements / parameter)
    })
    invisible(x)
} 

