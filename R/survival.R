# BitsnBobs R Package
# Mark Eisler May 2026
# For general bits and bobs of code
#
# Requires R version 4.3.0 (2023-04-21) -- "Already Tomorrow" or later
#
# survival.R


# ========================================
#' Likelihood ratio test statistic
#'
#' @description
#' `LRT()` is a generic function for the likelihood ratio test statistic from a given object. 
#'
#' @details
#' Currently implemented for `"coxph"` and `"coxme"` objects.
#'
#' @references
#' \href{https://cran.r-project.org/web/packages/coxme/vignettes/coxme.pdf}{
#'   Terry Therneau, 2024: Mixed Effects Cox Models}.
#'
#' @family survival
#' @seealso [`coxph()`][survival::coxph], [`coxme()`][coxme::coxme], [`logLik.coxme()`][coxme::logLik.coxme].
#'
#' @param object	 any object from which a likelihood ratio test statistic, or a contribution to a 
#'   likelihood ratio test statistic, can be extracted.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @param type character, which of the two types of partial likelihood to extract, either
#'   "penalized" or "integrated".
#'
#' @return
#' Returns the likelihood ratio statistic as a `double`. 
#'
#' @keywords models regression survival
#'
#' @export
#' @examples
#' if (require("coxme", quietly = TRUE)) {
#'     efit1 <- coxph(Surv(y, uncens) ~ trt, eortc)
#'     efit2 <- coxme(Surv(y, uncens) ~ trt + (1|center), eortc)
#'     
#'     summary(efit1) |> print_lf()
#'     cat("LRT(efit1):", LRT(efit1), "\n\n")
#' 
#'     efit2 |> print_lf()
#'     c(LRT(efit2), LRT(efit2, type = "penalized"))
#' } else {
#'     message("R packages coxme and survival must both be installed to run example.")
#' }
#' 


LRT <- function(object, ...)
    UseMethod("LRT")

# ========================================
#  Method for class "coxph"
#  S3method LRT.coxph()
#'
#' @rdname LRT
#' @export

LRT.coxph <- function(object, ...)
    with(object, 2 * diff(loglik[1:2]))

# ========================================
#  Method for class "coxme"
#  S3method LRT.coxme()
#'
#' @rdname LRT
#' @export

LRT.coxme <- function(object, ..., type = c("integrated", "penalized"))
{
    type <- match.arg(type)
    idx <- switch(type,
        integrated = 1:2,
        penalized = c(1, 3)
    )
    with(object, 2 * diff((loglik + c(0, 0, penalty))[idx]))
}