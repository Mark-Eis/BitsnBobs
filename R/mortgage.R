# BitsnBobs R Package
# Mark Eisler - Jan 2024
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# mortgage.R


# ========================================
#' @title
#' Effective Rate of Interest from a Nominal Rate and its Conversion Frequency or the Inverse
#'
#' @description
#' `eff_rate()` calculates the effective rate of annual interest from a nominal rate and its interest conversion
#' frequency.
#'
#' `nom_rate()` calculates the nominal rate of interest for a given interest conversion frequency from an effective rate.
#'
#' `cc_rate()` calculates the continuous compounding rate of interest from an effective rate.
#'
#' @details
#' Effective Rate of Interest (er): —
#'
#'   \deqn{er = \displaystyle \left(1 + \frac{nr}{ic} \right)^{ic} - 1}{%
#'     er = (1 + nr/ic)<sup>ic</sup> - 1}
#'
#' Nominal Rate of Interest (nr): —
#'
#'   \deqn{nr = \displaystyle \left(\left(1 + er \right)^{\frac{1}{ic}} - 1 \right).ic}{%
#'     nr = ((1 + er)\<sup>1/ic</sup> - 1).ic}
#'
#' Continuous Compounding Rate of Interest (ccr): —
#'
#'   \deqn{ccr = \displaystyle e^{er} - 1}{%
#'     ccr = exp(er) - 1}
#' 
#' where \eqn{er} is the effective rate of interest, \eqn{nr} is the nominal rate of interest, and \eqn{ic} is
#' the interest conversion frequency. See [`amort.period()`][FinancialMath::amort.period] in package
#' \CRANpkg{FinancialMath} for further details.
#'
#' @references
#'   Investopedia: Effective Annual Interest Rate: Definition, Formula, and Example.
#'     \href{https://www.investopedia.com/terms/e/effectiveinterest.asp}{www.investopedia.com/terms/e/effectiveinterest}
#'
#'   Investopedia: Continuous Compounding Definition and Formula.
#'     \href{https://www.investopedia.com/terms/c/continuouscompounding.asp}{
#'       www.investopedia.com/terms/c/continuouscompounding}
#'
#' @family amort
#' @seealso [`amort.period()`][FinancialMath::amort.period], [`amort.table()`][FinancialMath::amort.table]
#'
#' @param nom_rate numeric, the nominal interest rate convertible `con_fq` times per year.
#'
#' @param con_fq, integer, the interest conversion frequency per year.
#'
#' @param eff_rate numeric, the effective rate of interest.
#'
#' @return A numeric: `eff_rate()` returns the effective rate of interest; `nom_rate()` returns the nominal
#'   interest rate convertible \var{con_fq} times per year.
#'
#' @export
#' @examples
#' ## A lender calculates interest daily using a simple annual rate of 5.7480%,
#' ## based on a 365-day year.
#'
#' ## Calculate effective rate: -
#' eff_rate(0.057480, 365)
#'
#' ## Calculate nominal rates at monthly and daily interest conversion frequencies: -
#' ## Monthly
#' nom_rate(0.05915929, 12)
#' ## Daily—same as the original simple annual rate
#' nom_rate(0.05915929, 365)
#'
#' ## Example from https://www.investopedia.com/terms/e/effectiveinterest.asp
#' eff_rate(0.10, c(2, 4, 12, 365)) |>
#'     setNames(c("Semiannual", "Quarterly", "Monthly", "Daily"))
#'
#' ## Limit of compounding (ibid.)
#' eff_rate(0.1, c(365, 365 * 24, 365 * 24 * 60, 365 * 24 * 60 * 60)) |>
#'     setNames(c("Daily", "Hourly", "Minutely", "Secondly"))
#' ## Continuous compounding
#' cc_rate(0.1)

eff_rate <- function(nom_rate, con_fq)
	(1 + nom_rate/con_fq)^con_fq - 1

# ========================================
#' nom_rate()
#' @rdname eff_rate
#' @export
nom_rate <- function(eff_rate, con_fq)
	((1 + eff_rate)^(1/con_fq) - 1) * con_fq

# ========================================
#' Function to return continuous compounding rate from effective annual rate
#' cc_rate()
#' @rdname eff_rate
#' @export
cc_rate <- function(eff_rate)
	exp(eff_rate) - 1

# ========================================
#' @title
#' Amount of Interest from an Effective Rate, a Payment Frequency and a Balance
#'
#' @description
#' `j()` calculates j, the amount of interest payable from an effective rate, a payment frequency and a balance.
#'
#' `j2eff_rate()` calculates the effective interest rate from the amount of interest payable, a payment frequency and
#' a balance.
#'
#' @details
#' Interest payable (j): —
#'
#'   \deqn{j = \displaystyle \left(\left(1 + er \right)^{\frac{1}{pf}} - 1 \right).bal}{%
#'     j = ((1 + er)<sup>1/pf</sup> - 1).bal}
#' 
#' Effective rate (er): —
#'
#'   \deqn{er = \displaystyle \left(1 + \frac{j}{bal} \right)^{pf} - 1}{%
#'     er = (1 + j/bal)<sup>pf</sup> - 1}
#' 
#' where \eqn{j} is the interest payment, \eqn{er} is the effective rate, \eqn{pf} is the payment frequency, and
#' \eqn{bal} is the balance. See [`amort.period()`][FinancialMath::amort.period] in package
#' \CRANpkg{FinancialMath} for further details.
#'
#' @family amort
#' @seealso [`amort.period()`][FinancialMath::amort.period], [`amort.table()`][FinancialMath::amort.table]
#'
#' @param eff_rate numeric, the effective annual rate of interest.
#'
#' @param pay_fq, \code{integer}, the payment frequency per year.
#'
#' @param bal numeric, the balance of the loan; default \var{1}.
#'
#' @param j numeric, the interest payment.
#'
#' @return A numeric: `j()` returns the calculated interest payment; `j2eff_rate()` returns the effective rate of
#'   interest.
#'
#' @export
#' @examples
#' j(0.05915929, 12, 4000)
#'
#' j2eff_rate(19.20444, 12, 4000)
#'
j <- function(eff_rate, pay_fq, bal = 1)
	((1 + eff_rate)^(1/pay_fq) - 1) * bal

# ========================================
#  Function to return effective interest rate from the amount of interest payable, a payment frequency and a balance.
#' @rdname j
#' @export
# ______________________________________
j2eff_rate <- function(j, pay_fq, bal = 1) {

	(j / bal + 1)^ pay_fq - 1
}

# ========================================
#' @title
#' Individual Values from the Output of amort.period or amort.table()
#'
#' @description
#' Functions to return individual values from the output of `amort.period()` or `amort.table()` in package
#' \pkg{FinancialMath}.
#'
#' @details
#' `get_amortval()` returns individual values from the output of [`amort.period()`][FinancialMath::amort.period] or
#' the `Other` list element of the output of [`amort.table()`][FinancialMath::amort.table] in package
#' \CRANpkg{FinancialMath}.
#'
#' `get_amortint()` returns the total value of interest payments from the `Schedule` list element of the output of
#' [`amort.table()`][FinancialMath::amort.table], up to and including the period given in `until`.
#'
#' @family amort
#' @seealso [`amort.period()`][FinancialMath::amort.period], [`amort.table()`][FinancialMath::amort.table]
#'
#' @param x either a matrix of input variables, calculated unknown variables, and amortization figures for a given
#' period, as  output by `amort.period()` or a list of two components, `Schedule` (a data frame of an amortization
#' schedule) and `Other`, (a matrix of the input variables and other calculated variables) as output by `amort.table()`.
#'
#' @param val a \code{character} string giving the exact name of the value to be returned.
#'
#' @param until a \code{numeric} signifying the period up to and including which total interest should be summated. 
#'
#' @return A numeric value.
#'
#' @export
#' @examples
#' \dontshow{
#' ## Requires {FinancialMath} package
#' if (!requireNamespace("FinancialMath", quietly = TRUE)) 
#'   warning("package 'FinancialMath' must be installed")
#' }
#' (apd <- try(
#'     FinancialMath::amort.period(Loan = 200099, n = 15 * 12, i = 0.0184, ic = 12, pf = 12, t = 60)
#' ))
#'
#' ## Default value is "Eff Rate"
#' get_amortval(apd)
#' get_amortval(apd, "PMT")  
#' get_amortval(apd, "Balance")  
#'
#' atb <- try(FinancialMath::amort.table(Loan = 200099, n = 15 * 12, i = 0.0184, ic = 12, pf = 12))
#' atb$Other
#'
#' ## Default value is "Eff Rate"
#' get_amortval(atb)
#' get_amortval(atb, "Total Paid")
#' get_amortval(atb, "Total Interest")
#'
#' get_amortint(atb, 60)
#'
#' rm(apd, atb)
 
get_amortval <- function(x, val = "Eff Rate") {

	if (is.list(x)) {
		stopifnot(
			identical(length(x), 2L), identical(names(x)[2], "Other"),
			identical(dimnames(x$Other)[[2]], "Details")
		)
		x <- x$Other
	} else
		stopifnot(
			all(c("matrix", "array", "double", "numeric") %in% .class2(x)),
			identical(dimnames(x)[[2]], "Amortization")		
		)
	idx <- dimnames(x)[[1]] %in% val
	if (!any(idx)) stop(paste0("\"", val, "\" not found in ", dimnames(x)[[2]]))
	x[which(idx)]
}

# ========================================
#  Function to return interest total over a period from the output of amort.table() in {FinancialMath}
#' @rdname get_amortval
#' @export
# ______________________________________
get_amortint <- function(x, until) {

	stopifnot(
		is.list(x),
		identical(length(x), 2L), identical(names(x)[1], "Schedule"),
		identical(dimnames(x$Other)[[2]], "Details")
	)
	idx <- dimnames(x$Schedule)[[2]] %in% "Interest Paid"
	if (!any(idx)) stop(paste0("\"Interest Paid\" not found in ", dimnames(x)[[2]]))
	sum(x$Schedule[seq_len(until), which(idx)])
}
