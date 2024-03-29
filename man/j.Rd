% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mortgage.R
\name{j}
\alias{j}
\alias{j2eff_rate}
\title{Amount of Interest from an Effective Rate, a Payment Frequency and a Balance}
\usage{
j(eff_rate, pay_fq, bal = 1)

j2eff_rate(j, pay_fq, bal = 1)
}
\arguments{
\item{eff_rate}{numeric, the effective annual rate of interest.}

\item{pay_fq, }{\code{integer}, the payment frequency per year.}

\item{bal}{numeric, the balance of the loan; default \var{1}.}

\item{j}{numeric, the interest payment.}
}
\value{
A numeric: \code{j()} returns the calculated interest payment; \code{j2eff_rate()} returns the effective rate of
interest.
}
\description{
\code{j()} calculates j, the amount of interest payable from an effective rate, a payment frequency and a balance.

\code{j2eff_rate()} calculates the effective interest rate from the amount of interest payable, a payment frequency and
a balance.
}
\details{
Interest payable (j): —

\deqn{j = \displaystyle \left(\left(1 + er \right)^{\frac{1}{pf}} - 1 \right).bal}{%
    j = ((1 + er)<sup>1/pf</sup> - 1).bal}

Effective rate (er): —

\deqn{er = \displaystyle \left(1 + \frac{j}{bal} \right)^{pf} - 1}{%
    er = (1 + j/bal)<sup>pf</sup> - 1}

where \eqn{j} is the interest payment, \eqn{er} is the effective rate, \eqn{pf} is the payment frequency, and
\eqn{bal} is the balance. See \code{\link[FinancialMath:amort.period]{amort.period()}} in package
\CRANpkg{FinancialMath} for further details.
}
\examples{
j(0.05915929, 12, 4000)

j2eff_rate(19.20444, 12, 4000)

}
\seealso{
\code{\link[FinancialMath:amort.period]{amort.period()}}, \code{\link[FinancialMath:amort.table]{amort.table()}}

Other amort: 
\code{\link{eff_rate}()},
\code{\link{get_amortval}()}
}
\concept{amort}
