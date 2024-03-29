% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{op-min-max}
\alias{op-min-max}
\alias{\%:<\%}
\alias{\%>:\%}
\title{Vectorised Min and Max Operators}
\usage{
x \%:<\% y

x \%>:\% y
}
\arguments{
\item{x, y}{numeric or character arguments (see Note).}
}
\value{
As for \code{\link[base:Extremes]{pmin()}} and \code{\link[base:Extremes]{pmax()}}, a vector of length the longest of the input vectors,
or length zero if one of the inputs had zero length.
}
\description{
Vectorised infix functions implementing pmin() and pmax().
}
\details{
The vectorised infix functions \verb{\%:<\%} and \verb{\%>:\%} may be useful in implementing \code{\link[base:Extremes]{pmin()}} and,
\code{\link[base:Extremes]{pmax()}} and was inspired by \code{\link[rlang:op-null-default]{\%||\%}} in the \pkg{\link[rlang]{rlang}} package.
}
\note{
\sQuote{Numeric} arguments are vectors of type integer and numeric,
  and logical (coerced to integer).  For historical reasons, \code{NULL}
  is accepted as equivalent to \code{integer(0)}.% PR#1283

  \code{pmax} and \code{pmin} will also work on classed S3 or S4 objects
  with appropriate methods for comparison, \code{is.na} and \code{rep}
  (if recycling of arguments is needed).
}
\examples{
1:10 \%:<\% 10:1
c(1:10, NA) \%:<\% c(NA, 10:1)

1:10 \%>:\% 10:1
c(1:10, NA) \%>:\% c(NA, 10:1)

}
\seealso{
\code{\link[rlang:op-null-default]{\%||\%}}, \code{\link[base:Extremes]{pmin()}} and \code{\link[base:Extremes]{pmax()}}.

Other utils: 
\code{\link{const}()},
\code{\link{endstop}()},
\code{\link{marker}()},
\code{\link{revmat}()}
}
\concept{utils}
\keyword{arith}
\keyword{univar}
