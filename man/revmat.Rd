% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{revmat}
\alias{revmat}
\title{Reverse a Matrix by Rows, Columns or Both}
\usage{
revmat(x, type = c("byrow", "bycolumn", "byboth"))
}
\arguments{
\item{x}{a \code{matrix}.}

\item{type}{a character string specifying whether to reverse matrix rows, columns or both; must be one of
\code{"byrow"} (default), \code{"bycolumn"} or \code{"byboth"}. You can specify just the initial letter.}
}
\value{
A \code{matrix} of the same dimensions as \var{x}.
}
\description{
Reverses the order of entire matrix rows, columns or both.
}
\details{
Depending on the argument \code{type}, this function reverses the order of entire rows, columns or both of a matrix,
including any dimnames if present.
}
\examples{
 m <- matrix(1:9, nrow = 3, byrow = TRUE, dimnames = list(paste0("x", 1:3), paste0("y", 1:3)))

 m
 revmat(m)
 revmat(m, "bycol")
 revmat(m, "byboth")

 rm(m)
}
\seealso{
\code{\link[base]{matrix}}, \code{\link[base]{t}}

Other utils: 
\code{\link{const}()},
\code{\link{endstop}()},
\code{\link{marker}()},
\code{\link{op-min-max}}
}
\concept{utils}
\keyword{array}
