% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/skewness.R
\name{skewness}
\alias{skewness}
\alias{skew}
\title{Skewness}
\usage{
skewness(x, adjust = TRUE)

skew(x, adjust = TRUE)
}
\arguments{
\item{x}{a \code{numeric} vector.}

\item{adjust}{\code{logical}, indicating whether \code{x} is a sample from a population; default \code{TRUE}.}
}
\value{
A \code{numeric} containing the skewness value.
}
\description{
Computes the skewness, \eqn{\gamma_{1}}{&gamma<sub>1</sub>}, of the values in \code{x} with optional adjustment to
give \eqn{G_{1}}{G<sub>1</sub>}, the expected populaton value of skewness from a sample.
}
\details{
Moments for samples of size \code{n} are given by: -

\deqn{m_{r} = \displaystyle \frac{\sum \left(x - \overline{x} \right)^{r}}{n}}{%
    m<sub>r</sub> = &sum;(x - x)<sup>r</sup>/n}

The skewness (or skew) \eqn{\gamma_{1}}{&gamma;<sub>1</sub>} of a numeric variable is the third moment about the
mean rendered dimensionless by dividing by the cube of the square root of the second moment: -

\deqn{\gamma_{1} = \displaystyle \frac{m_3}{{\sqrt{m_{2}}}^3}}{%
    &gamma;<sub>1</sub> = m<sub>3</sub> / (m<sub>2</sub>)<sup>3/2</sup>}

The expected population value of skewness \eqn{G_{1}}{G<sub>1</sub>} from a sample is obtained using: -

\deqn{G_{1} = \displaystyle \frac{\sqrt{n(n - 1)}}{n-2}\gamma_{1}}{%
    G<sub>1</sub> = (&radic;(n(n-1))/(n-2))&gamma;<sub>1</sub>}

\code{\link{skew}} is an alias for \code{skewness()}.

(Adapted from Crawley, 2012, and Joanes and Gill, 1998.)
}
\examples{
 ## Heights of 100 randomly selected male university students, adapted from Spiegel and Stephens
 ## (Theory and Problems of Statistics. 4th edn. McGraw-Hill. 1999. ISBN 9780071755498).
 table(heights)
 hist(heights, seq(59.5, 74.5, 3))
 skewness(heights) 
 skewness(heights, adjust = FALSE)

 ## Litter sizes in albino rats (n = 815), data from King (1924; Litter production and
 ## the sex ratio in various strains of rats. The Anatomical Record 27(5), 337-366).
 table(litter_sizes)
 hist(litter_sizes, 0:12)
 skewness(litter_sizes) 
 skewness(litter_sizes, adjust = FALSE)

}
\references{
Crawley, Michael J. (2012) \emph{The R Book}. John Wiley & Sons, Incorporated. ISBN:9780470973929. p.350-352.
\href{https://onlinelibrary.wiley.com/doi/book/10.1002/9781118448908}{\doi{10.1002/9781118448908}}

Joanes, D.N., and Gill, C.A. (1998). Comparing measures of sample skewness and kurtosis.
\emph{Journal of the Royal Statistical Society. Series D (The Statistician)} \strong{47}(1): 183–189.
\href{https://doi.org/10.1111/1467-9884.00122}{\doi{10.1111/1467-9884.00122}}
}
\seealso{
\code{\link[stats:Distributions]{distributions}}

Other skewness: 
\code{\link{kurtosis}()},
\code{\link{kurtosis.test}()},
\code{\link{skewness.test}()}
}
\concept{skewness}
\keyword{univar}
