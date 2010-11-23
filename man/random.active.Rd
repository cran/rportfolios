\name{random.active}
\alias{random.active}
\title{ Random actively managed portfolio }
\description{
  This function generates an actively managed random portfolio relative to
  a given benchmark portfolio.  The active portfolio is the sum of the benchmark portfolio
  and a notional neutral long short portfolio with given gross notional exposure.
  The number of non zero positions in the long short portfolio is k.
}
\usage{
random.active(x.b, x.g, k = length( x.b ),  max.iter = 2000, eps = 0.001)
}
\arguments{
  \item{x.b}{ A numeric vector with the investment weights in the benchmark portfolio }
  \item{x.g}{ A positive numeric value for the gross notional exposure in the long short portfolio }
  \item{k}{ A positive integer value for the number of non zero positions in the long short portfolio }
  \item{max.iter}{ A positive integer value for the maximum iterations for the long short portfolio }
  \item{eps}{ A small positive real value for the convergence criteria for the gross notional exposure }
}
\details{
  The algorithm uses the function \code{random.longshort} to generate long portfolios that have identical
  total long and short exposures equal to one half the given gross notional exposure \code{x.g}.
  The resultant portfolio \code{x.ls} is algebraically added to the benchmark portfolio \code{x.b}.
}
\value{
  An \eqn{n \times 1} numeric vector with the investment weights.
}
\references{ 
Jacobs, B. I. and K. N. Levy, 1997. The Long and Short of Long-Short Investing,
\emph{Journal of Investing}, Spring 1997, 73-86.

Jacobs, B. I., K. N. Levy and H. M. Markowitz, 2005. Portfolio Optimization with
Factors, Scenarios and Realist Short Positions, \emph{Operations Research}, July/August 2005,
586-599.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{ 
  \code{\link{random.longshort}}
}
\examples{
###
### the benchmark portfolios consists of 30 equally weighted investments
###
x.b <- rep( 1, 30 ) / 30
###
### the gross notional exposure of the long short portfolio is a benchmark weight
###
x.g <- 1 / 30
###
### generate 100 active portfolios with 30 non zero positions in the long short portolios
###
x <- random.active( x.b, x.g )
###
### generate 100 active portfolios with 10 non zero positions in the long short portolios
###
y <- random.active( x.b, x.g, 10 )
}
\keyword{ math }
