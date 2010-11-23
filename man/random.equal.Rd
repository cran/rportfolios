\name{random.equal}
\alias{random.equal}
\title{ Random equal weighted portfolios }
\description{
  This function generates a random portfolio of n investments in which there are only k
  positive equal weights.  The weights sum to the given value \eqn{x_t}.
}
\usage{
random.equal(n = 2, k, x.t=1)
}
\arguments{
  \item{n}{ A positive integer for the number of investments in the portfolio }
  \item{k}{ A positive integer for the number of investments with positive equal weights }
  \item{x.t}{ A positive numeric value for the sum of weights }
}
\details{
  The R function \code{sample} is used to generate a simple random sample without replacement
  of k values from the integers \eqn{1,2, \ldots ,n}.  These are the subscripts into 
  an \eqn{n \times 1} zero vector to assign
  the equal weight \eqn{x_t / k}.
}
\value{
  An \eqn{n \times 1} numeric vector of investment weights for the equal weighted portfolio.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
x <- random.equal( 30, 5 )
}
\keyword{ math }
