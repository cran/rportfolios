\name{random.shortonly}
\alias{random.shortonly}
\title{ Random short only portfolio }
\description{
  This function generates a vector of investment weights for a portfolio
  where the weights are non-positive, absolute weights do not exceed a given upper and
  and the sum of the absolute weights weights is a given total. The number of non zero
  positions in the portfolio is k.
}
\usage{
random.shortonly(n = 2, k = n, x.t = 1, x.l = 0, 
x.u = x.t, max.iter = 1000)
}
\arguments{
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non zero weights }
  \item{x.t}{ A positive numeric value for the sum of the absolute value of investment weights }
  \item{x.l}{ A positive numeric value for the lower bound on the absolute value of investment weights }
  \item{x.u}{ A positive numeric value for the upper bound on the absolute value of investment weights }
  \item{max.iter}{ A positive integer value for the maximum iterations in the rejection method }
}
\details{
  The function \code{random.longonly} is used to generate a long only portfolio that satisfies
  the lower bound, upper bound and sum of weight conditions.  The value returned is a vector
  with the opposite signs.
}
\value{
  An \eqn{n \times 1} numeric vector of investment weights for the short only portfolio.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{ 
  \code{\link{random.longonly}}
}
\examples{
###
### generate short only portfolio of 30 investments with 30 non-zero positions
###
x <- random.shortonly( 30 )
###
### generate short only portfolio of 30 investments with 10 non-zero positions
###
y <- random.shortonly( 30, 10 )
}
\keyword{ math }
