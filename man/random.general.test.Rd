\name{random.general.test}
\alias{random.general.test}
\title{ Random general portfolio }
\description{
  This function generates a general random portfolio of n investments with k long and short positions.
  The probability that a non-zero investment weight is positive is p.  The maximum absolute
  exposure for any investment is x.u.  The default value is 1. The function is 
  used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
random.general.test(n = 2, k = n, segments = NULL, p = 0.5, x.u = 1)
}
\arguments{
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non-zero positions }
  \item{segments}{ A vector or list of vectors that defines the portfolio segments }
  \item{p}{ A positive numeric value for the probability that an investment weight is positive }
  \item{x.u}{ A positive numeric value for the maximum absolute exposure to an investment }
}
\details{
  If \eqn{ k < n} the function \code{random.general.test} is recursively called with n set equal to k
  to obtain a \eqn{k \times 1} vector of non-zero long and short weights.
  The R function \code{sample} is used to generate a simple random sample without replacement
  of k values from the integers \eqn{1,2, \ldots ,n}.  These are the subscripts into 
  an \eqn{n \times 1} zero vector to assign the k non-zero weights.  This vector is returned.

  If \eqn{ k = n }, the R function \code{rbinom} is used to generate a vector of plus and minus ones corresponding
  to the long and short positions.  The R function \code{runif} is used to generate uniformly
  distributed values between 0 and 1.  These are scaled by x.u and then multiplied by the signs.
  The sum of the investment weights is not restricted.
}
\value{
  A list with two named components.
  \item{x }{An \eqn{n \times 1}  numerical vector of investment weights}
  \item{iter }{An integer value for the number of iterations used to obtain the investment weights}
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\examples{
###
### long only portfolio of 30 investments with 30 non-zero positions
###
result.x.long <- random.general.test( 30, p=1.0 )
###
### long only portfolio of 30 investments with 10 non-zero positions
###
result.y.long <- random.general.test( 30, 10, p=1.0 )
###
### short only portfolio of 30 investments with 30 non-zero positions
###
result.x.short <- random.general.test( 30, p=0.0 )
###
### short only portfolio of 30 investments with 10 non-zero positions
###
result.y.short <- random.general.test( 30,10,  p=0.0 )
###
### long short portfolio of 30 investments with 30 non-zero positions
###
result.x.long.short <- random.general.test( 30, p=0.5 )
###
### long short portfolio of 30 investments with 10 non-zero positions
###
result.y.long.short <- random.general.test( 30, 10, p=0.5 )
###
### long bias portfolio of 30 investments with 30 non-zero positions
###
result.x.long.bias <- random.general.test( 30, p=0.7 )
###
### long bias portfolio of 30 investments with 10 non-zero positions
###
result.y.long.bias <- random.general.test( 30, 10, p=0.7 )
###
### short bias portfolio of 30 investments with 30 non-zero positions
###
result.x.short.bias <- random.general.test( 30, p=0.3 )
###
### short bias portfolio of 30 investments with 10 non-zero positions
###
result.y.short.bias <- random.general.test( 30, 10, p=0.3 )
}
\keyword{ math }
