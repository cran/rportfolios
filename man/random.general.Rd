\name{random.general}
\alias{random.general}
\title{ Random general portfolio }
\description{
  This function generates a general random portfolio of n investments with k long or short positiobs,
  The probability that a a non-zero investment weight is positive is p.  The maximum absolute
  exposure for any investment is x.u.  The default value is \eqn{1 / k}.
}
\usage{
random.general(n = 2, k=n, p = 0.5, x.u = 1/k)
}
\arguments{
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non-zero positions }
  \item{p}{ A positive numeric value for the probability that an investment weight is positive }
  \item{x.u}{ A positive numeric value for the maximum absolute exposure to an investment }
}
\details{
  If \eqn{ k < n} the function \code{random.general} is recursively called with n set equal to k
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
  An \eqn{n \times 1} numeric vector of investment weights for the equal weighted portfolio.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
###
### long only portfolio of 30 investments with 30 non zero positions
###
x.long <- random.general( 30, p=1.0 )
###
### long only portfolio of 30 investments with 10 non zero positions
###
y.long <- random.general( 30, 10, p=1.0 )
###
### short only portfolio of 30 investments with 30 non zero positions
###
x.short <- random.general( 30, p=0.0 )
###
### short only portfolio of 30 investments with 10 non zero positions
###
y.short <- random.general( 30, 10, p=1.0 )
###
### long short portfolio of 30 investments with 30 non zero positions
###
x.long.short <- random.general( 30, p=0.5 )
###
### long short portfolio of 30 investments with 10 non zero positions
###
y.long.short <- random.general( 30, 10, p=0.5 )
###
### long bias portfolio of 30 investments with 30 non zero positions
###
x.long.bias <- random.general( 30, p=0.7 )
###
### long bias portfolio of 30 investments with 10 non zero positions
###
y.long.bias <- random.general( 30, 10, p=0.7 )
###
### short bias portfolio of 30 investments with 30 non zero positions
###
x.short.bias <- random.general( 30, p=0.3 )
###
### short bias portfolio of 30 investments with 10 non zero positions
###
y.short.bias <- random.general( 30, 10, p=0.3 )
}
\keyword{ math }
