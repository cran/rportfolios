\name{random.shortonly.test}
\alias{random.shortonly.test}
\title{ Random short only portfolio }
\description{
  This function generates a vector of investment weights for a portfolio
  where the weights are non-positive, absolute weights do not exceed a given upper and
  and the sum of the absolute weights is a given total.  The number of non zero
  positions in the portfolio is k. The function is used
  to evaluate the performance of the portfolio generation algorithm.
}
\usage{
random.shortonly.test(n = 2, k = n, x.t = 1, x.l = 0, 
x.u = x.t, max.iter = 1000)
}
\arguments{
  \item{n}{ An integer value for the number of investments in the portfolio }
  \item{k}{ An integer value for the number of non zero weights }
  \item{x.t}{ Numeric value for the sum of the absolute value of the investment weights }
  \item{x.l}{ Numeric value for the lower bound on the absolute value of an investment weight }
  \item{x.u}{ Numeric value for the upper bound on the absolute value of an investment weight }
  \item{max.iter}{ An integer value for the maximum iteration in the acceptance rejection loop }
}
\details{
  The function uses \code{random.longonly.test} to generate a long only portfolio in test mode.
  The component x compute is used to define the short portfolio.  The short portfolio together
  with the component iter, the number of iterations used to construct the long only portfolio,
  are stored in a list of named components.
}
\value{
  A list with two named components
  \item{x }{An \eqn{n \times 1}  numerical vector of investment weights}
  \item{iter }{An integer value for the number of iterations used to obtain the investment weights}
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
 \code{\link{random.shortonly}}
}
\examples{
###
### generate a short only portfolio of 30 investments with 30 non-zero positions
###
x.result <- random.shortonly.test( 30 )
###
### generate a short only portfolio of 30 investments with 10 non-zero positions
###
y.result <- random.shortonly.test( 30, 10 )
}
\keyword{ math }
