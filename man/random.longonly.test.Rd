\name{random.longonly.test}
\alias{random.longonly.test}
\title{Random long only portfolio test}
\description{
  This function generates a vector of investment weights for a portfolio
  where the weights are non-negative. Each investment weight is within specified
  lower and upper bounds.  Furthermore, the sum of the weights is a given total.
  The number of non-zero positions in the portfolio is k.
  The function is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
random.longonly.test(n = 2, k=n, x.t = 1, x.l = 0, 
x.u = x.t, max.iter = 1000)
}
\arguments{
  \item{n}{ An integer value for the number of investments in the portfolio }
  \item{k}{ An integer value for the number of non zero weights }
  \item{x.t}{ Numeric value for the sum of the investment weights }
  \item{x.l}{ Numeric value for the lower bound on an investment weight }
  \item{x.u}{ Numeric value for the upper bound on an investment weight }
  \item{max.iter}{ An integer value for the maximum iteration in the acceptance rejection loop }
}
\details{
  See function \code{random.longonly} for the details of how this function works.  This function
  is used primarily to evaluate the algorithm
}
\value{
  A list with two named components.
  \item{x }{An \eqn{m \times n}  numerical vector of investment weights}
  \item{iter }{An integer value for the number of iterations used to obtain the investment weights}
}
\references{
Cheng, R. C. H., 1977. The Generation of Gamma Variables with Non-integral Sape Parameter,
\emph{Journal of the Royal Statistical Society}, Series C (Applied Statistics), 26(1), 71.

Marsaglia, G. and T. A. Bray, 1964. A Convenient method for generating normal variables, \emph{SIAM Review},
6(3), July 1964, 260-264.

Ross, S. M. (2006). \emph{Simulation}, Fourth Edition, Academic Press, New York NY.

}
\author{Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
 \code{\link{random.longonly}}
}
\examples{
###
### long only portfolio of 30 investments with 30 non-zero positions
###
x.result <- random.longonly.test( 30 )
###
### long only portfolio of 30 investments with 10 non-zero positions
###
y.result <- random.longonly.test( 30, 10 )
}
\keyword{ math }
