\name{rshortonly.test}
\alias{rshortonly.test}
\title{ Generate random short only portfolios }
\description{
  This function generates m random short only portfolios with n investments where
  each investment absolute weight bounded in an interval and the sum of the absolute weights equals a given amount.  
  This function is used to test the algorithm that generates the random portfolios. The
  number of non zero positions in the portfolio is k. The function 
  is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
rshortonly.test(m, n = 2, k = n, x.t = 1, x.l = 0, 
x.u = x.t, max.iter = 1000)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios }
  \item{n}{ An integer value for the number of investments in the portfolio }
  \item{k}{ An integer value for the number of non zero weights }
  \item{x.t}{ Numeric value for the sum of the absolute value of the investment weights }
  \item{x.l}{ Numeric value for the lower bound on the absolute value of an investment weight }
  \item{x.u}{ Numeric value for the upper bound on the absolute value of an investment weight }
  \item{max.iter}{ An integer value for the maximum iteration in the acceptance rejection loop }
}
\details{
  The function executes the function \code{random.shortonly.test} using the R function
  \code{lapply}.  The result which is a list contains the investment weight vectors and number
  of iterations.  Thse data are stored in a matrix of investment weights and a vector
  of iterations.  These arrays are returned as a list.
}
\value{
  A list with two named components.
  \item{xmatrix }{An \eqn{m \times n} numerical matrix of investment weights}
  \item{iters }{An \eqn{m \times 1} integer vector for the number of iterations used 
  to obtain the investment weights}
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }

\seealso{
  \code{\link{random.longonly.test}}
}
\examples{
###
### generate 100 short only portfolios of 30 investments with 30 non zero positions
###
x.result <- rshortonly.test( 100, 30 )
###
### generate 100 short only portfolios of 30 investments with 10 non zero positions
###
x.result <- rshortonly.test( 100, 30, 10 )
}
\keyword{ math }
