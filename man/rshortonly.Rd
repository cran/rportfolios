\name{rshortonly}
\alias{rshortonly}
\title{ Generate short only portfolios }
\description{
  This function generates m random short only portfolios with
  n investments with each investment absolute weight bounded in an interval
  and the sum of the absolute value of weights equals a given amount.
}
\usage{
rshortonly(m, n = 2, k = n, x.t = 1, x.l = 0, 
x.u = x.t, max.iter = 1000)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios }
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non zero weights }
  \item{x.t}{ A positive numeric value for the sum of the absolute value of investment weights }
  \item{x.l}{ A positive numeric value for the lower bound on the absolute value of investment weights }
  \item{x.u}{ A positive numeric value for the upper bound on the absolute value of investment weights }
  \item{max.iter}{ A positive integer value for the maximum iterations in the rejection method }
}
\details{
  The function executes the function \code{random.shortonly} using the R function
  sapply.  The result returned is the transpose of the matrix generated in the previous
  step.
}
\value{
  A numeric \eqn{imes n} matrix.  The rows are the portfolios and
  the columns are the investment weights for each portfolio
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{random.shortonly}}
}
\examples{
x.matrix <- rshortonly( 100, 30 )
y.matrix <- rshortonly( 100, 30, 10 )
}
\keyword{ math }
