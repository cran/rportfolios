\name{rlongonly}
\alias{rlongonly}
\title{ Generate random long only portfolios }
\description{
  This function generates m random long only portfolios with
  n investments with each investment weight bounded in an interval
  and the sum of the weights equals a given amount.  The number
  of non-zero positions is k.
}
\usage{
rlongonly(m, n = 2, k = n, segments = NULL, x.t = 1, x.l = 0, 
x.u = x.t, max.iter = 1000)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios }
  \item{n}{ A positive integer value for the number of investments in each portfolio }
  \item{k}{ A positive integer value for the number of non zero weights }
  \item{segments}{ A vector or list of vectors that defines the portfolio segments }
  \item{x.t}{ A positive numeric value for the sum of investment weights }
  \item{x.l}{ A positive numeric value for the lower bound of an investment weight }
  \item{x.u}{ A positive numeric value for the upper bound of an investment weight }
  \item{max.iter}{ A positive integer value for the number of rejection iterations }
}
\details{
  The function executes the function \code{random.longonly} using the R function
  sapply.  The result returned is the transpose of the matrix generated in the previous
  step.
}
\value{
  A numeric \eqn{m \times n} matrix.  The rows are the portfolios and
  the columns are the investment weights for each portfolio
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{
  \code{\link{random.longonly}}
}
\examples{
###
### 100 long only portfolios of 30 investments with 30 non-zero positions
###
x.matrix <- rlongonly( 100, 30 )
###
### 100 long only portfolios of 30 investments with 10 non-zero positions
###
y.matrix <- rlongonly( 100, 30, 10 )
}
\keyword{ math }
