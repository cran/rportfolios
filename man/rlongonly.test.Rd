\name{rlongonly.test}
\alias{rlongonly.test}
\title{ Generate random long only portfolios }
\description{
  This function generates m random long only portfolios with
  n investments with each investment weight bounded in an interval
  and the sum of the weights equals a given amount. The number
  of non-zero positions is k. This function is used to test
  the algorithm that generates the random portfolios.
}
\usage{
rlongonly.test(m, n = 2, k = n, segments = NULL, x.t = 1, x.l = 0, 
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
  The function executes the function \code{random.longonly.test} using the R function
  \code{lapply}.  The result which is a list contains the investment weight vectors and number
  of iterations.  Thse data are stored in a matrix of investment weights and a vector
  of iterations.  These arrays are returned as a list.
}
\value{
  A list with two named components.
  \item{xmatrix }{A numerical \eqn{ m \times n} matrix of investment weights}
  \item{iters }{An integer \eqn{ m \times 1} vector for the number iterations used 
  to obtain the investment weights}
}
\references{
Cheng, R. C. H., 1977. The Generation of Gamma Variables with Non-integral Shape Parameter,
\emph{Journal of the Royal Statistical Society}, Series C (Applied Statistics), 26(1), 71.

Marsaglia, G. and T. A. Bray, 1964. A convenient method for generating normal variables, \emph{SIAM Review},
6(3), July 1964, 260-264.

Ross, S. M. (2006). \emph{Simulation}, Fourth Edition, Academic Press, New York NY.

}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{
  \code{\link{random.longonly.test}}
}
\examples{
###
### generate 100 long only portfolios with 30 investments and 30 non-zero positions
###
x.result <- rlongonly.test( 100, 30 )
###
### generate 100 long only portfolios with 30 investments and 10 non-zero positions
###
y.result <- rlongonly.test( 100, 30, 10 )
}
\keyword{ math }
