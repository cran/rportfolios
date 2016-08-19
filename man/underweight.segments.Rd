\name{underweight.segments}
\alias{underweight.segments}
\title{ Underweight Active Investment Segment Exposures }
\description{
  This function underweight the investment exposures of the given portfolios
  in the given active investment segments by the proportion \eqn{x_u} of the total
  exposure in the active segment.
}
\usage{
underweight.segments(portfolios, segments, x.u)
}
\arguments{
  \item{portfolios}{ A numeric vector or matrix for the portfolio investment exposures }
  \item{segments}{ A vector or list of vectors that define the active investment segment }
  \item{x.u}{ A positive real value for the proportion of total active exposure
  allocated to the passive investment exposures  }
}
\details{
  if \eqn{ x_u = 0}, then the original portfolios are returned.  If \eqn{ x_u = 1},
  then the total exposure of the active segment is allocated
  to the passive investment segment of all the portfolios.  The private function
  \code{vector.underweight.segments} i performs the actual work and returns a vector.
  If \code{portfolios} is a matrix of investment weights, then the apply
  function is used with the private function to obtain a matrix of weights.
  The transpose of this matrix is returned.
}
\value{
  A vector of adjusted investment exposures for one portfolio or 
  a matrix for more than one portfolio.
}
\references{
Grinold, R. C. and R. H. Kahn, 1999.
\emph{Active Portfolio Management: Quantitative Approach for
Providing Superior Returns and Controlling Risk}, Second Edition,
McGraw-Hill, New York, NY.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{
  \code{\link{segment.complement}}
}
\examples{
onePortfolio <- random.longonly( 10 )
I <- list()
I[[1]] <- c( 1, 2, 3 )
I[[2]] <- c( 4, 5 )
I[[3]] <- c( 6, 7 )
I[[4]] <- c( 8, 9, 10 )
underweight.segments( onePortfolio, I[[1]], 0 )
underweight.segments( onePortfolio, I[[1]], .1 )
}
\keyword{ math }
