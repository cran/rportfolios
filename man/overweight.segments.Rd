\name{overweight.segments}
\alias{overweight.segments}
\title{ Overweight Active Investment Segment Exposures }
\description{
  This function overweights the investment exposures of the given portfolios
  in the given investment segments by the proportion \eqn{x_o} of the total
  exposure in the segment complement.
}
\usage{
overweight.segments(portfolios, segments, x.o)
}
\arguments{
  \item{portfolios}{  A vector or matrix that defines the portfolios }
  \item{segments}{ A vector or list of vectors that defines the investment segments }
  \item{x.o}{ A positive real value for the proportion of total passive exposure
  allocated to the active exposures  }
}
\details{
  if \eqn{ x_o = 0}, then the original portfolios are returned.  If \eqn{ x_o = 1},
  then the total exposure of the segment complement, or passive segment, is allocated
  to the active investment segment of all the portfolios.  The private function
  \code{vector.overweight.segments} does the actual work.  If the argument \code{portfolios}
  is a matrix, then the apply function is used with private function to obtain a matrix of
  weights.  The transpose of this matrix is returned.
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
overweight.segments( onePortfolio, I[[1]], 0 )
overweight.segments( onePortfolio, I[[1]], .1 )
}
\keyword{ math }
