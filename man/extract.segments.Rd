\name{extract.segments}
\alias{extract.segments}
\title{ Extract Investment Segment Exposures }
\description{
  This function extracts the investment exposures from one or more
  portfolios for the specified investment segments.
}
\usage{
extract.segments(portfolios, segments, collapse = FALSE)
}
\arguments{
  \item{portfolios}{ A vector or matrix that defines the portfolios }
  \item{segments}{ A vector or list of vectors that defines the investment segments }
  \item{collapse}{ A logical value.  If TRUE, only the investment segment exposures are returned }
}
\details{
  If the \code{collapse} argument is FALSE, the segment complement exposures are zero and
  the investment segment exposures are taken from the portfolios.  If the \code{collapse}
  argument is TRUE, then only the investment segment exposures are returned.  The private function
  \code{vector.extract.segments} is used to perform the extraction.  For matrices of investment
  weights, the apply function is used with \code{vector.extract.segments}  to obtain a matrix of
  extracted segment weights.  The transpose of this matrix is returned.
}
\value{
  A vector for one portfolio or a matrix for multiple portfolios.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\examples{
onePortfolio <- random.longonly( 10 )
I <- list()
I[[1]] <- c( 1, 2, 3 )
I[[2]] <- c( 4, 5 )
I[[3]] <- c( 6, 7 )
I[[4]] <- c( 8, 9, 10 )
extract.segments( onePortfolio, I[[1]], FALSE )
extract.segments( onePortfolio, I[[1]], TRUE )
}
\keyword{ math }
