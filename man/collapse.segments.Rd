\name{collapse.segments}
\alias{collapse.segments}
\title{ Collapse a list or vectors of portfolio segments }
\description{
  This function returns a vector of investment indices from the
  given segments vector or list of vectors.
}
\usage{
collapse.segments(segments)
}
\arguments{
  \item{segments}{ A vector or list of vectors that defint the portfolio segments }
}
\value{
  A vector of investment indices.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\examples{
###
### define the segments
###
    I <- list()
    I[[1]] <- c( 1, 2, 3 )
    I[[2]] <- c( 4, 5 )
    I[[3]] <- c( 6, 7 )
    I[[4]] <- c( 8, 9, 10 )
    collapse.segments( I )
    collapse.segments( I[[1]] )
    collapse.segments( I[[2]] )
    collapse.segments( I[[3]] )
    collapse.segments( I[[4]] )
}
\keyword{ math }
