\name{segment.complement}
\alias{segment.complement}
\title{ Complement of Investment Segments }
\description{
  This function returns a vector of investments that are in a portfolio
  with n investments but not in the given investment segments
}
\usage{
segment.complement(n, segments)
}
\arguments{
  \item{n}{ A positive integer for the number of investments in a portfolio }
  \item{segments}{ A vector or list of vectors that defines the investment segments }
}
\details{
  If the investments in the given segment are for the entire portfolio, a NULL value
  is returned.  If the segments argument is NULL, then the entire portfolio of n
  investments is returned.
}
\value{
  A vector of investments or a NULL value.
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
    segment.complement( 10, I )
    segment.complement( 10, NULL )
    segment.complement( 10, I[[1]] )
    segment.complement( 10, I[[2]] )
    segment.complement( 10, I[[3]] )
    segment.complement( 10, I[[4]] )
}
\keyword{ math }
