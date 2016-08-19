\name{set.segments}
\alias{set.segments}
\title{ Set segment weights from portfolios }
\description{
  This function assigns the given investment weights to larget portfolios
  using the investment indices in the segments
}
\usage{
set.segments(portfolios, n, segments)
}
\arguments{
  \item{portfolios}{ A vector or matrix of investment weights for the segments }
  \item{n}{ A positive integer value for the number of investments in the larger portfolio }
  \item{segments}{ A vector or list of vectors that defines the segment investments }
}
\details{
  A private function \code{vector.set.segments} is used to take weights in a given portfolio
  vector and assign them to a larger vector using the collapsed investment index vector.  If
  the \code{portfolios} argument is a matrix, then the R function \code{apply} is used to
  perform this task for each row vector.
}
\value{
  A vector or matrix.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{
  \code{\link{collapse.segments}}
}
\examples{
###
### simulate 300 long only portfolios with 30 investments
###
portfolios <- rlongonly( 300, 30 )
###
### define six segments with five investments in each
###
segment1 <- 1:5
segment2 <- 11:15
segment3 <- 21:25
segment4 <- 31:35
segment5 <- 41:45
segment6 <- 51:55
segments <- list( segment1, segment2, segment3, segment4, segment5, segment6 )
newPortfolios <- set.segments( portfolios, 60, segments )
}
\keyword{ math }
