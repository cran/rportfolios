\name{portfolio.composite}
\alias{portfolio.composite}
\title{ Merge portfolios into a composite }
\description{
  This function merges a list of portfolios using a specified
  set of weights.  The components in the list can be single portfolio vectors
  a matrix of portfolios.
}
\usage{
portfolio.composite(portfolios, weights = NULL)
}
\arguments{
  \item{portfolios}{ A list of vectors or matrices corresponding to portfolios of investments }
  \item{weights}{ A numeric vector of weights for the components }
}
\details{
  The private function \code{vector.composite} is used to create a single portfolio
  from a list of portfolio weight vectors.  The private function \code{matrix.composite}
  generates the weighted composite matrix from a list of portfolio weight matrices.
}  
\value{
  A vector or matrix.
}  
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\examples{
segments <- list()
segments[[1]] <- c( 1, 2, 3 )
segments[[2]] <- c( 4, 5 )
segments[[3]] <- c( 6, 7 )
segments[[4]] <- c( 8, 9, 10 )
weights <- c( .3, .2, .2, .3 )
vectors <- list()
matrices <- list()
for ( i in 1:4 ) {
vectors[[i]] <- random.longonly( 10, segments=segments[[i]] )
matrices[[i]] <- rlongonly( 200, 10, segments=segments[[i]] )
}
combined.vectors <- portfolio.composite( vectors, weights )
combined.matrices <- portfolio.composite( matrices, weights )
}
\keyword{ math }
