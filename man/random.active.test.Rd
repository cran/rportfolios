\name{random.active.test}
\alias{random.active.test}
\title{ Random actively managed portfolio }
\description{
  This function generates an actively managed random portfolio relative to
  a given benchmark portfolio.  The active portfolio is the sum of the benchmark portfolio
  and a notional neutral long short portfolio with given gross notional exposure.
  The number of non zero positions in the long short portfolio is k.
  The function is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
random.active.test(x.b, x.g, k = length(x.b), segments = NULL, max.iter = 2000, 
eps = 0.001)
}
\arguments{
  \item{x.b}{ A numeric vector with the investment weights in the benchmark portfolio }
  \item{x.g}{ A positive numeric value for the gross notional exposure in the long short portfolio }
  \item{k}{ A positive integer value for the number of non zero weights in the long short portfolio }
  \item{segments}{ A vector or list of vectors that defines the portfolio segments }
  \item{max.iter}{ A positive integer value for the maximum iterations for generating the long short portfolio }
  \item{eps}{ A small positive real value for the convergence criteria for the gross notional exposure }
}
\details{
  The algorithm uses the function \code{random.longshort.test} to generate long portfolios that have identical
  total long and short exposures equal to one half the given gross notional exposure \code{x.g}.
  The resultant portfolio \code{x.ls} is algebraically added to the benchmark portfolio \code{x.b}.
}
\value{
  A list with two named components.
  \item{x }{An \eqn{n \times 1}  numerical vector of investment weights}
  \item{iter }{An integer value for the number of iterations used to obtain the investment weights}
}
\references{ 
Grinold, R. C. and R. H. Kahn, 1999. \emph{Active Portfolio Management: Quantitative
Approach for Providing Superior Returns and Controlling Risk}, Second Edition, McGraw-Hill,
New York, NY.

Qian, E. E., R. H. Hua and E. H. Sorensen, 2007. \emph{Quantitative Equity Portfolio Management},
Chapman \& Hall, London, UK.

Scherer, B., 2007. \emph{Portfolio Construction and Risk Budgeting}, Third Edition,  Risk Books,
London, UK.

}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{ 
  \code{\link{random.longshort}}
}
\examples{
###
### benchmark consists of 20 equally weighted investments
###
x.b <- rep( 1, 30 ) / 30
###
### gross notion exposure is one of the investment weights
###
x.g <- 1 / 30
###
### generate 100 active portfolios with 30 non zero positions in the long short portfolio
###
x.result <- random.active.test( x.b, x.g )
###
### generate 100 active portfolios with 10 non zero positions in the long short portfolio
###
y.result <- random.active.test( x.b, x.g, 10 )
}
\keyword{ math }
