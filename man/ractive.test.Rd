\name{ractive.test}
\alias{ractive.test}
\title{ Generate random active portfolios }
\description{
  This function generates m random actively managed portfolios relative to
  a given benchmark portfolio. Each portfolio is the combination of a benchmark portfolio
  and a notional neutral long short portfolio with given gross notional exposure.
  The number of non zero positions in the long short portfolios is k.  The function is used
  to evaluate the computational performance of the portfolio generation algorithm.
}
\usage{
ractive.test(m, x.b, x.g, k = length(x.b), segments = NULL, max.iter = 2000, eps = 0.001)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios in the sample }
  \item{x.b}{ A numeric vector with the investment weights in the benchmark portfolio }
  \item{x.g}{ A positive numeric value for the gross notional exposure in the long short portfolio }
  \item{k}{ A positive integer value for the number of non zero positions in the long short portfolio }
  \item{segments}{ A vector or list of vectors that defines the portfolio segments }
  \item{max.iter}{ A positive integer value for the maximum iterations for the long short portfolio }
  \item{eps}{ A small positive real value for the convergence criteria for the gross notional exposure }
}
\details{
  The function executes the function \code{random.active.test} using the R function \code{lapply}.
  The result is a list containing the investment weight vector and the number of iterations.
  These data are stored in a matrix of investment weights and a vector of iterations.
  This list is returned.
}
\value{
  A list with two named components.
  \item{xmatrix }{An \eqn{m \times n} matrix of investment weights}
  \item{iters }{An \eqn{m \times 1} vector with the number of iterations used to obtain the portfolios}
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
  \code{\link{random.active.test}}
}
\examples{
###
### benchmark consists of 20 equally weighted investments
###
x.b <- rep( 1, 30 ) / 30
###
### the gross notional exposure of the long short portfolio is a benchmark weight
###
x.g <- 1 / 30
###
### generate 100 random active portfolios with 30 non zero positions in the long short portfolios
###
x.matrix <- ractive.test( 100, x.b, x.g )
###
### generate 100 random active portfolios with 10 non zero positions in the long short portfolios
###
y.matrix <- ractive.test( 100, x.b, x.g, 10 )
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ math }
