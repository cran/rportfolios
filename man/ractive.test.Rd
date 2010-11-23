\name{ractive.test}
\alias{ractive.test}
\title{ Generate random active portfolios }
\description{
  This function generates m random actively managed portfolios relative to
  a given benchmark portfolio. Each portfolio is the combination of a benchmark portfolio
  and a notional neutral long short portfolio with given gross notional exposure.
  The number of non zero positions in the long short portfolios is k.
  The function is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
ractive.test(m, x.b, x.g, k = length(x.b),  max.iter = 2000, eps = 0.001)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios in the sample }
  \item{x.b}{ A numeric vector with the investment weights in the benchmark portfolio }
  \item{x.g}{ A positive numeric value for the gross notional exposure in the long short portfolios }
  \item{k}{ A positive integer value for the number of non zero positions in the long short portfolios }
  \item{max.iter}{ A positive integer value for the maximum iterations for the long short portfolios }
  \item{eps}{ A small positive real value for the convergence criteria for the gross notional exposure
  of the long short portfolios }
}
\details{
  The function executes the function \code{random.active.test} using the R function
  \code{lapply}.  The result which is a list contains the investment weight vectors and number
  of iterations.  Thse data are stored in a matrix of investment weights and a vector
  of iterations.  These arrays are returned as a list.
}
\value{
  A list with two named components.
  \item{xmatrix }{A numerical \eqn{m \times n} matrix of investment weights}
  \item{iters }{An \eqn{m \times 1} integer vector for the number of iterations 
  used to obtain the investment weights for each portfolio}
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
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{random.active.test}}
}
\examples{
###
### the benchmark portfolios consists of 30 equally weighted investments
###
x.b <- rep( 1, 30 ) / 30
###
### the gross notional exposure of the long short portfolio is a benchmark weight
###
x.g <- 1/30
###
### generate 100 active portfolios with 30 non zero positions in the long short portolios
###
x.result <- ractive.test( 100, x.b, x.g )
###
### generate 100 active portfolios with 10 non zero positions in the long short portfolios
###
y.result <- ractive.test( 100, x.b, x.g, 10 )
}
\keyword{ math }
