\name{random.longshort.test}
\alias{random.longshort.test}
\title{ Random long short portfolio test }
\description{
  This function generates a vector of investment weights for a portfolio
  with a given net and gross notional exposure. There are k non-zero
  positions in the portfolio.  The function is used to 
  evaluate the performance of the portfolio generation algorithm.
}
\usage{
random.longshort.test(n = 2, k = n, x.t.long = 1, x.t.short = x.t.long, 
max.iter = 2000, eps = 0.001)
}
\arguments{
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non zero positions }
  \item{x.t.long}{ A positive real value for the sum of the long exposures  }
  \item{x.t.short}{ A positive real value for the sum of the absolute value of the short exposures }
  \item{max.iter}{ A positive integer value for the maximum iterations in the acceptance rejection method }
  \item{eps}{ A small positive real value for the convergence criteria for the gross notional exposure }
}
\details{
  The function uses the same portfolio generation method described in \code{random.longshort}.
}
\value{
  A list with two named components.
  \item{x }{An \eqn{n \times 1}  numerical vector of investment weights}
  \item{iter }{An integer value for the number of iterations used to obtain the investment weights}
}
\references{
Jacobs, B. I. and K. N. Levy, 1997. The Long and Short of Long-Short Investing,
\emph{Journal of Investing}, Spring 1997, 73-86.

Jacobs, B. I., K. N. Levy and H. M. Markowitz, 2005. Portfolio Optimization with
Factors, Scenarios and Realist SHort Positions, \emph{Operations Research}, July/August 2005,
586-599.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{ 
  \code{\link{random.longonly}},
  \code{\link{random.longshort}},
  \code{\link{random.shortonly}}
}
\examples{
###
### long short portfolio of 30 investments with 30 non-zero positions
###
x.result <- random.longshort.test( 30 )
###
### long short portfolio of 30 investments with 10 non-zero positions
###
x.result <- random.longshort.test( 30, 10 )
}
\keyword{ math }
