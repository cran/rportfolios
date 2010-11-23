\name{rlongshort}
\alias{rlongshort}
\title{ Generate long short portfolios }
\description{
  This function generates m random long short portfolios with
  n investments with the given gross and net notional exposure requirements.
  There are k non-zero positions in the portfolio.
}
\usage{
rlongshort(m, n = 2, k = n, x.t.long = 1, x.t.short = x.t.long, 
max.iter = 2000, eps = 0.001)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios generated }
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non zero positions }
  \item{x.t.long}{ A positive real value for the sum of the long exposures  }
  \item{x.t.short}{ A positive real value for the sum of the absolute value of the short exposures }
  \item{max.iter}{ A positive integer value for the maximum iterations in the acceptance rejection method }
  \item{eps}{ A small positive real value for the convergence criteria for the gross notional exposure }
}
\value{
  An \eqn{m \times n} numeric matrix of investment weights for the long short portfolios
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
  \code{\link{random.longshort}}
}
\examples{
###
### 100 portfolios of 30 investments with 30 non-zero positions
###
x.matrix <- rlongshort( 100, 30 )
###
### 100 portfolios of 30 investments with 10 non-zero positions
###
y.matrix <- rlongshort( 100, 30, 20 )
}
\keyword{ math }
