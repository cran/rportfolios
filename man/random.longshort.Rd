\name{random.longshort}
\alias{random.longshort}
\title{ Generate random long short porfolio }
\description{
  This function generates a vector of investment weights for a long short portfolio
  where the the gross notional exposure is x.t.long + x.t.short and
  the net notional exposure is x.t.long - x.t.short.  There are k non-zero positions
  in the portfolio.
}
\usage{
random.longshort(n = 2, k = n, segments = NULL, x.t.long = 1, x.t.short = x.t.long, 
max.iter = 2000, eps = 0.001)
}
\arguments{
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non zero weights }
  \item{segments}{ A vector or list of vectors that defines the portfolio segments }
  \item{x.t.long}{ A positive real value for the sum of the long exposures  }
  \item{x.t.short}{ A positive real value for the sum of the absolute value of the short exposures }
  \item{max.iter}{ A positive integer value for the maximum iterations in the acceptance rejection method }
  \item{eps}{ A small positive real value for the convergence criteria for the gross notional exposure }
}
\details{
  The function implements an algorithm in which the outer structure is the iterative
  acceptance rejection method.  Within each iteration, the R function \code{random.longonly}
  is used to construct a long only investment weight vector \code{x.long} where the sum of
  these weights is \code{x.t.long}.  The R function \code{random.shortonly} is used to construct
  a short only investment eight vector \code{random.short} such that the sum of the absolute
  value of these weights is \code{x.t.long}.  The sum of these two weight vectors, \code{x.longshort},
  satisfies the net notional requirement of the desired portfolio.  If the absolute value of 
  computed gross notiona exposure for \code{x.longshort} minus $x.t.long + x.t.short$ is less than
  the argument \code{eps}, then the desired portfolio is generated and result is returned.
  Otherwise, the process is repeated within the acceptance rejection loop until (1) the required
  portfolio is generated or {2} the iteration limit is exceeded.
}
\value{
  An \eqn{n \times 1} vector of investment weights for the long short portfolio.
}  
\references{ 
Jacobs, B. I. and K. N. Levy, 1997. The Long and Short of Long-Short Investing,
\emph{Journal of Investing}, Spring 1997, 73-86.

Jacobs, B. I., K. N. Levy and H. M. Markowitz, 2005. Portfolio Optimization with
Factors, Scenarios and Realist SHort Positions, \emph{Operations Research}, July/August 2005,
586-599.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{ 
  \code{\link{random.longonly}},
  \code{\link{random.shortonly}}
}
\examples{
###
### long short portfolio of 30 investments with 30 non-zero positions
###
x <- random.longshort( 30 )
###
### long short portfolio of 30 investments with 10 non-zero positions
###
y <- random.longshort( 30, 10 )
}
\keyword{ math }
