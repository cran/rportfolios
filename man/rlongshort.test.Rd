\name{rlongshort.test}
\alias{rlongshort.test}
\title{ Generate random long short portfolios }
\description{
  This function generates m random long short portfolios with
  n investments that satisfy the given gross and net notional exposure requirements.
  There are k non-zero positions in each portfolio.
  The function is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
rlongshort.test(m, n = 2, k = n, x.t.long = 1, x.t.short = x.t.long, 
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
\details{
  The function executes the function \code{random.longshort.test} using the R function
  \code{lapply}.  The result which is a list contains the investment weight vectors and number
  of iterations.  Thse data are stored in a matrix of investment weights and a vector
  of iterations.  These arrays are returned as a list.  Gross notional exposure for each portfolio
  is \code{x.t.long + x.t.short} and net notional exposure is \code{x.t.long - x.t.short}.
  The argument \code{eps} is the tolerance applied towards the the gross notional exposure of each
  portfolio.
}
\value{
  A list with two named components.
  \item{xmatrix }{A numerical \eqn{m \times n} matrix of investment weights}
  \item{iters }{An \eqn{m \times 1} integer vector for the number of iterations used 
  to obtain the investment weights}
}
\references{ 
Jacobs, B. I. and K. N. Levy, 1997. The Long and Short of Long-Short Investing,
\emph{Journal of Investing}, Spring 1997, 73-86.

Jacobs, B. I., K. N. Levy and H. M. Markowitz, 2005. Portfolio Optimization with
Factors, Scenarios and Realistic Short Positions, \emph{Operations Research}, July/August 2005,
586-599.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{random.longshort.test}}
}
\examples{
###
### 100 long short portfolios with 30 investments and 30 non-zero positions
###
x.result <- rlongshort.test( 100, 30 )
###
### 100 long short portfolios with 30 investments and 20 non-zero positions
###
y.result <- rlongshort.test( 100, 30, 20 )
}
\keyword{ math }
