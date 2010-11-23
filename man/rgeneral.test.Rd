\name{rgeneral.test}
\alias{rgeneral.test}
\title{  Generate random general portfolios }
\description{
  This function generates m random general portfolios with n investments each
  that can have k positive or negative.  The probability that a given investment weight is
  positive is p.  The maximum absolute exposure is x.u which has \eqn{1 / k} as the default
  The function is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
rgeneral.test(m, n = 2, k = n, p = 0.5, x.u = 1/k)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios }
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non-zero long and short positions }
  \item{p}{ A positive numeric value for the probability that a position is positive }
  \item{x.u}{ A positive numeric value for the maximum absolute exposure to an investment }
}
\details{
  The function executes the function \code{random.general.test} using the R function
  \code{lapply}.  The result which is a list contains the investment weight vectors and number
  of iterations.  Thse data are stored in a matrix of investment weights and a vector
  of iterations.  These arrays are returned as a list.
}
\value{
  A list with two named components.
  \item{xmatrix }{An \eqn{m \times n}  numerical matrix of investment weights}
  \item{iters }{An \eqn{m \times 1} integer vector for the number of iterations 
  used to obtain the investment weights}
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{random.general.test}}
}
\examples{
###
### 100 long only portfolios of 30 investments
###
x.long <- rgeneral.test( 100, 30, p=1.0 )
y.long <- rgeneral.test( 100, 30, 10, p=1.0 )
###
### 100 short only portfolios of 30 investments
###
x.short <- rgeneral.test( 100, 30, p=0.0 )
y.short <- rgeneral.test( 100, 30, 10, p=0.0 )
###
### 100 long short portfolios of 30 investments
###
x.long.short <- rgeneral.test( 100, 30, p=0.5 )
y.long.short <- rgeneral.test( 100, 30, 10, p=0.5 )
###
### 100 long bias portfolios of 30 investments
###
x.long.bias <- rgeneral.test( 100, 30, p=0.7 )
y.long.bias <- rgeneral.test( 100, 30, 10, p=0.7 )
###
### 100 short bias portfolios of 30 investments
###
x.short.bias <- rgeneral.test( 100, 30, p=0.3 )
y.short.bias <- rgeneral.test( 100, 30, 10, p=0.3 )
}
\keyword{ math }
