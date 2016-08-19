\name{rbenchmark.test}
\alias{rbenchmark.test}
\title{ Generate random naive benchmark portfolios }
\description{
  This function generates m random long only benchmark portfolios with
  n investments where the sume of the weights equals a given amount.
  The weights are naively derived from an i.i.d. sample of truncated random variables.
  This function is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
rbenchmark.test(m, n = 2, k = n, segments = NULL, x.t = 1, 
margins = c("unif", "beta", "exp", "frechet", 
"gamma", "gev", "gpd", "gumbel", "lnorm", "logis", "norm", 
"weibull"), ...)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios }
  \item{n}{ A positive integer for the number of investments in the portfolio }
  \item{k}{ A positive integer for the number of non-zero exposures or cardinality }
  \item{segments}{ A vector or list of vectors that defines the investment segments }
  \item{x.t}{ A positive real value for the sum of the investment exposures }
  \item{margins}{ A character value for the underlying distribution of the truncated
  random variable.  The default is a uniform distribution }
  \item{\dots}{ Other arguments passed to the random variate simulation function }
}
\details{
  The function executes the function \code{random.benchmark.test} using the R function
  \code{lapply}.  The result which is a list contains the investment weight vectors and number
  of iterations.  Thse data are stored in a matrix of investment weights and a vector
  of iterations.  These arrays are returned as a list.
}
\value{
  A list with two named components.
  \item{xmatrix }{A numerical \eqn{ m \times n} matrix of investment weights}
  \item{iters }{An integer \eqn{ m \times 1} vector for the number iterations used 
  to obtain the investment weights}
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{
  \code{\link{random.benchmark.test}}
}
\examples{
###
### 100 long only portfolios of 30 investments with 30 non-zero positions
### the margins of the truncated random variables are uniform
###
p.1.result <- rbenchmark.test( 100, 30 )
###
### 100 long only portfolios of 30 investments with 10 non-zero positions
### the margins of the truncated random variables are uniform
###
p.2.result <- rbenchmark.test( 100, 30, 10 )
###
### 100 long only portfolios of 30 investments with 30 non-zero positions
### the margins of the truncated random variables are log normal
### with zero log mean and unit log standard deviation
###
p.3.result <- rbenchmark.test( 100, 30, margins="lnorm", meanlog=0, sdlog=1 )
###
### 100 long only portfolios of 30 investments with 10 non-zero positions
### the margins of the truncated random variables are log norm
### with zero log mean and unit log standard deviation
###
p.4.result <- rbenchmark.test( 100, 30, 10, margins="lnorm", meanlog=0, sdlog=1 )
}
\keyword{ math }
