\name{rbenchmark}
\alias{rbenchmark}
\title{ Generate random naive benchmark portfolios }
\description{
  This function generates m random long only benchmark portfolios with
  n investments where the sume of the weights equals a given amount.
  The weights are naively derived from an i.i.d. sample of truncated random variables.
}
\usage{
rbenchmark(m, n = 2, k = n, segments = NULL, x.t = 1, 
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
  The function executes the function \code{random.benchmark} using the R function
  sapply.  The result returned is the transpose of the matrix generated in the previous
  step.
}
\value{
  A numeric \eqn{m \times n} matrix.  The rows are the portfolios and
  the columns are the investment weights for each portfolio
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{
  \code{\link{random.benchmark}}
}
\examples{
###
### 100 long only portfolios of 30 investments with 30 non-zero positions
### the margins of the truncated random variables are uniform
###
p.1.matrix <- rbenchmark( 100, 30 )
###
### 100 long only portfolios of 30 investments with 10 non-zero positions
### the margins of the truncated random variables are uniform
###
p.2.matrix <- rbenchmark( 100, 30, 10 )
###
### 100 long only portfolios of 30 investments with 30 non-zero positions
### the margins of the truncated random variables are log normal
### with zero log mean and unit log standard deviation
###
p.3.matrix <- rbenchmark( 100, 30, margins="lnorm", meanlog=0, sdlog=1 )
###
### 100 long only portfolios of 30 investments with 10 non-zero positions
### the margins of the truncated random variables are log norm
### with zero log mean and unit log standard deviation
###
p.4.matrix <- rbenchmark( 100, 30, 10, margins="lnorm", meanlog=0, sdlog=1 )
}
\keyword{ math }
