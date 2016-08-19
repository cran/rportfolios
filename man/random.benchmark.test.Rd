\name{random.benchmark.test}
\alias{random.benchmark.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Random Naive Benchmark Portfolio }
\description{
  This function generates a vector of investment weights for a benchmark portfolio
  where the weights are non-negative and the sume of the weights is a given total.
  The weights are naively derived from an i.i.d. sample of truncated random variables.
  This function is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
random.benchmark.test(n = 2, k = n, segments = NULL, x.t = 1, 
margins = c("unif", "beta", "exp", "frechet", 
"gamma", "gev", "gpd", "gumbel", "lnorm", "logis", "norm", 
"weibull"), ...)
}
\arguments{
  \item{n}{ A positive integer for the number of investments in the portfolio }
  \item{k}{ A positive integer for the number of non-zero exposures or cardinality }
  \item{segments}{ A vector or list of vectors that defines the investment segments }
  \item{x.t}{ A positive real value for the sum of the investment exposures }
  \item{margins}{ A character value for the underlying distribution of the truncated
  random variable.  The default is a uniform distribution }
  \item{\dots}{ Other arguments passed to the random variate simulation function }
}
\details{
  The details are described in the function \code{random.benchmark}.
}
\value{
  A list with two named components.
  \item{x }{An \eqn{m \times n}  numerical vector of investment weights}
  \item{iter }{An integer value for the number of iterations used to obtain the investment weights}
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{
 \code{\link{random.benchmark}}
}
\examples{
###
### long only portfolio of 30 investments with 30 non-zero positions
### the margins of the truncated random variables are uniform
###
p.1 <- random.benchmark.test( 30 )
###
### long only portfolio of 30 investments with 10 non-zero positions
### the margins of the truncated random variables are uniform
###
p.2 <- random.benchmark.test( 30, 10 )
###
### long only portfolio of 30 investments with 30 non-zero positions
### the margins of the truncated random variables are log normal
### with zero log mean and unit log standard deviation
###
p.3 <- random.benchmark.test( 30, margins="lnorm", meanlog=0, sdlog=1 )
###
### long only portfolio of 30 investments with 10 non-zero positions
### the margins of the truncated random variables are log normal
### with zero log mean and unit log standard deviation
###
p.4 <- random.benchmark.test( 30, 10, margins="lnorm", meanlog=0, sdlog=1 )
}
\keyword{ math }
