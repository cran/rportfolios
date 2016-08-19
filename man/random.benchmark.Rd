\name{random.benchmark}
\alias{random.benchmark}
\title{ Random Naive Benchmark Portfolios }
\description{
  This function generates a vector of investment weights for a benchmark portfolio
  where the weights are non-negative and the sume of the weights is a given total.
  The weights are naively derived from an i.i.d. sample of positively truncated 
  random variables.
}
\usage{
random.benchmark(n = 2, k = n, segments = NULL, x.t = 1, 
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
  If the \code{segments} argument is a NULL value, then the benchmark has full cardinality,
  \eqn{ k = n}, or partial cardinality, \eqn{ k < n}.  In the case of partial cardinality,
  an investment segment is defined by a simple random sample without replacement of
  \eqn{k} investment indices from the \eqn{n} investments.  When the \code{segments} argument is not 
  NULL, the investment segment is constructed from the argument.  The investment segment
  is represented by the set \eqn{A} with cardinality \eqn{k}.  If argument \code{k}
  and \code{segments} are not specified, then then \eqn{A = \left\{ {i|1 \le i \le n} \right\}}
  For the \eqn{k} non-zero investment exposures, a random sample of size \eqn{k} is
  drawn from the truncated random variable, \eqn{S_i \quad i \in A}.  The non-zero investment exposures
  are given by \eqn{x_i  = {{S_i } \mathord{\left/
 {\vphantom {{S_i } {\sum\limits_{j \in A} {S_j ,} \;i \in A}}} \right.
 \kern-\nulldelimiterspace} {\sum\limits_{j \in A} {S_j ,} \;i \in A}}}.
 
  Currently, there are twelve truncated distributions available.  They are the uniforn (the default),
  beta, exponential, Frechet, gamma, generalized extreme value (gev), generalized Pareto (gpd),
  Gumbel, log normal, logistic, normal and Weibull distributions.  Random samples are truncated
  to the positive half of the real line.
}
\value{
  A numeric \eqn{n \times 1} vector of investment exposures.
}
\references{
Qian, E. E., R. H. Hua and E. H. Sorensen, 2007.
\emph{Quantitative Equity Portfolio Management}, Chapman \& Hall,
London, UK.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\seealso{
 \code{\link{random.benchmark.test}}
}
\examples{
###
### long only portfolio of 30 investments with 30 non-zero positions
### the margins of the truncated random variables are uniform
###
p.1 <- random.benchmark( 30 )
###
### long only portfolio of 30 investments with 10 non-zero positions
### the margins of the truncated random variables are uniform
###
p.2 <- random.benchmark( 30, 10 )
###
### long only portfolio of 30 investments with 30 non-zero positions
### the margins of the truncated random variables are log normal
### with zero log mean and unit log standard deviation
###
p.3 <- random.benchmark( 30, margins="lnorm", meanlog=0, sdlog=1 )
###
### long only portfolio of 30 investments with 10 non-zero positions
### the margins of the truncated random variables are log normal
### with zero log mean and unit log standard deviation
###
p.4 <- random.benchmark( 30, 10, margins="lnorm", meanlog=0, sdlog=1 )
}
\keyword{ math }
