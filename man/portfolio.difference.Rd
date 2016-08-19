\name{portfolio.difference}
\alias{portfolio.difference}
\title{ Portfolio Difference Measure }
\description{
  This function computes a measure of the difference between one or
  more portfolios and a benchmark portfolio.
}
\usage{
portfolio.difference(portfolios, x.b, method = c("relative", "absolute"))
}
\arguments{
  \item{portfolios}{ A numeric vector or matrix that defines the portfolio exposures }
  \item{x.b}{ A numeric vector that defines the benchmark exposures }
  \item{method}{ A character value that defines the difference measure }
}
\details{
  The absolute deviation between a portfolio \eqn{\bf{x}} and a benchmark \eqn{\bf{x}_b}
  is denoted by \eqn{{D_a}\left({{\bf{x}},{{\bf{x}}_b}} \right)} and is computed as
  \eqn{{D_a}\left( {{\bf{x}},{{\bf{x}}_b}} \right) =
  \frac{1}{2}\sum\limits_{i = 1}^n {\left| {{x_i} - {x_{b,i}}}
  \right|}}.
  
  The relative deviation between a portfolio and a benchmark is denoted by
  \eqn{{D_r}\left( {{\bf{x}},{{\bf{x}}_b}} \right)} and is computed as
  \eqn{{D_r}\left( {{\bf{x}},{{\bf{x}}_b}} \right) =
  \frac{1}{n}\sum\limits_{i = 1}^n {\frac{{\left| {{x_i} -
  {x_{b,i}}} \right|}}{{{x_i} + {x_{b,i}}}}}}.
  
  The private function \code{vector.difference} performs the actual calculation
  of the difference based on the given method.
}
\value{
  A single numeric measure for one portfolio or a numeric vector for a
  collection of portfolios
}
\references{
  Worthington, A. C., 2009. Household Asset Portfolio Diversification: Evidence from the
  Household, Income and Labour Dynamics in Australia (Hilda) Survey, Working Paper,
  Available at SSRN: http://ssrn.com/abstract=1421567.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\examples{
onePortfolio <- random.longonly( 100, 75 )
aBenchmark <- rep( 0.01, 100 )
portfolio.difference( onePortfolio, aBenchmark, method="absolute" )
portfolio.difference( onePortfolio, aBenchmark, method="relative" )
}
\keyword{ math }
