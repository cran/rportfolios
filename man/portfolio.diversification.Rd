\name{portfolio.diversification}
\alias{portfolio.diversification}
\title{ Portfolio Diversification Measure }
\description{
  This function computes one of several portfolio diversification measures
  for a single portfolio or a collection of portfolios.
}
\usage{
portfolio.diversification(portfolios, method = c("naive", "herfindahl", 
"herfindahl-hirschman", "hannah-kay", "shannon"), alpha = 2)
}
\arguments{
  \item{portfolios}{ a vector or matrix of portfolio exposures }
  \item{method}{ a character value for the method used to compute the measure }
  \item{alpha}{ a numeric value for parameter required for the Hannah-Kay measure }
}
\details{
  The function ocmputes a portfolio diversification measure for a single portfolio or
  for a collection of portfolios organized as a matrix.
}
\value{
  A vector with one or more values.
}
\references{
Worthington, A. C., 2009. Household Asset Portfolio
Diversification: Evidence from the Household, Income and Labour
Dynamics in Australia (Hilda) Survey, Working Paper, Available at
SSRN: \code{http:////ssrn.com//abstract=1421567}.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\examples{
onePortfolio <- random.longonly( 100, 75 )
naive <- portfolio.diversification( onePortfolio, method = "naive" )
herfindahl <- portfolio.diversification( onePortfolio, method = "herfindahl" )
herfindahl.hirschman <- portfolio.diversification( onePortfolio, method = "herfindahl-hirschman" )
hannah.kay <- portfolio.diversification( onePortfolio, method = "hannah-kay" )
shannon <- portfolio.diversification( onePortfolio, method = "shannon" )

}
\keyword{ math }
