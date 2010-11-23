\name{random.equal.test}
\alias{random.equal.test}
\title{ Random equal weighted portfolios }
\description{
  This function generates a random portfolio of n investments in which there are only k
  positive equal weights. The sum of the weights is \eqn{x_t}.
  The function is used to evaluate the performance of the portfolio generation algorithm.
}
\usage{
random.equal.test(n = 2, k, x.t = 1)
}
\arguments{
  \item{n}{ A positive integer for the number of investments in the portfolio }
  \item{k}{ A positive integer for the number of investments with positive equal weights }
  \item{x.t}{ The sum of the investment weights }
}
\details{
  The R function \code{sample} is used to generate a simple random sample without replacement
  of k values from the integers \eqn{1,2, \ldots ,n}.  These are the subscripts into 
  an \eqn{n \times 1} zero vector to assign
  the equal weight \eqn{x_t / k}.
}
\value{
  A list with two named components.
  \item{x }{An \eqn{n \times 1} numerical vector of investment weights}
  \item{iter }{An integer value for the number of iterations used to obtain the investment weights}
}
\references{
Evans, J. and S. Archer, 1968. Diversification and the Reduction of Risk: An Empirical Analysis,
\emph{Journal of Finance}, 23, 761-767.

Upson, R. B., P. F. Jessup and K. Matsumoto, 1975. Portfolio Diversification Strategies, 
\emph{Financial Analysts Journal}, 31(3), 86-88.

Elton, E. J. and M. J. Gruber, 1977. Risk Reduction and Portfolio Size: An Analytical Solution, 
\emph{Journal of Business}, 50(4), 415-437.

Bird, R. and M. Tippett, 1986. Naive Diversification and Portfolio Risk - A Note, 
\emph{Management Science}, 32(2), 244-251.

Statman, M., 1987. How many stocks make a diversified portfolio, \emph{Journal of Financial and
Quantitative Analysis}, 22, 353-363.

Newbould,  G. D. and P. S. Poon, 1993. The minimum number of stocks needed for diversification, 
\emph{Financial Practice and Education}, 3, 85-87.

O'Neal, E. S., 1997. How Many Mutual Funds Constitute a Diversified Mutual Fund Portfolio,
\emph{Financial Analysts Journal}, 53(2), 37-46.

Statman, M., 2004. The diversification puzzle, \emph{Financial Analysts Journal}, 60, 48-53.

Benjelloun, H. and Siddiqi, 2006. Direct diversification with small stock portfolios. 
\emph{Advances in Investment Analysis and Portfolio Management}, 2, 227-252.

Benjelloun, H., 2010. Evans and Archer - forty years later, \emph{Investment Management and
Financial Innovation}, 7(1), 98-104.
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\examples{
###
### equally weighted portfolio of 30 investments of which 5 are non-zero and
### the rest are zero.  the weights sum to 1.
###
result <- random.equal.test( 30, 5 )
}
\keyword{ math }
