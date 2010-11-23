\name{requal}
\alias{requal}
\title{ Generate equal weighted portfolios }
\description{
  This function generates m random equal portfolios with
  k non-zero, equal weights and the sum of the weights equals \eqn{x_t}.
}
\usage{
requal(m, n = 2, k, x.t=1)
}
\arguments{
  \item{m}{ A positive integer for the number of portfolios in the sample }
  \item{n}{ A positive integer for the number of non-zero equal weights }
  \item{k}{ A positive integer for the number of investments in the portfolio }
  \item{x.t}{ A positive number for the sum of the weights }
}
\details{
  The function executes the function \code{random.equal} using the R function
  sapply.  The result returned is the transpose of the matrix generated in the previous
  step.
}
\value{
  A numeric \eqn{m \times n} matrix.  The rows are the portfolios and
  the columns are the investment weights for each portfolio
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
\seealso{
  \code{\link{random.equal}}
}
\examples{
###
### generate 100 equal weighted portfolios of 30 investments with 10 non zero positions
###
x.matrix <- requal( 100, 30, 10 )
}
\keyword{ math }
