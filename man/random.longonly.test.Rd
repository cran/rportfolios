\name{random.longonly.test}
\alias{random.longonly.test}
\title{ Random long only portfolio }
\description{
  This function generates a vector of investment weights for a portfolio
  where the weights are non-negative, do not exceed a given upper and
  and the sum of the weights is a given total.  The number of non zero
  positions is k.  This function is used to evaluation the computational
  performance of the portfolio generation algorithm.
}
\usage{
random.longonly.test(n = 2, k = n, segments = NULL, x.t = 1, x.l=0, 
x.u = x.t, max.iter = 1000)
}
\arguments{
  \item{n}{ An integer value for the number of investments in the portfolio }
  \item{k}{ An integer value for the number of non zero weights }
  \item{segments}{ A vector or list of vectors that defines the portfolio segments }
  \item{x.t}{ Numeric value for the sum of the investment weights }
  \item{x.l}{ Numeric value for the lower bound on an investment weight }
  \item{x.u}{ Numeric value for the upper bound on an investment weight }
  \item{max.iter}{ An integer value for the maximum iteration in the acceptance rejection loop }
}
\details{
  The simulation methods combines the acceptance rejection method used for generating
  gamma and gaussian random variables with a continuous analog of the method used in
  Ross (2006) to generate a vector of multinomial random variables.  \eqn{n - 1} random variables
  are constructed where the first \eqn{U_1} is uniformly distributed in the interval \eqn{\left[ X_l, X_t \right]}.
  Random variable \eqn{U_2} is a uniform random variable in \eqn{\left[ {X_l,X_t  - U_1 } \right]} given \eqn{U_1}.
  Random variable \eqn{U_3} is a uniform random variable in \eqn{\left[ {0,X_t  - U_1  - U_2 } \right]} given \eqn{U_1} and \eqn{U_2}.
  This conditional generation of uniform random variables stops with \eqn{U_{n - 1}} which is uniform on
  \eqn{\left[ {X_l,X_t  - \sum\limits_{j = 1}^{n - 2} {U_j } } \right]} given the first \eqn{n - 2} random variables.
  if \eqn{X_t  - \sum\limits_{j = 1}^{n - 1} {U_j }} is less than or equal to \eqn{X_u}, then the final random variable is 
  \eqn{U_n  = X_t  - \sum\limits_{j = 1}^{n - 1} {U_j }}. Otherwise, the above procedure of
  generating uniform random variables conditionally is repeated until this condition is satisfied.
  The vector  \eqn{{\mathbf{W}}} is a random sample of size \eqn{n} of the values in vector \eqn{{\mathbf{U}}}
  where the sampling is performed without replacement.
}
\value{
  A list with two named components.
  \item{xmatrix }{An \eqn{m \times n} matrix of investment weights}
  \item{iters }{An \eqn{m \times 1} vector with the number of iterations used to obtain the portfolios}
  A list with two named components.
  \item{x }{An \eqn{n \times 1} numerical vector of investment weights}
  \item{iter }{An integer value for the number of iterations used to obtain the investment weights}
}
\references{
Cheng, R. C. H., 1977. The Generation of Gamma Variables with Non-integral Sape Parameter,
\emph{Journal of the Royal Statistical Society}, Series C (Applied Statistics), 26(1), 71.

Kinderman, A. J. and J. G. Ramage, 1976. Computer Generation of Normal Random Variables,
\emph{Journal of the American Statistical Association}, December 1976, 71(356), 893.

Marsaglia, G. and T. A. Bray, 1964. A Convenient method for generating normal variables, \emph{SIAM Review},
6(3), July 1964, 260-264.

Ross, S. M. (2006). \emph{Simulation}, Fourth Edition, Academic Press, New York NY.

Tadikamalla, P. R., (1978). Computer generation of gamma random variables - II, {Communications
of the ACM}, 21 (11), November 1978, 925-928.
}
\author{ Frederick Novomestky \email{fn334@nyu.edu} }
\examples{
###
### long only portfolio of 30 investments with 30 non-zero positions
###
result.x <- random.longonly.test( 30 )
###
### long only portfolio of 30 investments with 10 non-zero positions
###
result.y <- random.longonly.test( 30, 10 )
}
\keyword{ math }
