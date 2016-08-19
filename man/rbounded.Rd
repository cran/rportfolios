\name{rbounded}
\alias{rbounded}
\title{ Random bounded portfolios }
\description{
  This function generates m portfolios of n investments
  where the weights are constrained to be within investment specificd
  lower and upper bounds.
}
\usage{
rbounded(m, n = 2, x.t = 1, x.l = rep(0, n), x.u = rep(x.t, n), max.iter = 1000)
}
\arguments{
  \item{m}{ An integer value for the number of portfolios to be generated }
  \item{n}{ An integer value for the number of investments in the portfolio }
  \item{x.t}{ Numeric value for the sum of the investment weights }
  \item{x.l}{ Numeric vector for the lower bounds on the investment weights }
  \item{x.u}{ Numeric vector for the upper bound on the investment weights }
  \item{max.iter}{ An integer value for the maximum iteration in the acceptance rejection loop }
}
\details{
  The function executes the function \code{random.bounded} using the R function
  sapply.  The result returned is the transpose of the matrix generated in the previous
  step.
}
\value{
  A numeric \eqn{m \times n} matrix.  The rows are the portfolios and
  the columns are the investment weights for each portfolio
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
\seealso{
 \code{\link{random.bounded}}
}
\examples{
###
### standard long only portfolio
###
p.1.matrix <- rbounded( 400, 30, 1, rep( 0, 30), rep( 1, 30 ) )

###
### 3% lower bound for all investments
### 100% upper bound for all investments
###
x.lb.all.3 <- rep( 0.03, 30 )
x.ub.all.100 <- rep( 1, 30 )
p.2.matrix <- rbounded( 400, 30, 1, x.l = x.lb.all.3, x.u= x.ub.all.100 )
###
### 4% upper bound for all investments
### 3% lower bound for all investments
x.ub.all.4 <- rep( 0.04, 30 )
p.3.matrix <- rbounded( 400, 30, 1, x.l = x.lb.all.3, x.u = x.ub.all.4 )
###
### 2% lower bound for 1-10, 3% lower bound for 11-20, 2% lower bound for 21-30
### 100% upper bound for all investments
###
x.lb.2.3.2 <- c( rep( 0.02, 10 ), rep( 0.03, 10 ), rep( 0.02, 10 ) )
p.4.matrix <- rbounded( 400, 30, 1, x.l = x.lb.2.3.2, x.u = x.ub.all.100 )
###
### 3% lower bound for 1-10, 2% lower bound for 11-20, 3% lower bound for 21-30
### 100% upper bound for all investments
###
x.lb.3.2.3 <- c( rep( 0.03, 10 ), rep( 0.02, 10 ), rep( 0.03, 10 ) )
p.5.matrix <- rbounded( 400, 30, 1, x.l = x.lb.3.2.3, x.u = x.ub.all.100 )
###
### 2% lower bound for 1-10, 3% lower bound for 11-20, 2% lower bound for 21-30
### 4% upper bound for all investments
###
x.lb.2.3.2 <- c( rep( 0.02, 10 ), rep( 0.03, 10 ), rep( 0.02, 10 ) )
p.6.matrix <- rbounded( 400, 30, 1, x.l = x.lb.2.3.2, x.u = x.ub.all.4 )
###
### 3% lower bound for 1-10, 2% lower bound for 11-20, 3% lower bound for 21-30
### 4% upper bound for all investments
###
x.lb.3.2.3 <- c( rep( 0.03, 10 ), rep( 0.02, 10 ), rep( 0.03, 10 ) )
p.7.matrix <- rbounded( 400, 30, 1, x.l = x.lb.3.2.3, x.u = x.ub.all.4 )
###
### 3% lower bound for all investments
### 4% upper bound for 1-10 5% for 11-20 and 4% for 21-30
###
x.ub.4.5.4 <- c( rep( 0.04, 10 ), rep( 0.05, 10 ), rep( 0.04, 10 ) )
p.8.matrix <- rbounded( 400, 30, 1, x.l = x.lb.all.3, x.u= x.ub.4.5.4 )
###
### 3% lower bound for all investments
### 5% upper bound for 1-10 4% for 11-20 and 5% for 21-30
###
x.ub.5.4.5 <- c( rep( 0.05, 10 ), rep( 0.04, 10 ), rep( 0.05, 10 ) )
p.9.matrix <- rbounded( 400, 30, 1, x.l = x.lb.all.3, x.u= x.ub.5.4.5 )
###
### 3% lower bound for 1-10, 2% for 11-20, 3% for 21-30
### 4% upper bound for 1-10  5% for 11-20  4% for 21-30
###
p.10.matrix <- rbounded( 400, 30, 1, x.l = x.lb.3.2.3, x.u = x.ub.4.5.4 )
###
### 2% lower bound for 1-10, 3% for 11-20, 2% for 21-30
### 4% upper bound for 1-10  5% for 11-20  4% for 21-30
###
p.11.matrix <- rbounded( 400, 30, 1, x.l = x.lb.2.3.2, x.u = x.ub.4.5.4 )
###
### 3% lower bound for 1-10, 2% for 11-20, 3% for 21-30
### 5% upper bound for 1-10  4% for 11-20  5% for 21-30
###
p.12.matrix <- rbounded( 400, 30, 1, x.l = x.lb.3.2.3, x.u = x.ub.5.4.5 )
###
### 2% lower bound for 1-10, 3% for 11-20, 2% for 21-30
### 5% upper bound for 1-10  4% for 11-20  5% for 21-30
###
p.13.matrix <- rbounded( 400, 30, 1, x.l = x.lb.2.3.2, x.u = x.ub.5.4.5 )
}
\keyword{ math }
