\name{random.bounded.test}
\alias{random.bounded.test}
\title{ Random bounded portfolio }
\description{
  This function generates a portfolio of n investments
  where the weights are constrained to be within investment specific
  lower and upper bounds.
}
\usage{
random.bounded.test(n = 2, x.t = 1, x.l = rep(0, n), x.u = rep(x.t, n), max.iter = 1000)
}
\arguments{
  \item{n}{ An integer value for the number of investments in the portfolio }
  \item{x.t}{ Numeric value for the sum of the investment weights }
  \item{x.l}{ Numeric vector for the lower bounds on the investment weights }
  \item{x.u}{ Numeric vector for the upper bound on the investment weights }
  \item{max.iter}{ An integer value for the maximum iteration in the acceptance rejection loop }
}
\details{
  The simulation method is an extension the method in the function \code{random.longonly}.  The desired
  portfolio \eqn{{\mathbf{x}} = {\left[ {\begin{array}{*{20}{c}}
  {{x_1}}&{{x_2}}& \cdots &{{x_n}} 
  \end{array}} \right]^\prime }} is defined \eqn{{\mathbf{x}} = {{\mathbf{x}}_l} + {\mathbf{z}}},
  that is, the sum of a portfolio of lower bounds with total allocation \eqn{{\mathbf{1'}}\;{{\mathbf{x}}_l}}
  and the portfolio \eqn{{\mathbf{z}}} with total allocation \eqn{{x_t} - {\mathbf{1'}}\,{x_l}}.
  This second portfolio has non-negative weights and upper bounds equal to the range vector
  \eqn{{{\mathbf{x}}_r} = {{\mathbf{x}}_u} - {{\mathbf{x}}_l}}.
  
  In the function \code{random.longonly}, all investment weights have the same lower and upper bounds.
  In \code{random.bounded} investment weights can have different bounds.  Therefore, rather
  than performing a random sampling without replacement of the weights, \code{random.bounded}
  begins with the selection of the indices \eqn{{i_1},{i_2}, \ldots ,{i_n}} as a random sample
  without replacement of the set of investment weight subscripts.  The subscript of the index sample
  defines the order in which the random weights are generated.  The allocations in \eqn{{\mathbf{z}}}
  are scaled uniform random variables.
    
  After completing this acceptance rejection procedure, the function determines any unallocated surplus
  which is the total allocation minus the sum of the lower bounds.  If there is any surplus, then
  the allocation gap is computed as the difference of the upper bounds and the current investment allocations.
  Investments are chosen at random and minimum of the surplus and the gap is added to the allocation.  The
  surplus is reduced by this amount and the adjustment is performed for each investment.
}
\value{
  A numeric vector with investment weights.
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
  \code{\link{random.longonly}}
}
\examples{
###
### standard long only portfolio
###
p.1 <- random.bounded.test( 30, 1 )
###
### 3% lower bound for all investments
### 100% upper bound for all investments
###
x.lb.all.3 <- rep( 0.03, 30 )
x.ub.all.100 <- rep( 1, 30 )
p.2 <- random.bounded.test( 30, 1, x.l = x.lb.all.3, x.u= x.ub.all.100 )
###
### 4% upper bound for all investments
### 3% lower bound for all investments
x.ub.all.4 <- rep( 0.04, 30 )
p.3 <- random.bounded.test( 30, 1, x.l = x.lb.all.3, x.u = x.ub.all.4 )
###
### 2% lower bound for 1-10, 3% lower bound for 11-20, 2% lower bound for 21-30
### 100% upper bound for all investments
###
x.lb.2.3.2 <- c( rep( 0.02, 10 ), rep( 0.03, 10 ), rep( 0.02, 10 ) )
p.4 <- random.bounded.test( 30, 1, x.l = x.lb.2.3.2, x.u = x.ub.all.100 )
###
### 3% lower bound for 1-10, 2% lower bound for 11-20, 3% lower bound for 21-30
### 100% upper bound for all investments
###
x.lb.3.2.3 <- c( rep( 0.03, 10 ), rep( 0.02, 10 ), rep( 0.03, 10 ) )
p.5 <- random.bounded.test( 30, 1, x.l = x.lb.3.2.3, x.u = x.ub.all.100 )
###
### 2% lower bound for 1-10, 3% lower bound for 11-20, 2% lower bound for 21-30
### 4% upper bound for all investments
###
x.lb.2.3.2 <- c( rep( 0.02, 10 ), rep( 0.03, 10 ), rep( 0.02, 10 ) )
p.6 <- random.bounded.test( 30, 1, x.l = x.lb.2.3.2, x.u = x.ub.all.4 )
###
### 3% lower bound for 1-10, 2% lower bound for 11-20, 3% lower bound for 21-30
### 4% upper bound for all investments
###
x.lb.3.2.3 <- c( rep( 0.03, 10 ), rep( 0.02, 10 ), rep( 0.03, 10 ) )
p.7 <- random.bounded.test( 30, 1, x.l = x.lb.3.2.3, x.u = x.ub.all.4 )
###
### 3% lower bound for all investments
### 4% upper bound for 1-10, 5% upper bound for 11-20 and 4% upper boundfor 21-30
###
x.ub.4.5.4 <- c( rep( 0.04, 10 ), rep( 0.05, 10 ), rep( 0.04, 10 ) )
p.8 <- random.bounded.test( 30, 1, x.l = x.lb.all.3, x.u= x.ub.4.5.4 )
###
### 3% lower bound for all investments
### 5% upper bound for 1-10, 4% upper bound for 11-20 and 5% upper bound for 21-30
###
x.ub.5.4.5 <- c( rep( 0.05, 10 ), rep( 0.04, 10 ), rep( 0.05, 10 ) )
p.9 <- random.bounded.test( 30, 1, x.l = x.lb.all.3, x.u= x.ub.5.4.5 )
###
### 3% lower bound for 1-10, 2% for 11-20, 3% for 21-30
### 4% upper bound for 1-10  5% for 11-20  4% for 21-30
###
p.10 <- random.bounded.test( 30, 1, x.l = x.lb.3.2.3, x.u = x.ub.4.5.4 )
###
### 2% lower bound for 1-10, 3% for 11-20, 2% for 21-30
### 4% upper bound for 1-10  5% for 11-20  4% for 21-30
###
p.11 <- random.bounded.test( 30, 1, x.l = x.lb.2.3.2, x.u = x.ub.4.5.4 )
###
### 3% lower bound for 1-10, 2% for 11-20, 3% for 21-30
### 5% upper bound for 1-10  4% for 11-20  5% for 21-30
###
p.12 <- random.bounded.test( 30, 1, x.l = x.lb.3.2.3, x.u = x.ub.5.4.5 )
###
### 2% lower bound for 1-10, 3% for 11-20, 2% for 21-30
### 5% upper bound for 1-10  4% for 11-20  5% for 21-30
###
p.13 <- random.bounded.test( 30, 1, x.l = x.lb.2.3.2, x.u = x.ub.5.4.5 )
}
\keyword{ math }
