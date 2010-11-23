\name{rgeneral}
\alias{rgeneral}
\title{ Generate random general portfolios }
\description{
  This function generates m random general portfolios with n investments each.  There are k positions
  that can be positive or negative.  The probability that a given investment weight is
  positive is p.  The maximum absolute exposure is x.u which has \eqn{1 / k} as the default
}
\usage{
rgeneral(m, n = 2, k = n, p = 0.5, x.u = 1/k)
}
\arguments{
  \item{m}{ A positive integer value for the number of portfolios }
  \item{n}{ A positive integer value for the number of investments in the portfolio }
  \item{k}{ A positive integer value for the number of non-zero investment positions }
  \item{p}{ A positive numeric value for the probability that a non-zero position is positive }
  \item{x.u}{ A positive numeric value for the maximum absolute exposure to an investment }
}
\details{
  The function executes the function \code{random.general} using the R function
  \code{sapply}.  The result returned is the transpose of the matrix generated in the previous
  step.
}
\value{
  An \eqn{m \times n} numeric matrix.  The rows are the portfolios and
  the columns are the investment weights for each portfolio
}
\author{ Frederick Novomestky \email{fnovomes@poly.edu} }
\seealso{
  \code{\link{random.general}}
}
\examples{
###
### 100 long only portfolios of 30 investments with 30 non-zero positions
###
x.long <- rgeneral( 100, 30, p=1.0 )
###
### 100 long only portfolios of 30 investments with 10 non-zero positions
###
y.long <- rgeneral( 100, 30, 10, p=1.0 )
###
### 100 short only portfolios of 30 investments with 30 non-zero positions
###
x.short <- rgeneral( 100, 30, p=0.0 )
###
### 100 short only portfolios of 30 investments with 10 non-zero positions
###
y.short <- rgeneral( 100, 30, 10, p=0.0 )
###
### 100 long short portfolios of 30 investments with 30 non-zero positions
###
x.long.short <- rgeneral( 100, 30, p=0.5 )
###
### 100 long short portfolios of 30 investments with 10 non-zero positions
###
y.long.short <- rgeneral( 100, 30, 10, p=0.5 )
###
### 100 long bias portfolios of 30 investments with 30 non-zero positions
###
x.long.bias <- rgeneral( 100, 30, p=0.7 )
###
### 100 long bias portfolios of 30 investments with 10 non-zero positions
###
y.long.bias <- rgeneral( 100, 30, 10, p=0.7 )
###
### 100 short bias portfolios of 30 investments with 30 non-zero positions
###
x.short.bias <- rgeneral( 100, 30, p=0.3 )
###
### 100 short bias portfolios of 30 investments with 10 non-zero positions
###
y.short.bias <- rgeneral( 100, 30, 10, p=0.3 )
}
\keyword{ math }
