rbenchmark.test <- function( m, n=2, k=n, segments=NULL, x.t=1,
    margins=c( "unif", "beta", "exp",    "frechet", "gamma",
               "gev",  "gpd",  "gumbel", "lnorm",   "logis",
               "norm", "weibull"), ... )
{
###
### This function generates m long only random benchmark portfolios with n investments where
### the weights equals a given total sum.  Results are return as a matrix
###
### Arguments
### m = a positive integer value for the number of portfolios to be generated
### n = a positive integer value for the number of investments in a portfolio
### k = a positive integer value for the number of non zero weights
### segments = a list or vector of investment indices that define portfolio segments
### x.t = a numeric value for the sum of the investment weights
### margins = a character value for the marginal distribution of the truncated variates
### ... = additional arguments passed to the random variable function for the margin
###
    if ( missing( m ) )
        stop( "Argument 'm' is missing" )
    if ( m <= 0 )
        stop( "Argument 'm' is not positive" )
###
### private function
###
    by.case <- function( case, number, size, theseSegments, total, marginals, ...)
    {
        return( random.benchmark.test( n=number, k=size, segments=theseSegments, x.t=total, 
            margins=marginals, ... ) )
    }
    results <- lapply( 1:m, by.case, n, k, segments, x.t, margins, ... )
###
### separate the investment weights and iterations into a matrix and vector
###
    xmatrix <- matrix( 0, nrow=m, ncol=n )
    iters <- rep( 0, m )
    for ( case in 1:m ) {
        thisResult <- results[[case]]
        iters[case] <- thisResult$iter
        xmatrix[case,] <- thisResult$x
    }
###
### create a new result list
###
    result <- list( xmatrix=xmatrix, iters=iters )
    return( result )
}
