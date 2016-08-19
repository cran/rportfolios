rbounded.test <- function( m, n=2, x.t=1, x.l=rep(0,n), x.u=rep(x.t,n), max.iter=1000 )
{
###
### This function generates m long only random portfolios with n investments where
### weights are between given lower and upper bound and the sum of
### the weights equals a given total sum.  Results are return as a matrix
###
### Arguments
### m = a positive integer value for the number of portfolios to be generated
### m = a positive integer value for the number of investments in a portfolio
### x.t = a numeric value for the sum of the investment weights
### x.l = a numeric value for the lower bound for each weight
### x.u = a numeric value for the upper bound for each weight
### max.iter = a positive integer value for the maximum iterations in the rejection method
###
    if ( missing( m ) )
        stop( "Argument 'm' is missing" )
    if ( m <= 0 )
        stop( "Argument 'm' is not positive" )
###
### private function
###
    by.case <- function( case, number, total, lower, upper, iterations )
    {
        return( random.bounded.test( n=number, x.t=total, 
            x.l=lower, x.u=upper, max.iter=iterations ) )
    }
    results <- lapply( 1:m, by.case, n, x.t, x.l, x.u, max.iter )
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
