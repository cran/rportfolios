rbounded <- function( m, n=2, x.t=1, x.l=rep(0,n), x.u=rep(x.t,n), max.iter=1000 )
{
###
### This function generates m long only random portfolios with n investments where
### weights are between given lower and upper bound and the sum of
### the weights equals a given total sum.  Results are return as a matrix
###
### Arguments
### m = a positive integer value for the number of portfolios to be generated
### n = a positive integer value for the number of investments in a portfolio
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
        return( random.bounded( n=number, x.t=total, 
            x.l=lower, x.u=upper, max.iter=iterations ) )
    }
    weights <- t( sapply( 1:m, by.case, n, x.t, x.l, x.u, max.iter ) )
    if ( n == 1 ) {
        weights <- t( weights )
    }    
    return( weights )
}
