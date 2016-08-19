rlongonly <- function( m, n=2, k=n, segments=NULL, x.t=1, x.l=0, x.u=x.t, max.iter=1000 )
{
###
### This function generates m long only random portfolios with n investments where
### weights are between given lower and upper bound and the sum of
### the weights equals a given total sum.  Results are return as a matrix
###
### Arguments
### m = a positive integer value for the number of portfolios to be generated
### n = a positive integer value for the number of investments in a portfolio
### k = a positive integer value for the number of non zero weights
### segments = a list or vector of investment indices that define portfolio segments
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
    by.case <- function( case, number, size, theseSegments, total, lower, upper, iterations )
    {
        return( random.longonly( n=number, k=size, segments=theseSegments, x.t=total, 
            x.l=lower, x.u=upper, max.iter=iterations ) )
    }
    weights <- t( sapply( 1:m, by.case, n, k, segments, x.t, x.l, x.u, max.iter ) )
    if ( n == 1 ) {
        weights <- t( weights )
    }    
    return( weights )
}
