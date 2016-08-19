rgeneral <- function(m, n=2, k=n, segments=NULL, p=0.5, x.u=1 )
{
###
### This function generates m random general portfolios with n investments 
### Each portfolio has k positive or negative positions.
### The probability that a given non-zero position is positive is p.
### The maximum absolute exposure is x.u which has 1 / k as the default
###
### Arguments
### m = a positive integer value for the number of portfolios to be generated
### n = a positive integer value for the number of investments in each portfolio
### k = a positive integer value for the number of non-zero positions.
### segments = a vector or list of vectors that define the portfolio segments
### p = a positive numeric value for the probability that a non-zero position
###     has positive weight
### x.u = a positive numeric value for the maximum absolute exposure to an investment
###
    if ( missing( m ) )
        stop( "Argument 'm' is missing" )
    if ( m <= 0 ) {
        stop( "Argument 'm' is not positive" )
    }
###
### private function
###
    by.case <- function( case, number, size, theSegments, probability, limit )
    {
        return( random.general( n=number, k=size, segments=theSegments, p=probability, x.u=limit ) )
    }
    weights <- t( sapply( 1:m, by.case, n, k, segments, p, x.u ) )
    if ( n == 1 ) {
        weights <- t( weights )
    }    
    return( weights )
}
