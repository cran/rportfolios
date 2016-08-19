random.general <- function( n=2, k=n, segments=NULL, p=0.5, x.u=1 )
{
###
### This function generates a general random portfolio of n invesments for which
### there are k non-zero long and short positions.
### The probability that a position is a long position is p.  The maximum absolute
### exposure for any investment is x.u.  The default value is 1.
###
### Arguments
### n = a positive integer for the number of investments in the portfolio
### k = a positive integer for the number of non-zero positions in the portfolio
### segments = a vector or list of vectors that define the portfolio segments
### p = a positive numeric value for the probability that an investment has positive weight
### x.u = a positive numeric value for the maximum absolute exposure to an investment
###
    if ( n <= 0 )
        stop( "argument 'n' is not positive" )
    if ( ( p < 0 ) || ( p > 1 ) )
        stop( "argument 'p' is not a valid probability" )
    if ( x.u <= 0 )
        stop( "argument 'x.u' is not positive" )
###
### investments over specified segments
###
    if ( !is.null( segments ) ) {
        segmentInvestments <- collapse.segments( segments )
        numberInvestments <- length( segmentInvestments )
        if ( numberInvestments > n || max( segmentInvestments ) > n )
            stop( "argument 'segments' has investments that are not allowed" )
        weights <- random.general( n=numberInvestments, 
            k=numberInvestments, segments=NULL, p, x.u )
        investments <- sample( segmentInvestments, numberInvestments, 
            replace=FALSE )
        x <- rep( 0, n )
        x[investments] <- weights
        return( x )
    }    
###
### cardinality k is less than n
###
    if ( k < n ) {
        weights <- random.general( n=k, k, segments=NULL, p, x.u )
        investments <- sample( 1:n, k, replace=FALSE )
        x <- rep( 0, n )
        x[investments] <- weights
        return( x )
    }
###
### cardinality must be n
###
    if ( k > n )
        stop( "argument 'k' is greater than 'm'" )
    signs <- 2 * rbinom(n, 1, p ) - 1
    exposures <- x.u * runif( n )
    x <- signs * exposures
    return( x )
}
