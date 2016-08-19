set.segments <- function( portfolios, n, segments )
{
###
### this function assigns the given investment weights to
### the investments defined by the given segments argument
###
### Arguments
### portfolios = a vector or matrix of investment weights for portfolios
### n = a positive integer value for the total number of investments
### segments = a vector or list of vectors that defines the investments
###
    if ( missing( portfolios ) )
        stop( "Argument 'portfolios' is missing" )
    if ( missing( n ) )
        stop( "Argument 'n' is missing" )
    if ( missing( segments ) )
        stop( "Argument 'segments' is missing" )
    segmentInvestments <- collapse.segments( segments )
    if ( n < max( segmentInvestments ) )
        stop( "n is less than the highest investment index in argument 'segments'" )
###
### private function
###
    vector.set.segments <- function( x, n, segmentInvestments )
    {
        p <- rep( 0, n )
        p[segmentInvestments] <- x
        return( p )
    }
###
    if ( is.vector( portfolios ) ) {
        return( vector.set.segments( portfolios, n, segmentInvestments ) )
    }
    if ( is.matrix( portfolios ) ) {
        return( t( apply( portfolios, 1, vector.set.segments, n, segmentInvestments ) ) )
    }
    stop( "Argument 'portfolios' is neither a vector nor a matrix" )
    return( NULL )
}
