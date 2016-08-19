overweight.segments <- function( portfolios, segments, x.o )
{
###
### This function overweight the investment exposures of the given portfolios
### in the given investment segments by the proportion x.o of the total
### exposure in these segments
###
### Arguments
### portfolios = a vector or matrix of investment portfolios
### segments = a vector or list of vectors that defines the investment segments
### x.o = a non-negative value less than or equal to 1 which is the proportion
###
    if ( missing( portfolios ) )
        stop( "Argument 'portfolios' missing in overweight.segments" )
    if ( missing( segments ) )
        stop( "Argument 'segments' missing in overweight.segments" )
    if ( missing( x.o ) )
        stop( "Argument 'x.o' missing in overweight.segments" )
    if ( x.o < 0 )
        stop( "Argument 'x.o' is negative in vector.underweight.segments" )
    if ( x.o > 1 )
        stop( "Argument 'x.o' is greater than 1 in vector.underweight.segments" )

    vector.overweight.segments <- function( x, segments, x.o )
    {
###
###     his private function overweights the exposures in the given segments
###     by the given proportion
###
###     Arguments
###     x = a numeric vector of investment weights
###     segments = a vector or list that defines the segment
###     x.o = a numeric value for the proportion to over the segments
###
        n <- length( x )
        allInvestments <- 1:n
        activeInvestments <- collapse.segments( segments )
        if ( length( activeInvestments ) > n || max( activeInvestments ) > n )
            stop( "Argument 'segments' has investments that are not allowed" )
        if ( length( activeInvestments ) == n ) {
            if ( all( allInvestments == activeInvestments ) )
                stop( "Cannot underweight all the investments in argument 'x'" )
        }        
        passiveInvestments <- segment.complement( n, activeInvestments )
        x.t.active <- sum( x[activeInvestments] )
        x.t.passive <- sum( x[passiveInvestments] )
        if ( x.t.active == 0 )
            stop( "all active investment exposures are zero" )
        rho.P.A <- x.t.passive / x.t.active
        x.adj <- rep( 0, n )
        x.adj[passiveInvestments] <- ( 1 - x.o ) * x[passiveInvestments]
        x.adj[activeInvestments] <- ( 1 + x.o * rho.P.A ) * x[activeInvestments]
        return( x.adj )
    }

    if ( is.vector( portfolios ) )
        return( vector.overweight.segments( portfolios, segments, x.o ) )
    if ( is.matrix( portfolios ) )
        return( t( apply( portfolios, 1, vector.overweight.segments, segments, x.o ) ) )
    stop( "Argument 'portfolios' is neither a vector nor a matrix" )
    return( NULL )
}
