underweight.segments <- function( portfolios, segments, x.u )
{
###
### This function underweights the investment exposures of the given portfolios
### in the given investment segments by the proportion x.u of the total
### exposure in these segments
###
### Arguments
### portfolios = a vector or matrix of investment portfolios
### segments = a vector or list of vectors that defines the investment segments
### x.u = a non-negative value less than or equal to 1 which is the proportion
###
    if ( missing( portfolios ) )
        stop( "Argument 'portfolios' missing in underweight.segments" )
    if ( missing( segments ) )
        stop( "Argument 'segments' missing in underweight.segments" )
    if ( missing( x.u ) )
        stop( "Argument 'x.u' missing in underweight.segments" )
    if ( x.u < 0 )
        stop( "Argument 'x.u' is negative in vector.underweight.segments" )
    if ( x.u > 1 )
        stop( "Argument 'x.u' is greater than 1 in vector.underweight.segments" )

    vector.underweight.segments <- function( x, segments, x.u )
    {
###
###     this function underweights the exposures in the given segments
###     by the given proportion of the active investment segment
###
###     Arguments
###     x = a numeric vector of investment weights
###     segments = a vector or list that defines the segment
###     x.u = a numeric value for the proportion to underweight the segments
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
        if ( x.t.passive == 0 )
            stop( "all passive investment exposures are zero" )
        rho.A.P <- x.t.active / x.t.passive
        x.adj <- rep( 0, n )
        x.adj[activeInvestments] <- ( 1 - x.u ) * x[activeInvestments]
        x.adj[passiveInvestments] <- ( 1 + x.u * rho.A.P ) * x[passiveInvestments]
        return( x.adj )
    }

    if ( is.vector( portfolios ) )
        return( vector.underweight.segments( portfolios, segments, x.u ) )
    if ( is.matrix( portfolios ) )
        return( t( apply( portfolios, 1, vector.underweight.segments, segments, x.u ) ) )
    stop( "Argument 'portfolios' is neither a vector nor a matrix" )
    return( NULL )
}
