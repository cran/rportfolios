extract.segments <- function( portfolios, segments, collapse=FALSE )
{
###
### This function extracts the investment weights in the given portfolios
### and for the selected investment segments
###
### Arguments
### portfolios = a vector or matrix of investment portfolios
### segments = a vector or list of vectors that defines the investment segments
### collapse = a logical value which if TRUE the passive investments are removed
###
    if ( missing( portfolios ) )
        stop( "Argument 'portfolios' is missing" )
    if ( missing( segments ) )
        stop( "Argument 'segments' is missing" )

    vector.extract.segments <- function( x, segments, collapse=FALSE )
    {
###
###     This private function extracts the investment weights in the portfolio
###     for the selected investment segments
###
###     Arguments
###     x = a numeric vector for the investment weights in a portfolio
###     segments = a vector or list of vectors that defines the investment segments
###     collapse = a logical value.  if TRUE passive investments are removed
###
        n <- length( x )
        allInvestments <- 1:n
        if ( is.null( segments ) )
            return( rep( 0, n ) )
        activeInvestments <- collapse.segments( segments )
        if ( length( activeInvestments ) > n || max( activeInvestments ) > n )
            stop( "Argument 'segments' includes investments that are not allowed" )
        if ( collapse ) {
            x.ext <- x[activeInvestments]
        }    
        else {
            x.ext <- rep( 0, n )
            x.ext[activeInvestments] <- x[activeInvestments]
        }
        return( x.ext )
    }
    
    if ( is.vector( portfolios ) )
        return( vector.extract.segments( portfolios, segments, collapse ) )

    if ( is.matrix( portfolios ) )
        return( t( apply( portfolios, 1, vector.extract.segments, segments, collapse ) ) )

    stop( "Argument 'portfolios' is neither a vector nor a matrix" )
    return( NULL )
}
