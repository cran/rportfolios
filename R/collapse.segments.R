collapse.segments <- function( segments )
{
###
### This function takes the argument which is either a vector of investment indices
### or a list of vectors of investment indices and returns a vector of investment indices
### duplicate indices are removed.  Indices are in ascending order.
###
### Arguments
### segments = a vector or list of investment indices
###
    if ( is.null( segments ) ) {
        return( NULL )
    }
    if ( is.list( segments ) ) {
        return( sort( unique( unlist( segments ) ) ) )
    }
    if ( is.vector( segments ) ) {
        return( sort( unique( segments ) ) )
    }
    stop( "Argument is neither a vector nor a list" )
    return( NULL )
}
