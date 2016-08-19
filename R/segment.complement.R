segment.complement <- function( n, segments )
{
###
### this function returns a vector of investments that are in
### a portfolio of n investments but are not in the given segments
###
### Arguments
### n = a positive integer value for the number of investments in the portfolio
### segments = a vector or list of vectors that defines the investment segments
###
    if ( missing( n ) )
        stop( "Argument 'n' is missing" )
    if ( n < 2 )
        stop( "Argument 'n' is less than 2" )
    if ( missing( segments ) )
        stop( "Argument 'segments' is missing" )
    allInvestments <- 1:n
    if ( is.null( segments ) ) {
        return( allInvestments )
    }    
    segmentInvestments <- collapse.segments( segments )
    if ( length( segmentInvestments ) > n || max(segmentInvestments ) > n )
        stop( "Argument 'segments' has investments that are not allowed" )
    if ( length( segmentInvestments ) == length( allInvestments ) ) {
        if ( all( segmentInvestments == allInvestments ) ) {
            return( NULL )
        }
    }    
    complementIndices <- !( allInvestments %in% segmentInvestments )
    complementInvestments <- allInvestments[complementIndices]
    return( complementInvestments )
}
