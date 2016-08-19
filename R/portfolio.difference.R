portfolio.difference <- function( portfolios, x.b, method = c( "relative","absolute" ) )
{
###
### this function computes the difference measure specified by
### the method for a single portfolio in a vector or a collection of
### portfolios in a matrix
###
### arguments
### portfolios = a vector or matrix of portfolio exposures
### x.b = a vector of benchmark exposures
### method = a character value for the method used to compute the
###          difference measure
### alpha = a numeric value for the parameter required by the hannah-kay
###         difference measure
###
    
    vector.difference <- function( x, x.b, method )
    {
###     this private function computes either the absolute or relative
###     difference of a portfolio and a benchmark portfolio
###
###     Arguments
###     x = a numeric vector of investment exposures
###     x.b = a numeric vector of investment exposures for a benchmark portfolio
###     method = a character value for the method used.
###
        n <- length( x )
        if ( !is.vector( x.b ) )
            stop( "Argument 'x.b' is not a vector in vector.difference" )
        if ( length( x.b ) != n )
            stop( "Argument 'x' and 'x.b' do not have the same lengths" )
        if ( method == "relative" ) {
            measure <- sum( abs( x - x.b ) / ( x + x.b ) )
            return( measure )
        }
        if ( method == "absolute" ) {
            measure <- 0.5 * sum( abs( x - x.b ) )
            return( measure )
        }
        stop( "Argument 'method' is unknown" )
        return( NULL )
    }
    
    method <- match.arg( method )
    if (missing( portfolios ) )
        stop( "Argument 'portfolios' is missing in portfolio.difference" )
    if ( missing( x.b ) )
        stop( "Argument 'x.b' is missing in portfolio.difference" )
    if ( is.vector( portfolios ) )
        return( vector.difference( portfolios, x.b, method ) )
    if ( is.matrix( portfolios ) ) {
        result <- as.vector( apply( portfolios, 1, vector.difference, 
            method ) )
        return( result )
    }
    stop( "argument portfolios is neither a vector nor a matrix" )
    return( NULL )
}
