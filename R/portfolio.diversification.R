portfolio.diversification <- function( portfolios, method = c( "naive",
    "herfindahl", "herfindahl-hirschman", "hannah-kay", "shannon" ), alpha = 2 )
{
###
### this function computes the diversification measure specified by
### the method for a single portfolio in a vector or a collection of
### portfolios in a matrix
###
### arguments
### portfolios = a vector or matrix of portfolio exposures
### method = a character value for the method used to compute the
###          diversification measure
### alpha = a numeric value for the parameter required by the hannah-kay
###         diversification measure
###

    vector.diversification <- function( x, method, alpha )
    {
###
###     this private function computes the diversification measure specified by
###     the method for a single portfolio in a vector
###     portfolios in a matrix
###
###     arguments
###     x       = a vector of portfolio exposures
###     method = a character value for the method used to compute the
###              diversification measure
###     alpha = a numeric value for the parameter required by the hannah-kay
###             diversification measure
###
        n <- length( x )
        non.zero <- x != 0
        k <- sum( non.zero )
        abs.x <- abs( x )
        if ( method == "naive" ) {
            if ( k == 0 ) {
                stop( "all the weights in the portfolio are zero" )
            }
            else {
                return ( 1 / k )
            }
        }
        if ( method == "herfindahl" || method == "herfindahl-hirschman" ) {
            return( sum( x * x ) )
        }    
        if ( method == "hannah-kay" ) {
            if ( alpha == 1 ) {
                return( prod( abs.x ^ abs.x ) )
            }
            power <- 1 / ( alpha - 1 )
            return( ( sum( abs.x ^ alpha ) ) ^ power )
        }
###
###     Shannon entropy measure
###
        if ( method == "shannon" ) {
            non.zero.x <- abs.x[non.zero]
            return( sum( non.zero.x * log( non.zero.x ) ) )
        }
    }


    if ( missing( portfolios ) )
        stop( "argument 'portfolios' is missing" )
    if ( !is.numeric( portfolios ) )
        stop( "argument 'portfolios' is not numeric" )
    method <- match.arg( method )
    if ( is.vector( portfolios ) )
        return( vector.diversification( portfolios, method, alpha ) )
    if ( is.matrix( portfolios ) ) {
        result <- as.vector( apply( portfolios, 1, vector.diversification, 
            method, alpha ) )
        return( result )
    }
    stop( "argument portfolios is neither a vector nor a matrix" )
    return( NULL )
}
