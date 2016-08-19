vector.rescale.weights <- function( x, x.t=1 )
{
###
### This function rescales the vector of investment weights x
### such that the sum of the weights is x.t
###
### arguments
### x = a numeric vector of investment weights
### x.t = a numeric value for the revised sum of investment weights
###
    if( missing( x ) )
        stop( "argument 'x' is missing in vector.rescale.weights" )
    if ( !is.numeric( x ) )
        stop( "argument 'x' is not numeric in vector.rescale.weights" )
    sum.x <- sum( x ) 
    if ( abs( sum.x ) < 1e-15 )
        stop( "sum of weights in 'x' is very close to zero in vector.rescale.weights" )
    return( ( x.t / sum.x ) * x )
}
