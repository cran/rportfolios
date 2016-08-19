random.shortonly <- function(  n=2, k=n, segments=NULL, x.t=1,
    x.l=0, x.u=x.t, max.iter=1000 )
{
###
### This function generates one random short only portfolio for the given number of assets and other
### portfolio characteristics
###
### Arguments
### n = a positive integer value which is the number of assets in the portfolio
### k = a positive integer value for the number of non-zero weights
### segments = a vector or list of vectors that defines the portfolio segments
### x.t = a numeric value for the sum of the absolute value 
###       of the allocations across all assets
### x.l = a numeric value for the lower bound on the absolute value 
###       of the allocation to an asset
### x.u = a numeric value for the upper bound on the absolute value 
###       of the allocation to an asset
### max.iter = a positive integer for the maximum number of iterations 
###       in the rejection method loop
###
###
### validate arguments
###
    if ( x.l >= x.u )
        stop( "Argument 'x.l' is greater than or equal to argument 'x.u'" )
    if ( x.l < 0 )
        stop( "Argument 'x.l' is less than zero" )
    if ( x.u < 0 )
        stop( "Argument 'x.u' is less than zero" )
    return( - random.longonly( n, k, segments, x.t, x.l, x.u, max.iter ) )
}
