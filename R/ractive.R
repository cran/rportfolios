ractive <- function( m, x.b, x.g , k=length(x.b ), segments=NULL, 
    max.iter=2000, eps=0.001 )
{
###
### This function generates m long short random portfolios with n investments where
### gross notional exposure is x.t.long + and x.t.short and the net notional
### exposure is x.t.long - x.t.short.  Results are returned as a matrix.
###
### Arguments
### m = a positive integer value for the number of portfolios to be generated
### x.b = a numeric vector with the benchmark investment weights
### x.g = a numeric value for the gross notional amount
### k = a positive integer value for the number of non-zero positions
### max.iter = a positive integer value for the maximum iterations in the rejection method
###
### private function
###
    by.case <- function( case, benchmark, gross.notional, size, theseSegments, 
        iterations, epsilon )
    {
        return( random.active( x.b=benchmark, x.g=gross.notional, 
            k=size, segments=theseSegments, max.iter=iterations, eps=epsilon ) )
    }
    weights <- t( sapply( 1:m, by.case, x.b, x.g, k, segments, max.iter, eps ) )
    return( weights )
}
