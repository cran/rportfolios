rlongshort <- function( m, n=2, k=n, segments=NULL, x.t.long=1, x.t.short=x.t.long , 
    max.iter=2000, eps = 0.001 )
{
###
### This function generates m long short random portfolios with n investments where
### gross notional exposure is x.t.long + and x.t.short and the net notional
### exposure is x.t.long - x.t.short.  Results are returned as a matrix.
###
### Arguments
### m = a positive integer value for the number of portfolios to be generated
### m = a positive integer value for the number of investments in a portfolio
### k = a positive integer value for the number of non-zero positions
### x.t.long = a numeric value for the sum of the long exposures in the long portfolio
### x.t.short = a numeric value for the sum of the short exposures in the short portfolio
### p.long = positive real value for the probability that an investment selected at random
###          has a positive exposure
### max.iter = a positive integer value for the maximum iterations in the rejection method
### eps = a positive numeric value for the acceptance rejection method based on gross notional exposure
###
### private function
###
    by.case <- function( case, number, size, theseSegments, total.long, 
        total.short, iterations, epsilon )
    {
        return( random.longshort( n=number, k=size, segments=theseSegments, x.t.long=total.long, 
            x.t.short=total.short, max.iter=iterations, eps = epsilon ) )
    }
    weights <- t( sapply( 1:m, by.case, n, k, segments, x.t.long, 
        x.t.short, max.iter, eps ) )
    return( weights )
}
