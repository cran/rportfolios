requal <- function( m, n=2, k=n, x.t=1 )
{
###
### This function generates m long only random portfolios with k investments where
### there are n non-zero equal weights that sum to x.t
###
### Arguments
### m = a positive integer value for the number of portfolios to be generated
### n = a positive integer value for the number of investments in a portfolio
### k = a positive integer value for the number of non zero investments
### x.t = the sum of the investment weights
###
### private function
###
    by.case <- function( case, number, size, total )
    {
        return( random.equal( n=number, k=size, x.t=total ) )
    }
###
### validate arguments
###
    weights <- t( sapply( 1:m, by.case, n, k, x.t ) )
    if ( n == 1 ) {
        weights <- t( weights )
    }    
    return( weights )
}
