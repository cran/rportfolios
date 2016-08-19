random.active.test <- function( x.b, x.g, k=length(x.b), segments=NULL, 
    max.iter=2000, eps=0.001 )
{
###
### This function generates an active portfolio relative to the
### given benchmark portfolio.  It is the benchmark portfolio plus
### a notional neutral long short portfolio with the given gross
### notional amount.
###
### Arguments
### x.b = a numeric vector with the benchmark weights
### x.g = a numeric value for the gross notional amount of the long short portfolio
### k = a positive integer value for the number of non zero weights
### segments = a vector or list of vectors that defines the portfolio segments
### max.iter = a positive integer value for the number of iterations in the rejection method
### eps = a positive numeric value for the acceptance rejection method based on gross notional exposure
###
    if ( missing( x.b ) )
        stop( "argument 'x.b' is missing" )
    if ( !is.vector( x.b ) )
        stop( "argument 'x.b' is not a vector" )
    if ( !is.numeric( x.b ) )
        stop( "argument 'x.b' is not a numeric vector" )
    n <- length( x.b )
    if ( n == 1 )
        stop( "Argument 'x.b' must be of length greater than 1" )
    if ( missing( x.g ) )
        stop( "argument 'x.g' is missing" )
    if ( x.g <= 0 )
        stop( "argument x.g is not positive" )
###
### long short tilt is restricted to the given active segments
###
    if ( !is.null( segments ) ) {
        activeInvestments <- collapse.segments( segments )
        numberInvestments <- length( activeInvestments )
        if ( numberInvestments > n || max( activeInvestments ) > n )
            stop( "argument 'segments' has investments that are not allowed" )
        thisResult <- random.active.test( x.b[activeInvestments], x.g, 
            k=numberInvestments, segments=NULL,max.iter, eps )
        weights <- thisResult$x
        iter <- thisResult$iter
        passiveInvestments <- segment.complement( n, activeInvestments )
        x.a <- rep( 0, n )
        x.p <- rep( 0, n )
        x.a[activeInvestments] <- weights
        x.p[passiveInvestments] <- x.b[passiveInvestments]
        x <- x.a + x.p
        result <- list( x=x, iter=iter )
        return( result )
    }    
###
### long short tilt is restricted to k randomly selected investments
###
    if ( k < n ) {
        if ( k < 2 )
            stop( "argument 'k' is less than two" )
        allInvestments <- 1:n
        activeInvestments <- sample( allInvestments, k, replace=FALSE )
        thisResult <- random.active.test( x.b[activeInvestments], x.g, k,
            segments=NULL, max.iter, eps )
        weights <- thisResult$x
        iter <- thisResult$iter
        passiveInvestments <- segment.complement( n, activeInvestments )
        x.a <- rep( 0, n )
        x.p <- rep( 0, n )
        x.a[activeInvestments] <- weights
        x.p[passiveInvestments] <- x.b[passiveInvestments]
        x <- x.a + x.p
        result <- list( x=x, iter=iter )
        return( result )
    }    
###
### long short tilt is for all investments with full cardinality
###
    if ( k > n )
        stop( "argument 'k' is greater than the number of benchmark investments" )
    x.t.b <- sum( x.b )
    x.t.long <- x.g * x.t.b / 2
    x.t.short <- x.t.long
    thisResult <- random.longshort.test( n, k, segments, x.t.long, 
        x.t.short, max.iter, eps )    
    x.ls <- thisResult$x
    iter <- thisResult$iter
    x <- x.b + x.ls
    result <- list( x=x, iter=iter )
    return( result )
}
