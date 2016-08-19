random.longshort.test <- function( n=2, k=n, segments=NULL, x.t.long=1, 
    x.t.short=x.t.long, max.iter=2000, eps=0.001 )
{
###
### This function constructs a random portfolio of weights in a long short portfolio.  It is
### algebraically a long only random portfolio plus a short only random portfolio. The gross
### notional exposure is x.t.long + x.t.short whereas the net notional exposure is
### x.t.long - x .t.short.
###
### Arguments
### n = a positive integer value for the number of investments
### k = a positive integer value for the number of non-zero weights
### segments = a vector or list of vectors that defines the portfolio segments
### x.t.long = a positive numeric value for the sum of the exposures in the long only leg
### x.t.short = a positive numeric value for the sum of the absolute exposures in the short only leg
### max.iter = a positive integer value for the number of iterations in the rejection method
### eps = a positive numeric value for the acceptance rejection method based on gross notional exposure
###
###
### validate arguments
###
    if ( x.t.long < 0 )
        stop( "Argument 'x.t'.long is less than zero" )
    if ( x.t.short < 0 )
        stop( "Argument 'x.t'.short is less than zero" )
    if ( k > n )
        stop( "Argument 'k' is greater than 'm'" )
    iter <- 0
    more <- TRUE
    gross.t <- x.t.long + x.t.short
###
### long short portfolio over the specified segments
###
    if ( !is.null( segments ) ) {
        segmentInvestments <- collapse.segments( segments )
        numberInvestments <- length( segmentInvestments )
        if ( numberInvestments > n || max( segmentInvestments ) > n )
            stop( "Argument 'segments' has investments that are not allowed" )
        thisResult <- random.longshort.test( n=numberInvestments, 
            k=numberInvestments, segments=NULL, x.t.long, x.t.short, 
            max.iter, eps )
        weights <- thisResult$x
        iter <- thisResult$iter
        investments <- sample( segmentInvestments, numberInvestments, 
            replace=FALSE )
        x <- rep( 0, n )
        x[investments] <- weights
        result <- list( x=x, iter=iter )
        return( result )
    }
###
### cardinality less than n
###
    if ( k < n ) {
        if ( k < 2 ) {
            stop( "cardinality k is less than 2" )
        }    
        result <- random.longshort.test( n=k, k, segments=NULL, x.t.long, 
            x.t.short, max.iter, eps )
        weights <- result$x
        iter <- result$iter
        investments <- sample( 1:n, k, replace=FALSE )
        x <- rep( 0, n )
        x[investments] <- weights
        result <- list( x=x, iter=iter )
        return( result )
    }
###
### cardinality equals n
###
   if ( n == 2 ) {
        long <- sample( 1:2, 1, replace=FALSE )
        short <- 3 - long
        x <- rep( 0, 2 )
        x[long] <- x.t.long
        x[short] <- - x.t.short
        result <- list( x=x, iter=1 )
        return( result )
    }
    while (more ) {
        signs <- 2 * rbinom(n, 1, 0.5 ) - 1
        if ( abs( sum( signs ) ) < n ) {
            investments <- seq( 1, n, 1 )
            long.segment <- investments[ signs == 1 ]
            short.segment <- investments[ signs == -1 ]
            x.long <- random.longonly( n, k, segments=long.segment, 
                x.t=x.t.long, x.l=0, x.u=x.t.long, max.iter )
            x.short <- random.shortonly( n, k, segments=short.segment, 
                x.t=x.t.short, x.l=0, x.u=x.t.short, max.iter )
            x.longshort <- x.long + x.short
            x.gross <- sum( abs( x.longshort ) )
            if ( abs( x.gross - gross.t ) <= eps ) {
                result <- list( x=x.longshort, iter=iter )
                return( result )
            }
        }
        iter <- iter + 1
        if ( iter > max.iter ) {
            stop( "Iteration limit exceeded in random.longshort" )
        }
    }
    if ( is.null( segments ) ) {
        investments <- sample( 1:n, k, replace=FALSE )
    }
    else {
        segmentInvestments <- collapse.segments( segments )
        investments <- sample( segmentInvestments, length( segmentInvestments ), 
            replace=FALSE )
    }    
    x <- rep( 0, n )
    x[investments] <- x.longshort
    result <- list( x=x, iter=iter )
    return( result )
}
