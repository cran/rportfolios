random.longonly <- function(  n=2, k=n, segments=NULL, x.t=1, x.l=0, x.u=x.t, 
    max.iter=1000 )
{
###
### This function generates one random long only portfolio for the given number of assets and other
### portfolio characteristics
###
### Arguments
### n = a positive integer value which is the number of assets in the portfolio
### k = a positive integer value which is the number of non-zero weights
### segments = a vector or list of vectors that defines the portfolio segments
### x.t = a numeric value for the sum of the allocations across all assets
### x.l = a numeric value for the lower bound on the allocation to an asset
### x.u = a numeric value for the upper bound on the allocation to an asset
### max.iter = a positive integer for the maximum number of iterations in the rejection method loop
###
    if ( n == 1 ) {
        x <- c( x.t )
        return( x )
    }    
    if ( k > n ) {
        stop( "Argument k is greater than n" )
    }    
###
### validate arguments
###
    if ( x.l >= x.u ) {
        stop( "Argument 'x.l' is greater than or equal to argument 'x.u'" )
    }
    if ( x.l < 0 ) {
        stop( "Argument 'x.l' is less than zero" )
    }
    if ( x.u < 0 ) {
        stop( "Argument 'x.u' is less than zero" )
    }
###
### long only portfolio over the specified segments
###
    if ( !is.null( segments ) ) {
        segmentInvestments <- collapse.segments( segments )
        numberInvestments <- length( segmentInvestments )
        if ( numberInvestments > n || max( segmentInvestments ) > n )
            stop( "Argument 'segments' has investments that are not allowed" )
        weights <- random.longonly( n=numberInvestments, k=numberInvestments, segments=NULL,
            x.t, x.l, x.u, max.iter )
        if ( numberInvestments == 1 ) {
            investments <- segmentInvestments
        }
        else {
            investments <- sample( segmentInvestments, numberInvestments, 
                replace=FALSE )
        }    
        x <- rep( 0, n )
        x[investments] <- weights
        return( x )
    }    
###
### cardinality less than n
###
    if ( k < n ) {
        if ( k <= 0 ) {
            stop( "Argument 'k' is less than or equal to zero" )
        }    
        weights <- random.longonly( n=k, k, segments=NULL, x.t, x.l, x.u, max.iter )
        investments <- sample( 1:n, k, replace=FALSE )
        x <- rep( 0, n )
        x[investments] <- weights
        return( x )
   }
###
### cardinality equal to n
###
    if ( x.t < 0 ) {
        stop( "Argument 'x.t' is negative in random.longonly" )
    }
    if ( x.u < 0 ) {
        stop( "Argument 'x.u' is negative in random.longonly" )
    }
    if ( x.l < 0 ) {
        stop( "Argument 'x.l' is negative in random.longonly" )
    }    
    if ( n * x.u <= x.t ) {
        stop( "The product of 'n' and 'x.u' is less than or equal to 'x.t' in random.longonly" )
    }
    if ( n * x.l >= x.t ) {
        stop( "The product of 'n' and 'x.l' is greater than or equal to 'x.t' in random.longlonly" )
    }
    if ( x.l > 0 ) {
        total <- x.t - n * x.l
        upper <- x.u - x.l
        iterations <- max.iter
        w <- random.longonly( n, x.t=total, x.l=0, x.u=upper, max.iter=iterations )
        return( x.l + w )
    }    
    nm1 <- n - 1
    iter <- 0
    more <- TRUE
    while( more ) {
        values <- rep( 0, n )
        U <- runif( n )
        values[1] <- x.u * U[1]
        iter <- iter + 1
        if ( n > 2 ) {
            for ( i in 2:nm1 ) {
                im1 <- i - 1
                cumulative.x <- sum( values[1:im1] )
                if ( cumulative.x < x.t ) {
                    upper.bound <- min( x.u, x.t - cumulative.x )
                    if ( upper.bound > 0 ) {
                        values[i] <- upper.bound * U[i]
                    }    
                }    
            }
        }
        cumulative.x <- sum( values[1:nm1] )
        values[n] <- x.t - cumulative.x
        if ( ( values[n] >= 0 ) && (values[n] <= x.u ) ) {
            indices <- sample( 1:n, n, replace=FALSE )
            weights <- values[indices]
            return( weights )
        }
        if ( iter > max.iter ) {
            stop( "Maximum number of iterations exceeded in random.longonly" )
        }
    }
    cumulative.x <- sum( values[1:nm1] )
    values[n] <- x.t - cumulative.x
    indices <- sample( 1:n, n, replace=FALSE )
    weights <- values[indices]
    return( weights )
}
