random.bounded.test <- function( n=2, x.t=1, x.l=rep(0,n), x.u=rep(x.t,n), max.iter=1000 )
{
###
### This function generates one random portfolio in which
### the asset allocations sum to the given total x.t and are
### constrained to be between the given vectors of lower and upper
### bounds
###
### Arguments
### n = a positive integer value which is the number of assets in the portfolio
### x.t = a numeric value which is the sum of the allocations across all assets
### x.l = a numeric vector of lower bounds for each of the assets in the portfolio
### x.u = a numeric vector of upper bounds for each of the assets in the portfolio
### max.iter = a positive integer which is the maximum number of iterations in
###            the rejection method loop
###
    if ( n < 1 ) {
        stop( "argument n is not a positive integer" )
    }    
    if ( n == 1 ) {
        x <- c(x.t)
        result <- list( x = x, iter = 1 )
        return( result )
    }
    if ( !is.vector( x.l ) ) {
        stop( "argument x.l is not a vector" )
    }
    if ( !is.vector( x.u ) ) {
        stop( "argument x.u is not a vector" )
    }
    if ( length( x.l ) != n ) {
        stop( "the length of argument x.l does not equal the number of assets n")
    }
    if ( length( x.u ) != n ) {
        stop( "the length of argument x.u does not equal the number of assets n" )
    }
    if ( sum ( x.l ) >= x.t ) {
        stop( "the sum of the lower bounds x.u is greater than or equal to x.t" )
    }
    if ( sum( x.u ) <= x.t ) {
        stop( "the sum of the upper bounds x.u is less than or equal to x.t" )
    }    
    if ( any( x.l >= x.u ) ) {
        stop( "at least one of the lower bounds in x.l is greater than or equal to an upper bound in x.u" )
    }
###
### compute the surplus allocation and allocation range
###
    x.s <- x.t - sum( x.l )
    x.r <- x.u - x.l
###
### initial run parameters
    nm1 <- n - 1
    iter <- 0
    more <- TRUE
    while ( more ) {
        indices <- sample( 1:n, n, replace = FALSE )
        z <- rep( 0, n )
        U <- runif( n )
        thisIndex <- indices[1]
        lambda <- min( x.s, x.r[thisIndex] )
        z[thisIndex] <- lambda * U[thisIndex]
        x.s <- x.s - z[thisIndex]
        iter <- iter + 1
        if ( n > 2 ) {
            for ( i in 2:nm1 ) {
                thisIndex <- indices[i]
                lambda <- min( x.s, x.r[thisIndex] )
                z[thisIndex] <- lambda * U[thisIndex]
                x.s <- x.s - z[thisIndex]
            }    
        }
        thisIndex <- indices[n]
        z[thisIndex] <- x.s
        z[thisIndex] <- min( x.s, x.r[thisIndex] )
        x <- x.l + z
        if ( x[thisIndex] <= x.u[thisIndex] ) {
###
###         determine the unallocated surplus
###
            x.s <- x.t - sum( x )
###
###         allocate the urplus if necessary
###
            if ( x.s > 0 ) {
###
###             determine which investments have slack relative to the upper bounds
###
                x.g <- x.u - x
###
###             select the order in which the investments are selected for assigning the slack
###
                indices <- sample( 1:n, n, replace = FALSE )
###
###             loop over the investments
###
                for ( i in 1:n ) {
                    thisIndex <- indices[i]
                    if ( x.g[thisIndex] > 0 ) {
                        amount <- min( x.g[thisIndex], x.s )
                        x[thisIndex] <- x[thisIndex] + amount
                        x.s <- x.s - amount
                    }
                }
           }
           result <- list( x = x, iter = iter )
           return( result )
        }
        if ( iter > max.iter ) {
            stop( "maximum number of iterations exceeded in random.bounded" )
        }    
    }
    result <- list( x = x, iter = iter )
    return( result )
}
