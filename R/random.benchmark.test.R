random.benchmark.test <- function( n = 2, k = n, segments=NULL, x.t = 1,
    margins=c( "unif", "beta", "exp",    "frechet", "gamma",
               "gev",  "gpd",  "gumbel", "lnorm",   "logis",
               "norm", "weibull"), ... )
{
###
### This function generates a random long only benchmark portfolio by first generating
### n i.i.d truncated random variables on [0,Inf) and computing the ratio
### of each random variable to the sum of the n random variables.
###
### Arguments
### n = a positive integer value for the number of investments
### k = a positive integer value for the number of non-zero investments
### segments = a vector or list of vectors that defines the investment segments
### x.t = a positive real value for the sum of the investment exposures
### margins = a character value for the marginal distribution of the truncated variates
### ... = additional arguments passed to the random variable function for the margin
###
    if ( n == 1 ) {
        result <- list( x=x.t, iter=1 )
        return( result )
    }    
    if ( k > n )
        stop( "Argument 'k' is greater than argument 'n'" )
    margins <- match.arg( margins )
###
### any distribution with a zero standard deviation or zero scale should 
### return an equally weighted portfolio
###
### lognormal distribution
###
    if ( margins == "lnorm" ) {
       margins.args <- list( ... )
       if ( !is.null( margins.args$sdlog ) ) {
           if ( margins.args$sdlog == 0 ) {
               return( random.equal.test( n, k, segments, x.t ) )
           }
       }    
    }   
###
### normal distribution
###
    if ( margins == "norm" ) {
       margins.args <- list( ... )
       if ( !is.null( margins.args$sd ) ) {
           if ( margins.args$sd == 0 ) {
               return( random.equal.test( n, k, segments, x.t ) )
           }
       }    
    }
###
### exponential distribution
###
    if ( margins == "exp" ) {
        margins.args < list( ... )
        if ( !is.null( margins.args$rate ) ) {
            if ( margins.args$rate == 0 ) {
                return( random.equal.test( n, k, segments, x.t ) )
            }
        }
    }
###
### beta distribution
###
    if ( margins == "beta" ) {
        margins.args <- list( ... )
        if ( is.null( margins.args$shape1 ) ) {
            stop( "shape1 argument is missing for margins = beta" )
        }
        if ( is.null( margins.args$shape2 ) ) {
            stop( "shape2 argument is missing for margins = beta" )
        }
        if ( margins.args$beta1 == 1 && margins.args$beta2 == 1 ) {
             return( random.equal.test( n, k, segments, x.t ) )
        }    
    }
###
### gamma distribution
###
    if ( margins == "gamma" ) {
        margins.args <- list( ... )
        if ( !is.null( margins.args$rate ) ) {
            if ( margins.args$Wrate == 0 ) {
                return( random.equal.test( n, k, segments, x.t ) )
            }
        }
    }    
###
### frechet, gev, gpd, gumbel, logistic and weibull distributions
###
    if ( margins %in% c( "frechet", "gev", "gpd", "gumbel", "logis", "weibull" ) ) {
       margins.args <- list( ... )
       if ( !is.null( margins.args$scale ) ) {
           if ( margins.args$scale == 0 ) {
               return( random.equal.test( n, k, segments, x.t ) )
           }
       }    
    }
###
### benchmark portfolio over the specified segments
###
    if ( !is.null( segments ) ) {
        segmentInvestments <- collapse.segments( segments )
        numberInvestments <- length( segmentInvestments )
        if ( numberInvestments > n || max( segmentInvestments ) > n )
            stop( "Argument 'segments' has investments that are not allowed" )
        weights <- random.benchmark( n=numberInvestments, k=numberInvestments, segments=NULL,
            x.t, margins )
        if ( numberInvestments == 1 ) {
            investments <- segmentInvestments
        }
        else {
            investments <- sample( segmentInvestments, numberInvestments, 
                replace=FALSE )
        }    
        x <- rep( 0, n )
        x[investments] <- weights
        result <- list( x=x, iter=1 )
        return( result )
    }    
###
### cardinality less than n
###
    if ( k < n ) {
        if ( k <= 0 ) {
            stop( "Argument 'k' is less than or equal to zero" )
        }    
        weights <- random.benchmark( n=k, k, segments=NULL, x.t, margins )
        investments <- sample( 1:n, k, replace=FALSE )
        x <- rep( 0, n )
        x[investments] <- weights
        result <- list( x=x, iter=1 )
        return( result )
   }
###
### cardinality equal to n
###
    S <- rtrunc( n, spec=margins, a=0, b=Inf, ... )
    values <- S / sum( S )
    indices <- sample( 1:n, n, replace=FALSE )
    weights <- values[indices]
    result <- list( x=weights, iter=1 )
    return( result )
}
