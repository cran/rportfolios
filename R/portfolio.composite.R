portfolio.composite <- function( portfolios, weights=NULL )
{
###
### This function merges a list of portfolios into a composite using the specified weights.
### The components of the list are either vectors or matrices.  The result is either a vector
### or a matrix.
###
### Arguments
### portfolios = a list of vectors or matrices corresponding to portfolios of investments
### weights = a numeric vector of the weights applied to each component portfolio
###
    if ( !is.list( portfolios ) )
        stop( "Argument portfolios is not a list" )
    
    matrix.composite <- function( portfolioMatrices, weights=NULL )
    {
###
###     This private function takes a list of matrices containing random samples of portfolioMatrices
###     and merges them into a composite matrix using the weights given in the second arguments
###
###     Arguments
###     portfolioMatrices = a list of portfolio matrices with the same dimensions
###     weights = a numeric vector with the weights applied to each portfolio matrix
###
        if ( !is.list( portfolioMatrices ) )
            stop( "Argument 'portfolioMatrices' is not a list" )
        n <- length( portfolioMatrices )
        if ( is.null( weights ) ) {
            theWeights <- rep( 1 / n, n )
        }
        else {
            theWeights <- weights
        }
        if ( !is.vector( theWeights ) )
            stop( "Argument 'weights' is not a vector" )
        for ( i in 1:n ) {
            thesePortfolios <- portfolioMatrices[[i]]
            if ( !is.matrix( thesePortfolios ) ) {
                stop( paste( "Argument portfolioMatrices[[", i, "]] is not a matrix", sep="" ) )
            }
            else {
                if ( i == 1 ) {
                    samples <- nrow( thesePortfolios )
                    investments <- ncol( thesePortfolios )
                }
                else {
                    if ( ( nrow( thesePortfolios ) != samples     ) || 
                         ( ncol( thesePortfolios ) != investments ) ) {
                        stop( "Not all the portfolio matrices are compatible" )
                    }
                }
            }
        }
###
###     initialize the matrix to contain the merged matrices
###
        mergedPortfolios <- matrix( 0, nrow=samples, ncol=investments )
###
###     compute the weighted sum of the component portfolio matrices
###
        for ( i in 1 :n ) {
            thesePortfolios <- portfolioMatrices[[i]]
            thisWeight <- theWeights[i]
            mergedPortfolios <- mergedPortfolios + thisWeight * thesePortfolios
        }
        return( mergedPortfolios )
    }

### ###    

    vector.composite <- function( portfolioVectors, weights=NULL )
    {
###
###     This private function generates a vector that combines the portfolio vectors in the first
###     argument using the weights in the second argument.  The default is an equal weighted
###     portfolio of the given vectors.
###
###     Arguments
###     portfolioVectors = a list of numeric vectors with the investment weights of each portfolio
###     weights = a numeric vector with weights for each component portfolio
###
        if ( !is.list( portfolioVectors ) )
            stop( "Argument 'portfolioVectors' is not a list" )
        n <- length( portfolioVectors )
        if ( is.null( weights ) ) {
            theWeights <- rep( 1/n, n )
        }
        else {
            theWeights <- weights
        }    
        if ( !is.vector( theWeights ) )
            stop( "Argument 'weights' is not a vector" )
        if ( length( portfolioVectors ) != length( theWeights ) )
            stop( "Length of 'portfolioVectors' and 'weights' not compatible" )
        mergedPortfolio <- rep( 0, length( portfolioVectors[[1]] ) )
        for ( i in 1:n ) {
            thisWeight <- theWeights[i]
            thisPortfolio <- portfolioVectors[[i]]
            mergedPortfolio <- mergedPortfolio + thisWeight *thisPortfolio
        }
        return( mergedPortfolio )
    }
    
### ###

    if ( is.vector( portfolios[[1]] ) )
        return( vector.composite( portfolios, weights ) )
    if ( is.matrix( portfolios[[1]] ) )
        return( matrix.composite( portfolios, weights ) )
       
    stop( "Components of portfolios are neither matrices nor vectors" )
    return( NULL )
}
