## The two functions below are used to cach the Inverse of a Matrix, if the interse of a matrix
## has been calculated, then the second function  cacheSolve won't calculate it again ,it
## retrives the result from cach


## First function is to create a matrix with function (set, get, setMatrixInv,getMatrixInv)
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL               ##s : inverse of a matrix
    set <-function(y){    
        x <<- y
        s <<-NULL
    }
    get <-function() x
    setMatrixInv <- function(solve) s<<- solve    ## set the matrix inverse by using function solve()
    getMatrixInv <- function() s                   ## get the matrix inverse 
    list(set =set, get = get,setMatrixInv = setMatrixInv,getMatrixInv = getMatrixInv)
    
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s<- x$getMatrixInv()
    ## if the  matrix has not been changed, then directly retrive its inverse from cache
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## else , calculate its inverse
    data <- x$get()
    s<-solve(data,...)
    x$setMatrixInv(s)
    s
    ## Write a short comment describing this function
    
    cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    }
    
} 