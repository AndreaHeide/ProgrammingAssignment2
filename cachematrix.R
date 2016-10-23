

## makeCacheMatrix creates a matrix object with get/set functions and the ability to store the inverse matrix
## I followed closely the example of 'makeVector' 

makeCacheMatrix <- function(x = matrix()) {
   
        invx <- NULL ## inverse of the matrix in environment of makeCacheMatrix, available to cacheSolve   
   
        set <- function(y){
                x <<- y   ## sets matrix in parent environment = makeCacheMatrix
                s <<- NULL ## sets inverse in parent environment = makeCahceMatrix to NULL
        }
        
        get <- function() x ## returns x from parent environment = makeCacheMatrix
        
        setinverse <- function(inverse) invx <<- inverse ## sets inverse in environment makeCacheMatrix
        
        getinverse <- function() invx   ##returns inverse from makeCacheMatrix environment
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse) ## list returned to make functions available, named
} 



## cacheSolve takes as input the matrix object from the makeCacheMatrix function,
## it checks wether the inverse has already been calculated and is in the cache, then it just returns it,
## otherwise it calculates the inverse and stores it via the setinverse function of the matrix object for the next time;
## the messages indicate wether it has ben calculated or fetched from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invx <- x$getinverse()
        if (!is.null(invx)) {
                message ("getting cached data")     ## check if invx has been calculated before and stored in parent envrionment  
                return(invx)                        ## return without calculating
        }
        else {
                m2 <- x$get() ## if it is not in the cache alreay, fetch matrix and calculate inverse
                invx <- solve(m2) %*% m2   ## use solve function to calculate inverse
                x$setinverse(invx)
                message("calculated invx")
                invx
        }
}
