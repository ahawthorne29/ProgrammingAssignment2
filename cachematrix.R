## These functions work together to check if a square matrix has an inverse in cache and, if not, adds the inverse to cache
## If the inverse is in the cache it will be returned

## makeCacheMatrix is a function that returns a list of four more functions. These four functions are used in the subsequent
## cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL ##initialise the object that will hold the inverse
          set <- function(y) {
                x <<- y
                m <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) x <<- solve
          getinverse <- function() m
          list (set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
  
}


## cacheSolve takes a matrix and first of all checks if its inverse is stored in the cache
##if it is stored it returns it with a message. If not, it calculates and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getinverse()       ##look in the cache to see if there already is an inverse for this matrix
          if(!is.null(m)) {         ##if there is an inverse then return it - we're done.
                message("getting cached data")
                return(m)
          }
          data <- x$get()           ##otherwise we calculate the inverse and store it in the cache
          m <- solve(data,...)
          x$setinverse(m)
          m
          }
  
