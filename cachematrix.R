#these functions calculate the mean of a matrix and calculate its inverse, then store that value


#set up the matrix and cache its inverse
#create a list of functions for setting and getting the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(z) inv <<- z
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

#this function calls the list of functions from makeCacheMatrix to get the inverse if it
#exists or to calculate and set it if there is no existing value or the matrix has changed

cacheSolve <- function(x, ...) {

                m <- x$getinv()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                inv <- solve(data)
                x$setinv(inv)
                inv
}
