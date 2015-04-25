## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix has four sub-functions: set, get, setinv, getinv and a variable v
## which stores the cache value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }       
        get <- function() x
        setinv <- function(iv) v <<- iv
        getinv <- function() v
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve first check if the inverse value has been previously calculated (i.e. v is not Null)
## If inverse value has been calculated, then return the cache value.
## Otherwise, calculate the inverse using solve() and store the inverse value using setinv, and
## return the newly calculated inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data)
        x$setinv(v)
        v
}

