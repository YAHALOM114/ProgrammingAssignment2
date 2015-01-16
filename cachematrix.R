##These functions caculate the inverse matrix of an inversable matrix and store the results in a cache for future use. If it need to caculate the inverse of the same matrix again, it uses the stored results.

##This function creates a special "matrix" object and cache its inverse. In addition, the function creats a list of set and get the matrix and its caculated inverse
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The function computes the inverse of Matrix "x" using "solve(x)" and stores the data in "s".If the inverse has been calculated the value is retrieved from the cache
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s  ## Return a matrix that is the inverse of 'x'
}
