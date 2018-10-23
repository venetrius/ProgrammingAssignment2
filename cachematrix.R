## makeCacheMatrix accept a matrix as an argument and return a list 
## a list contains methods to get and set the matrix as well methods to set and get the inverse 
makeCacheMatrix <- function(x = matrix()) {
    test = nrow(x) == ncol(x)
    if(length(test) == 0 || !test){
        warning("Not a square matrix")
        return()
    }
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## accepct one argument (wich should be) the list created by makeCacheMatrix
## if the inverse already calculated returns the chached value
## else calculates the inverse, saves and returns it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

