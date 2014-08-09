## This function will calculate the inverse of the matrix and stroe it into cahce.
## Next time when you have to calculate the inverse, it will use the cache value



## makeCacheMatrix function returns a list to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  ## cache the inverse of the matrix x
    set <- function(y){  ## set the value of the matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x  ## get the value of the matrix
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getiverse)
}



## cacheSolve function will do the following thing:
## Firstly, it will check if the inverse has been calculated.
## If so, then use the cache value
## If not, calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
