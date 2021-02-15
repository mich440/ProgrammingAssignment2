## Put comments here that give an overall description of what your
## functions do
## The function is to cache the inverse of a matrix instead of doing it
## repeatedly. 
## Write a short comment describing this function
## The function makeCacheMatrix is actually a list that has the function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix 
        get <- function() x  ## get the matrix 
        setinverse <- function(inverse) m <<- inverse  ## set inverse of matrix
        getinverse <- function()  ## get inverse of matrix
                m  ## return the inverse value
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)  ## list of the functions
}


## Write a short comment describing this function
## The function cacheSolve, calculates the inverse of the matrix
## that was created with the function above. 
## If the inverse has already been calculated, it produces the result from
## the getinverse function (skipping the computation)
## If not then it calculates the inverse of the matrix and sets the value of
## the inverse using the setinverse function
cacheSolve <- function(x, ...) {
        m <- x$getinverse() 
        ## returns the inverse if it is already calculated
        if(!is.null(m)) {
                message("getting the cached data")
                return(m)
        }
        data <- x$get() ## get the matrix
        m <- solve(data) ## calculate the inverse
        x$setinverse(m) ## set the inverse 
        m ## Return a matrix that is the inverse of 'x'
}
