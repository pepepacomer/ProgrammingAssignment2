#makeCacheMatrix and cacheSolve are two functions that are used to
#create a special object that stores a matrix and cache its mean.


#makeCacheMatrix creates a special "matrix", which is really
#a list containing a function to set the value of the matrix, 
#get the value of the matrix, set the value of the matrix and
#get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
       #Inverse
       inv <- NULL
       #Set function
       set <- function(y) {
              x <<- y
              inv <<- NULL
       }
       #get function
       get <- function() x
       #setinverse function
       setinverse <- function(inverse) inv <<- inverse
       #getinverse function
       getinverse <- function() inv
       #list
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" 
##created with the above function. 
##It first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value
#of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getinverse()
       if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
       }
       data <- x$get()
       inv <- solve(data)
       x$setinverse(inv)
       inv
}
