## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The below method/function(s) returns set of functions to the given variable as a list.

#For instance,

#Assume I have created an matrix matinput<-matrix(c(2,3,2,2),2,2) (This is invertibale matrix)

#output1<-makeCacheMatrix(matinput) returns a list of all the functions defined to that variable.

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL # Create a temporary variable m
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          ## to return the value of stored vector
          get <- function() x
          ## set the inverse in the parent environment
          setinverse <- function(solve) m <<- solve
          ## get the inverse from the parent environment
          getinverse <- function() m
          ## returns the functions as a list with the names specified as set,get,setinverse & getinversw
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if the inverse is found in the parent environment, then the value is returned.
          m <- x$getinverse()
          if(!is.null(m)) {
            message("getting cached data")
            return(m)
          }
          ## if the inverse is not found, get the matrix into variable data
          data <- x$get()
          ## now find the inverse of the data variable and store in m variable
          m <- solve(data, ...)
          ## the inverse is stored 
          x$setinverse(m)
          ## inverse is returned
          m
}
