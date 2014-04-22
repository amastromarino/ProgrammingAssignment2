## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function will create a matrix object

makeCacheMatrix <- function(x = matrix()) {
  ##This clears m right off the bat
  m <- NULL
  ##This creates the set function, which will clear m
  ##letting cacheSolve know what to do
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##This creates the get function
  get <- function() x
  ##This creates the setinverse function
  setinverse <- function(inverse) m <<- inverse
  ##This creates the getinverse function
  getinverse <- function() m
  ##This creates the list, which will let us do them later
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
##This function will compute the inverse of the maatrix returned by
##the makeCachematrix formula

cacheSolve <- function(x, ...) {
  ##Checks to see if m is cached or needs to be calculated
  m <- x$getinverse()
  if(!is.null(m)){
    ##If cached, returns cached data
    message("getting cached data")
    return(m)
  }
  ##Calculates the inverse by setting the data and solving
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)

  ## Return a matrix that is the inverse of 'x'
  m
}

