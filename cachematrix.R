## To calculate the inverse of a matrix and store it in cache 
## That way the required value can be retrieved easily

## This function creates a matrix object and returns a list

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(matrix) m <<- solve
      getmatrix <- function() m
      func_list <<- list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
      func_list
}


## This function calculates the inverse of the matrix or gets the value from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrix(m)
      m
}
