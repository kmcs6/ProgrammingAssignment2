## These two functions allow a user to cache the inverse of a matrix x, so that
## instead of repeatedly calling solve() in the main function the cached value
## can be retrieved.

## makeCacheMatrix is a function that stores a list of functions that cache 
## (i.e. "set" and "get") a specified matrix 'x' and its inverse 'n'

makeCacheMatrix <- function(x = matrix()) {
      n <- NULL
      set_matrix <- function (y = matrix()) {  ## sets matrix stored in main function
            x <<- y
            n <<- NULL
      }
      get_matrix <- function() x  ##returns matrix x stored in the main function
      set_inverse <- function(inv) n <<- inv ## stores inverse matrix into "inv"
      get_inverse <- function() n            ## returns inverse matrix "n"
      list(set_matrix = set_matrix, get_matrix = get_matrix,  ## returns list of functions
           set_inverse = set_inverse, get_inverse = get_inverse)
}


## The cacheSolve function retrieves the inverse of the matrix 'x' from the cache,
## but if the inverse does not exist the inverse is calculated, stored in the 
## cache and returned.

cacheSolve <- function(x = matrix(), ...) {
      n <- x$get_inverse()             ## retrieves inverse matrix from cache
      if (!is.null(n)) {               ## if the inverse exists, return the value
            message("getting cached data")
            return(n)
      }
      matx <- x$get_matrix()    ## if no inverse exists, retrieve the matrix
      n <- solve(matx, ...)     ## calculate the inverse of the matrix
      x$set_inverse(n)          ## cache the inverse
      n                         ## return the inverse
}
