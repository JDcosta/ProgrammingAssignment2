#Matrix Inversion is Matrix inversion is usually a costly computation
#and there is likely to be a some benefit to caching the inverse 
#of a matrix and when needed looking up the value of the inverse from
#the cache rather than computing the inverse repeatedly.
#Below are two functions that are used to create a special object 
#that stores a matrix and caches its inverse by taking advantage of 
#the scoping rules of the R language and how they can be manipulated 
#to preserve state inside of an R object.

#The first function -makeCacheMatrix- takes a matrix as its argument and
#creates a special "matrix" object that can cache the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL
      set <- function(y) {
            x <<- y
            xinv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) xinv <<- inverse
      getinverse <- function() xinv
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


#The cacheSolve function takes as an argument the special "matrix" object 
#returned by the makeCacheMatrix above and computes its inverse.If the inverse 
#has already been calculated (and the matrix has not changed), then the 
#cachesolve  retrieves the inverse from cache else it computes the inverse
#using the solve function in R, caches and returns it.
# for this assignment all input matrices are assumed to be square and invertible

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      xinv <- x$getinverse()
      if(!is.null(xinv)) {
            message(" Retreiving the inverse from cache")
            return(xinv)
      }
      data <- x$get()
      xinv <- solve(data, ...)
      x$setinverse(xinv)
      xinv
}
