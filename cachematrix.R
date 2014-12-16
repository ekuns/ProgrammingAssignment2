## The functions in this class, together, provide a cached implementation
## of the R solve() function to calculate the inverse of a matrix.  e.g.:
## http://cran.r-project.org/doc/manuals/r-release/R-intro.html#index-solve
## Calculating the inverse of a matrix can take a while.  If a larger
## calculation needs to repeatedly calculate the inverse of a matrix, and
## the matrix isn't changing (or not often), one can save a significant
## amount of time by caching the results of the calculation.
##
## Usage example, assuming m and m2 are square, invertible matrices
## m <- matrix(runif(7^2),7)
## cm <- makeCacheMatrix(m)        # create a Cache Matrix object
## inverseOfM <- cacheSolve(cm)    # Get the inverse of m
## inverseOfM <- cacheSolve(cm)    # Get the inverse of m (as cached)
## m2 <- matrix(runif(7^2),7)      # make a new matrix
## cm$set(m2)                      # replace the matrix with a new one
## inverseOfM <- cacheSolve(cm)    # Get the inverse of m2 (recalculated)
## inverseOfM <- cacheSolve(cm)    # Get the inverse of m2 (as cached)
#########################################################################

## makeCacheMatrix: Create and return an object that can be used to
## calculate and cache the inverse of the provided matrix.  The object
## returned is meant to be used in two ways.  For the sake of example,
## assume that m is a matrix and you create an object with:  
## cm <- makeCacheMatrix(m)
## then, you can:
## 1) calculate the inverse by calling cacheSolve(cm)
## 2) change the matrix that will be inverted by calling cm$set(newMatrix)
##
## Argument: x - a matrix
## Returns: A "cache matrix" object with four methods (get/set/
## setinverse/getinverse) and a privately held, lexically scoped matrix
## and cached inverse of that matrix.  The methods getinverse() and
## setinverse() are meant to be invoked only from within cacheSolve().
## It is safe to call get() and set() at will.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # our lexically scoped cached inverse (if not NULL)
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve:  Returns the inverse of the provided Cache Matrix, using the
## cached copy of the calculation if it exists.  If it does not exist,
## then the inverse will be calculated and cached.
## Argument: x - a Cache Matrix object as returned from makeCacheMatrix()
##           ... additional optional arguements to pass to solve()
## Returns: The inverse of the current matrix held by x
cacheSolve <- function(x, ...) {
  # Gets the cached inverse.  If it's not null, then we have a cached
  # value to return, so return it.
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # If the cached value is null then we have to calculate the inverse
  # and cache it before we can return it.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
