##Assignment: Programming Assignment 2: Lexical Scoping
##The two functions calculate the inverse of a matrix and saves it to cache
##When use attemps to calclate matrix inverse, cached value is used instead of repeating calculation


##First Function - "makeCacheMatrix" - This function creates a special "matrix" object that can cache its inverse
##Creates a scedial "matrix" object, which is a list containing a function to do the following:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub-functions/methods
  
  ## define the cache m
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the
    ## parent environment
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal to the inverse of the matrix x
  getinverse <- function() m ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Second Function - "cacheSolve" - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
    
  m <- x$getinverse()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
