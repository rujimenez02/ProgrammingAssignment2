## Used the samples proided as a template for the creation of these functions


##  makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinverse = function(inverse) m <<- inverse 
  getinverse = function() m
  list(set=set, 
       get=get, 
       setinverse=setinverse,
       
       getinverse=getinverse)
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  m = x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m) 
  }
  
  data = x$get()
  m = solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(m)
  
  return(m)
  
}



test = function(mat) {
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
}
