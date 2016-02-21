## The first function is designed make a Cache Matrix

## Write a short comment describing this function

makeCacheMatrix <- function(myMatrix = matrix()) { ##Create Function called makeCacheMatrix
  inv <- NULL  ##Create the inv variable and set the value to NULL
  set <- function(y) {  ##Reset the function
    myMatrix <<- y  ##Assign the value of myMatrix to y, from the parent environment
    inv <<- NULL  ##Set variable inv to NULL
  }
  
  get <- function() myMatrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(myMatrix, ...) {
  inv <- myMatrix$getInverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return(inv)
  }
  
  data <- myMatrix$get()
  inv <- solve(data, ...)
  myMatrix$setInverse(inv)
  inv
}



## Write a short comment describing this function

## cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#}
