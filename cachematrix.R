## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL ## sets the inverse, "inv", to NULL as a placeholder for future value
  set <- function(y){ 
    x <<- y ## defines a function to set the matrix "x" to a new matrix "y" 
    inv <<- NULL ## resets the inverse, "inv", to NULL
  } 
  get <- function() x ## returns the matrix "x"
  setinverse <- function(inverse) inv <<- inverse ## sets the inverse, "inv", to "inverse"
  getinverse <- function() inv ## returns the inverse, "inv"
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## returns the vector as a list of all the functions just defined

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() ## assigns to "inv" the value from getinverse(x)
  if(!is.null(inv)){ ## if "inv" is not NULL, return the inverse matrix "inv"
    message("getting cached data") 
    return(inv)
  }
  data <- x$get() ## if "inv" is NULL, assign the matrix x to "data"
  inv <- solve(data) ## calculate the inverse of the matrix x and assign it to "inv"
  x$setinverse(inv) ## sets the inverse of the matrix x calculated in the previous line to "inv"
  inv ## returns the inverse of the matrix x
}
