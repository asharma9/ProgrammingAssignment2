##### "makeCacheMatrix" creates a special matrix object
#### cacheSolve calculates inverse of the matrix 
## I calculate inverse of the matrix using "solve"
####The covention I have used for it are "p" and "q" as  two matrix 


makeCacheMatrix <- function(p = matrix()) {
  inv_p <- NULL
  set <- function(q) {
    p <<- q
    inv_p <<- NULL
  }
  get <- function() p
  setinverse<- function(inverse) inv_p <<-inverse
  getinverse <- function() inv_p
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#####cOMPUTING THE INVERSE OF THE SPECIAL MATRIX FROM THE ABOVE STEP
cacheSolve <- function(p, ...) {
  ## Return a matrix that is the inverse of 'p'
  inv_p <- p$getinverse()
  if (!is.null(inv_p)) {
    message("getting cached inverse matrix")
    return(inv_p)
  } else {
    inv_p <- solve(p$get())
    p$setinverse(inv_p)
    return(inv_p)
  }
}
