## These functions are used when we want to compute and to cache the inverse of a matrix
## (if this inverse has not been cached yet). 
## If the inverse of the matrix we pass to the functions has been already 
## calculated and so cached, at the end we had it from the cache.
## If not, the functions calculate the inverse, store it in the cache for next time 
## we ask for it and in the meantime return it to us.

## This function creates a matrix to cache its inverse and returns a list of function 
## to set and get the value of the matrix and to set and get the inverse 
## of that matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set=set, get=get,
       setinv=setinv, getinv= getinv)
}

## This function checks, via the getinv function, if the inverse of a matrix 
## is already present in the cache. If not it calculates the inverse of the matrix
## and sets it to the cache via the setinv function and return it via getinv function.
## If yes it returns the inverse via getinv function.
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(is.null(x$getinv())){
    message("setting data in the cache and getting them")
    x$setinv(solve(x$get()))
  }
  x$getinv()
}
