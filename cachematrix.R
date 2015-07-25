## Two functions to create a special object that stores a matrix and caches its 
## inverse


## This function accepts a matrix argument and returns a list of functions that 
## set and get the matrix as well as inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #set the inverse to NULL
  inv_m<-NULL
  
  #function sets the value of matrix and inverse
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  
  #function that returns the matrix
  get <- function() x
  
  #function that computes the inverse of the matrix
  setinv <- function(solve) inv_m <<- solve(x)
  
  #function that returns inverse of the matrix
  getinv <- function() inv_m
  
  #call all the functions defined above and return the list
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Function that returns the inverse of the special matrix returned by above 
## First checks to see if the inverse has already been computed, if so gets
## from cache, else computes the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #get the inverse
  i <- x$getinv()
  
  #check to see if it is already computed, if so get from cache and return
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #else compute the inverse and return
  data <- x$get()
  i<-solve(data)
  x$setinv(i)
  i
}
