## These functions define a sequence of events including the
## creation of a list which references and stores properties of a matrix
## and the calculation of its inverse on an as needed basis

## This makeCacheMatrix function defines 4 events:
## The setting of a matrix
## The getting of a matrix
## The setting of its inverse (initialized as Null)
## The getting of its inverse
## It then stores all events in a list to be used by the 
## cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This cacheSolve function takes the list created from the  
## first function and checks that it contains getinverse data 
## with the !is.null(i), if this test returns true, it prints   
## a message and recycles the getinverse data it contains 
## (stored from an earlier run).
## If the result happens to be null, eg. on first run, then it 
## reads in the actual matrix referenced in the list and runs the 
## 'solve' function on it to get new getinverse data which 
## is then stored in the same list.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i        ## Return a matrix that is the inverse of 'x'
}
