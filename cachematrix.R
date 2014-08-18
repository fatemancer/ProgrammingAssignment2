## These functions calculate the inverse of the given matrix
## The first one creates an object that stores some given matrix along with some methods to manipulate it
## and the second one allows accessing this data and doing calculations.
## I also add 1 function for testing.
## The details follow

## This is a function for quick testing. 
## Note: it always recalculates the inverse, because each MakeCacheMatrix() call at its beginning  erases the cache. 

cacheMatrix <- function(x = matrix())
{
  
temp <- makeCacheMatrix(x)
cacheSolve(temp)

}

## This function creates an object that contains data and some methods to manipulate it. 
## The contents should be output to an object (e.g. my_matrix_cached <- makeCacheMatrix(my_matrix))
## The object should be passed to the cacheSolve() function
## More detailed explanation of the function inner work follow in the code

makeCacheMatrix <- function(x = matrix()) {

  # cleaning the cache
  m <- NULL 

  # creating a function to set the matrix to invert
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #
  
  # creating a function to get the current matrix to invert
  get <- function() x
  
  # creating a function to put the inverted value to the cache
  setsolve <- function(solve) 
  {
    m <<- solve 
  }
  
  # creating a function to take the inverted value from the cache
  getsolve <- function() m

  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  
}

## This function is to (re)create the matrix inverse any thime you like
## It takes the list from makeCacheMatrix() as an argument.
## More detailed explanation follows


  cacheSolve <- function(x, ...) {
    
    # we try to get the cache
    m <- x$getsolve()
    
    # if the cache is not null (something is there), we take it out and print it along with the message about retrieving the cache
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    } else 
      
    # if the cache is null then we get the data about what matrix to process from the argument and calculate the inverse. 
    # for our purpose we do not need the (...) from the example - 
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    
    # then we print the result
    m
}

