## This function has 2 subfunctions: cachecheck(), cacheSolve()
## First we define all the functions, and the call the 1st one, which in its 
## turn calls the 2nd. I purposefully wrote this in a 
## different way than the makeVector()/cachemean() ones, because the code seems to be 
## more intuitive this way. It still does what it should - calculates the
## inverse of the matrix, or shows the cache of last calculation if I ask for
## the inverse of the same matrix. 

makeCacheMatrix <- function (x=matrix())
  
{
  ##let's define a checking cache function first
  cachecheck <- function() {
    #checking if the last used X and current X are the same
    if (identical(x,used_x)) {
      cacheSolve(x,T)
      ## calling the solve function with "cached=true" argument
    } else
      cacheSolve(x)
    ##else calling it with "false" one
  }
}

  ##now let's define a function that prints a cache
cacheSolve <- function(x, cached=F)
{
  {
    ##if it's called with TRUE parameter, it just shows the cache for prev.result
    if(cached==T) {
      message("getting cached data")
      return(cache) } else
        ##and if called with FALSE one, it actually solves the cache. 
    result <- solve(x)
    ##after calculating the cache it updates the cache and the last used X parameters
    used_x <<- x
    cache <<- result
    ##and exits with a result
    return(result)
  }
  ##Now let us start the whole thing, calling the cachecheck() function
  cachecheck()
}