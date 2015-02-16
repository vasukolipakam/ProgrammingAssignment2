# Date : 16 Feb 2015
# Author : V Kolipakam
# Assignment 2

#The first function, makeVector creates a special "vector", 
#which is really a list containing a function to
#a) set the value of the vector
#b) get the value of the vector
#c)set the value of the mean
#d) get the value of the mean

makeCacheMatrix = function(x = matrix()) {
  inver = NULL
  set = function(z) {
    x <<- z
    inver <<- NULL
  }
  get = function() x
  set_inverse = function(inverse) inver <<- inverse
  get_inverse = function() inver
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

## The following function calculates the mean of the special "vector" 
##created with the above function. However, it first checks to see if the mean 
##has already been calculated. If so, it gets the mean from the cache and skips 
##the computation. Otherwise, it calculates the mean of the data and sets the value 
##of the mean in the cache via the setmean function.


cacheSolve = function(x, ...) {
  inver = x$get_inverse()
  if(!is.null(inver)) {
    message("Checking Cached Data...")
    return(inver)
  }
  z = x$get()
  inver = solve(z)
  x$set_inverse(inver)
  inver
}