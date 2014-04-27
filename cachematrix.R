## Pair of functions to either:
## * Calculate the Inverse of a matrix X and Cache the result for eventual use or
## * Get the already cached value
## This is convenient in cases where the matrix is huge, and its calculation takes a lot of resources


## MakeCachedMatrix creates an special matrix, which is really a list with functions
## to 
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Calculates the Inverse of the Matrix and cache it in m.
## 4. Get the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) m <<- solve    #Only time when it is calculaed.
  get_inverse <- function() m            #Get the value of inverse wich could be NULL or the actaul presolved value
  list(set = set, get = get,             #Returned list with functions.
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## cacheSolve is the function to access a posible cached calculation of the inverse
## Matrix. When trying to get the inverse from the special Matrix created with the function
## above, will check for its prexisting value, if this is null, it will go and calculate. If not, it uses
## what is cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()                           # Get the inverse from the Special Matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)                                    #If not null, means we have a cached solved value.
  }
  data <- x$get()
  m <- solve(data, ...)                          #If there is no cached Inverse, the we calculate.
  x$set_inverse(m)                               #...and cache it for future use.
  m
  
}
