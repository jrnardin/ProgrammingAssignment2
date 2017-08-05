## Programming Assignment #2 in Week #3 of the R Programming Course
## this assignment is to write a set of functions that enable caching of 
## data to reduce the repetition of potentially time-consuming  computations

## personally, I like to put my comments about a function, within the function 
## itself, so I have done that below with just single # instead of double

makeCacheMatrix <- function(x = matrix()) {
      # builds a set of functions and returns the functions within 
      # a list to the parent environment
            
            m <- NULL
            set <- function(y) {
                  x <<- y
                  m <<- NULL
            }
            get <- function() x
            setSolve <- function(solve) m <<- solve
            getSolve <- function() m
            list(set = set, get = get,
                 setSolve = setSolve,
                 getSolve = getSolve)
      }


## still prefer to put comments inside the function being described
## is that lexical commenting?  ;)

cacheSolve <- function(x, ...) {
      #requires an argument that is returned by makeCachMatrix() 
      #function then retrieves the inverse of that matrix from the cached value 
      #that is stored in the makeCacheMatrix() object's environment.
      
      m <- x$getSolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setSolve(m)
      m
}
