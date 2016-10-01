## cachematrix.R
## ejcrotty@gmail.com
##
## inverting a matrix could take a while if it is large - so using a cache would eliminate
## doing it more than once.  Which would save possibly a lot of time.
##  we use the same structure as the example with the cached means of a vector
##  except this is for a matrix and uses the solve function to invert

##The first function, makeCacheMatcrix  is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse value of  the matric
## get the inverse value of the matrix


makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
      x <<- y
      invr <<- NULL
      }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## cacheSolve uses the solve function to invert the matrix
## OR
## if it has already been solved, it will return it from cache 
## instead of doing it again ( solve could take a while if the matrix is big ) 
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invr <- x$getinverse()
## if it already exists        
  if(!is.null(invr)) {
    message("getting cached data.")
    ## return the already existing      
    return(invr)
  }
  ## otherwise use solve to invert 
  ## copy it into "data"      
  data <- x$get()
  ## invert "data" and store it in invr      
  invr <- solve(data)
  ## call the setinverse function to store it global      
  x$setinverse(invr)
  ## return it!      
  invr
}
