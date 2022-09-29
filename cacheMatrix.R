## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {       # Creates a special matrix called makeCachematrix which is a list containing to set,get the matrix and to set and get inverse of the same.
inv <- NULL                                       # setting inv to NULL
  set <- function(y) {                            # set function for matrix creation
    x <<- y                                       # assigning y to x
    inv <<- NULL                                  # setting inv to NULL
  }
  get <- function() x                             # get function for matrix 
  setInverse <- function(inverse) inv <<-inverse  # set function for matrix inverse
  getInverse <- function() inv                    # get function for matrix creation
  list(set = set, get = get,                      # Creation of list for all functions
       setInverse = setInverse,
       getInverse = getInverse)
}                                                 # End of makeCacheMatrix function
        

## Write a short comment describing this function

cacheSolve <- function(x, ...) {                  # Create cacheSolve function
     
  inv <- x$getInverse()                           # setting value of getinverse of x to inv
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }                                              # Checking inv exists and printing message and returning the inv value
  data <- x$get()                                # setting value of getmatrix of x to data
  inv <- solve(data, ...)                        # assigning value of solve of x to inv
  x$setInverse(inv)                              # passing inv to setInverse of x
  inv                                            # printing the value of x
}                                                # End of cacheSolve function
