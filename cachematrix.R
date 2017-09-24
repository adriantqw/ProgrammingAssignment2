## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function that creates a chached matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # Reset m to NULL everytime function is called upon 
  m <- NULL
  
  # Create function to redefine the matrix
  set <- function(y){
    
    # Redefine matrix
    x <<- y
    
    # Reset m to NULL because matrix has changed
    m <<- NULL
  }
  
  # Create a function to extract a defined matrix
  get <- function(){
    x
  }
  
  # Create function the define the inverse
  setinv <- function(inverse){
    
    # Define m as the inverse
    m <<- inverse
  }
  
  # Create function to extract the inverse
  getinv <- function() {
    m
  }
  
  # Return a list of functions
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv
  )
}


## Write a short comment describing this function

# Function that solves the inverse of a matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Extract inverse of matrix if it has been calculated
  m <- x$getinv()
  
  # If it has been calculated return the inverse of the matrix
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # Extract the pre-defined matrix
  data <- x$get()
  
  # Calculate inverse of matrix
  m <- solve(data)
  
  # "Cache" the results
  x$setinv(m)
  
  # Return inverse
  m
}
