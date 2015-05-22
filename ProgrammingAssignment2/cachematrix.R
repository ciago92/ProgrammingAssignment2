# This program is designed to check if the result of an inverse matrix calculation is cached before actually
# running the calculation

# This function creates a list with functions for setting and getting the values of the matrix as well as its inverse
makeCacheMatrix <- function(input = matrix()) {
  inverse <- NULL
  set <- function(T) {
    input <<- T
    inverse <<- NULL
  }
  get <- function() input #Get the input
  setInverse <- function(inverse2) inverse <<- inverse2 #Store the inverse
  getInverse <- function() inverse #Retrieve the inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) #Package it up
}

# We are told to assume that the matrix is always invertible, so no error handling is needed.
cacheSolve <- function(input, ...) {
  # Get the cached value, if it exists
  inverseMatrix <- input$getInverse()
  # Test if it exists
  if(!is.null(inverseMatrix)) {
    #If it already exists, declare as such and return it
    message("Retrieving the cached data.")
    return(inverseMatrix)
  }
  #Otherwise, get the original input
  inputMatrix <- input$get()
  #Invert it
  inverseMatrix <- solve(inputMatrix)
  #Cache it
  input$setInverse(inverseMatrix)
  #And return it
  inverseMatrix
}
