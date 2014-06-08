## The following program codes two functions that allow the user to cache a matrix and its inverse in a special object:
## makeCacheMatrix()
## cacheSolve()
## At the end of this document are examples of application of the functions


######################################################################################################################
## makeCacheMatrix()
## This function initialize the special object with a given matrix as an input
## The output is a list of four functions : 
## set() caches the the matrix in the special object
## get() returns the cached matrix
## setinv() caches the inverse of the matrix in the special object
## getinv() returns the inverse of the matrix if already computed (null if not)

makeCacheMatrix <- function(x = matrix()) {
  #testing the class of the input
  if(class(x)!="matrix"){
    message("object is not a matrix")
    return(NULL)
  }
  #if input of class "matrix" : setting values of the matrix and its inverse
  solved.matrix <- NULL
  set <- function(y) {
    x <<- y
    solved.matrix <<- NULL
  }
  get <- function() x
  setinv <- function(solve) solved.matrix <<- solve
  getinv <- function() solved.matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
######################################################################################################################

######################################################################################################################
## cacheSolve()
## Return a matrix that is the inverse of 'x' and caches it in the special object created by makeCacheMatrix()
## If first call to the function: computes the inverse of the matrix
## If not the first call: fetches the inverse already cached in the special object

cacheSolve <- function(x, ...) { 
  #setting value for the inverse (NULL if first call to the function, else: inverse of the matrix)
  solved.matrix <- x$getinv()
  #if not the first call to the function: no need to compute the inverse, just return the inverse already cached
  if(!is.null(solved.matrix)) {
    message("getting cached data")
    return(solved.matrix)
  }
  #if first call to the function: compute the inverse of the cached matrix
  data <- x$get()
  solved.matrix <- solve(data, ...)
  x$setinv(solved.matrix)
  solved.matrix
}
######################################################################################################################

######################################################################################################################
## Examples of application
d<-makeCacheMatrix(matrix(c(2,1,5,6,3,5,2,3,0), nrow=3))
d$get()
d$getinv()

cacheSolve(d)
cacheSolve(d)

b<-makeCacheMatrix(c(1,2,3))

