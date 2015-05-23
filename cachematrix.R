## Here the functions are used to cache the inverse of a square matrix

## makeCaheMatrix function creates a spectial matrix object that can cache
## its inverse. It includes four functions, set, get, setinverse, getinverse.


makeCacheMatrix <- function(x = matrix()) {
  # check is the matrix is square matrix or not
  if (ncol(x)!= nrow(x)){
    message ("This program is used for square matrix")
  }
  i <- NULL    # i is the inverse of the matrix 
  
  # set: set the data of the matrix 
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  # get:  get the value of the matrix, 
  # here return x  which is the vale of the matrix
  get <- function() x
  
  # setinverse: set the inverse of the matrix
  # and setting the value of its inverse to i (return i)
  setinverse <- function(inverse) i <<-inverse
  
  # getinverse: get the inverse of the matix to i
  getinverse <- function() i
  
  # return the results as a list contain these four function
  list( set = set, get = get, setinverse = setinverse,getinverse =getinverse)
  
}


## cacheSolve function is to computes the inverse of the special matrix
## returned by makeCacheMatrix above. 
## If the inverse has already been calculate, get the pre-exist results directly
## If not, solve the matrix and get the results.

cacheSolve <- function(x, ...) {
 
  i <- x$getinverse()
  # check if the inverse has already been calculate
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  # if not, solve the results
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}

## Sample test result (if the matrix you enter doesn't have inverse, 
## it will return a error)
## > test<-makeCacheMatrix(matrix(c(1,3,3,4,5,6,7,8,9), nrow=3, ncol=3))
## > cacheSolve(test)
##      [,1] [,2]      [,3]
##[1,] -0.5    1 -0.500000
##[2,] -0.5   -2  2.166667
##[3,]  0.5    1 -1.166667
