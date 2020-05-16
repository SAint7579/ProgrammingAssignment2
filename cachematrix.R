## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ##Initializing the value of inverse
  inverse <- NULL
  set <- function(y)
  {
    ##Superassigning the value of y to x
    x <<- y
    inverse <<- NULL
  }
  
  ##Getting the value of the matrix
  get <- function() x
  
  ##This function puts the value of inverse in the varaible
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  ##This function returns the inverse
  getinverse <- function() inverse
  
  #returning the list with the functions
  list(set = set, get =get, setinverse = setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        #checking if the value is already chached in x
        if(!is.null(inverse))
        {
          message("getting cached data")
          #Break and return if the code exists 
          return(inverse)
        }
        
        data <- x$get()
        #Calculating the inverse of the matrix
        inverse <- solve(data)
        ##Setting the value for inverse for storing in chache
        x$setinverse(inverse)
        inverse
}


#OUTPUT:
#> mat <- matrix(1:4, nrow = 2, ncol = 2)
#> v <- makeCacheMatrix(mat)
#> cacheSolve(v)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(v)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
