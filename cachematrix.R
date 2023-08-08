## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
##create variable for inverse
  inv <- NULL
  
##create the matrix with a function 
  set_matrix <-function(y) {
    x <<- y 
    inv <<- NULL
  }
  
##get matrix with a function
  get_matrix <- function() x 
  
##set the inverse with a function to an object in a different environment.
  set_inv <- function(inverse) inv <<- inverse

##get the inverse with a function 
  get_inv <- function() inv

##get a list of functions to interact with the matrix
  list(set_matrix = set_matrix, 
       get_matrix = get_matrix,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
  inv <-x$get_inv() ##assign the get_inv value from the makeCacheMatrix function
  
  ##this states that if the value of inv is not null then the inverse from the cache step is retrieved.
  if(!is.null(inv)){
      message("Cached Data")
      return(inv) 
  }
  
  ##creates 'data' object that contains the values from the prior functions get_matrix
  data <-x$get_matrix()
  
  ##sets inv to solve the data.
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
}

##Program Test
##create matrix 1
Data_1 <- makeCacheMatrix(matrix(c(5:8), nrow=2, ncol=2))
##check that this is defined as a list per the function. 
class(Data_1)
##Output: [1] "list"
##Check that the output for this matrix is correct. 
Data_1$get_matrix()
##      [,1] [,2]
##[1,]    5    7
##[2,]    6    8
##try to pull inverse, but this should be null since the cacheSolve hasn't been run yet. 
Data_1$get_inv()
##Output:  NULL 

##obtain inverse using the cacheSolve function
cacheSolve(Data_1)
##     [,1] [,2]
##[1,]  -4  3.5
##[2,]   3 -2.5

##check that this inverse is now available in the list 
Data_1$get_inv()
#      [,1] [,2]
#[1,]   -4  3.5
#[2,]    3 -2.5

##run cacheSolve again; this should pull the 'getting cached data' message
cacheSolve(Data_1)
##Cached Data
##      [,1] [,2]
##[1,]   -4  3.5
##[2,]    3 -2.5


##create matrix 2 
Data_2 <- makeCacheMatrix(matrix(c(10:13), nrow=2, ncol=2))
Data_2$get_matrix()
##Check that the output for this matrix is correct. 
##     [,1] [,2]
##[1,]   10   12
##[2,]   11   13
##try to pull inverse, but this should be null since the cacheSolve hasn't been run for this matrix yet.
Data_2$get_inv()
##Output: NULL

##obtain inverse using the cacheSolve function
cacheSolve(Data_2)
##     [,1] [,2]
##[1,]  -6.5  6
##[2,]   5.5 -5

##check that this inverse is now available in the list
Data_2$get_inv()
##     [,1] [,2]
##[1,]  -6.5  6
##[2,]   5.5 -5

##run cacheSolve again; this should pull the 'getting cached data' message
cacheSolve(Data_2)
##Cached Data
##     [,1] [,2]
##[1,]  -6.5  6
##[2,]   5.5 -5