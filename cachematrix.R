## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Functions for creating and using inverted matrices which caching ability

makeCacheMatrix <- function(x = matrix()) {

  
  ## Creates cacheable matrix for inputting to
  ## cacheSolve() function which sets and gets
  ## the cached values
  
  makeCacheMatrix <- function(original.matrix = matrix()) {
    
    
    ## let's check if we have correct input
    
    if(!is.matrix(original.matrix)) {
      stop("Please create a matrix")
    }
    
    inverted.matrix <- NULL
    
    set <-function(y) {
      original.matrix <<-y
      inverted.matrix <<-NULL
      
      
    }
    
    ## functions for gettin and setting cached inv. matrix value
    get <- function() original.matrix
    
    ##inversing the matrix using build in solve() function in R
    
    set.inverse <-function(solve) inverted.matrix <<- solve
    get.inverse <- function() inverted.matrix
    
    list(
      set = Set, 
      get = get,
      set.inverse= set.inverse,
      get.inverse= get.inverse)
    
  }
  
    
  
}



## Write a short comment describing this function
## computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## if the inverse has already been calculated and there's no change in the matrix
## the tha cacheSolve() returns the cached inverse


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  cacheSolve <- function(cacheable.matrix, ...) {
    inverted.matrix <- cacheable.matrix$get.inverse()
    #do we have cached matrix available?
    if(is.null(inverted.matrix)) {
     message("Getting cached inverse matrix")
      return(inverted.matrix)
      
    }
    ## let's create inverted matrix in case
    ## there is no cached matrix available
    
    matrix.to.inverse <-cacheable.matrix$get()
    inverted.matrix<- solve(matrix.to.inverse)
    cacheable.matrix$set.inverse(inverted.matrix)
    inverted.matrix
    
    
  }
}


