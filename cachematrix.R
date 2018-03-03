## Coursera - R Programming - week 3 - programming assignment 2
## Demonstrating Lexical Scoping --> CacheInverseMatrix.R 

## The first function, makeCacheMatrix takes the input as a matrix and creates 
## a special "matrix" object that can cache its inverse

## This function performs below tasks
## set the value of the matrix
## get the value of the matrix
## set the the inverse matrix
## get the the inverse matrix

## Assuming that the matrix argument will always be invertible

makeCacheMatrix <- function(x = matrix()) {
      ## initialize inv.matrix as NULL, it will hold the value of inverse matrix
      inv.matrix <- NULL                        
      ## set the new value of the matrix in parent environment if there is any new matrix
      set <- function(y){
            x <<-y
            ## if there is a new matrix, reset the inv.matrix as null
            inv.matrix <<-NULL
      }
      ## get function returns the value of the matrix arguments supplied
      get <- function() x
      
      ## set the value of inv.matrix in parent environment
      setinverse <- function(inverse) inv.matrix <<- inverse
      
      ## get the value of inv.matrix where it is called
      getinverse <- function() inv.matrix
      
      list(set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse)

}


#########################################################################################

## The following function cacheSolve computes the inverse of the special "matrix" 
## returned by the makeCacheMatrix function. 

## It first checks to see if the inverse has already been calculated and the 
## matrix has not changed, then cacheSolve retrives the value of the inverse from
## the cache and returns that

## If the inverse is not already computed and not present in cache, it calculates 
## the inverse and sets the value of the inverse in the cache via the 
## setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      ## Assign the value of the inv.matrix from the outcome of previous function
      ## It assigns NULL for the 1st run
      inv.matrix <- x$getinverse()
      
      ## Check if inv.matrix has any cached value. 
      ## If there is any cached value, it retunrs that value as inverse matrix
      if(!is.null(inv.matrix)){
            message ("Getting the cached result")
            return(inv.matrix)
      }
      
      ## If there is no cached value for inv.matrix, the function computes it 
      ## and return the inverse matrix
      data <- x$get()
      inv.matrix <- solve(data,...)
      x$setinverse(inv.matrix)
      
      inv.matrix
}


## test with [4*4] matrix
test.matrix <- matrix(rnorm(16),4,4)
test.matrix
cache.matrix <- makeCacheMatrix(test.matrix)
cache.matrix$get()
cache.matrix$getinverse()
cacheSolve(cache.matrix)
cacheSolve(cache.matrix)
