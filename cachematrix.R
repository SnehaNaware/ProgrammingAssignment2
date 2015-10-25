## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
## assignment is to write a pair of functions that cache the inverse of a matrix

## Below are two functions that are used to create a special object that stores a numeric vector and cache's its Inverse Matrix.

makeCacheMatrix <- function(x = matrix()) {  #This function creates a special "matrix" object that can cache its inverse
#Set the value of the vector
inv <- NULL 
    set <- function(y) { 
         x <<- y 
         inv <<- NULL 
     } 
#Get the value of the vector
     get <- function() x 
     setinverse <- function(inverse) inv <<- inverse   #Set the value of the Inverse Matrix
     getinverse <- function() inv  #Get the value of the Inverse Matrix
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}
#Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'
       
        inv <- x$getinverse() 
     if(!is.null(inv)) { 
         message("getting cached data.") 
         return(inv) 
     } 
     data <- x$get() 
     inv <- solve(data) # It returns its inverse value
     x$setinverse(inv) 
     inv 
}

# Do some check on Ineverse matrix  
 # y = rbind (c( 4 , 7) , c (2,6))
  # m = makeCacheMatrix(y) 
 # m$get() 
# cacheSolve(m)

