## Overall description of function is to write a pair of functions that cache the inverse of a matrix.

## The first function makecatcheMatrix is matrix list containing a function to execute the below:
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of inverse of the matrix
##4.	get the value of inverse of the matrix

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-Null
    
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## Next function computes the inverse of the  "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##This can be done with the solve function as below
cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
#Sample tests
#Create an invertible matrix of 3 x 3
x <- matrix(c(1,2,3,0,1,4,5,6,0), nrow=3)

# Print x
x

# Make Cache Matrix of x and store in y
y <- makeCacheMatrix(x)

# Inverse x using cacheSolve
cacheSolve(y)
