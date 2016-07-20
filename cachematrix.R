## takes an invertible matrix and returns an object which has functions
## for storing its value and inverse in order set,get,setinverse,getinverse

## create an invertible matrix (invertible matrix is a square matrix)
## a <- matrix(1:4,2,2)
## test cacheMtrxVar <- makeCacheMatrix(a)
## test cacheSolveVar <- cacheSolve(makeCacheMatrix(a))
## output cacheSolveVar
        ##      [,1] [,2]
        ## [1,]   -2  1.5
        ## [2,]    1 -0.5

## reverse it back to test 
## c <- cacheSolve(makeCacheMatrix(cacheSolveVar))
makeCacheMatrix <- function(x = matrix()) {
  # Set inverse of X to NULL
  xInverseVar <- NULL

  # function to set the value of x 
  set <- function(y) {
    x <<- y
    xInverseVar <<- NULL
  }

  #function to return the value as it is of x
  get <- function() x

  #function to set the value of inversevariable
  setInverse <- function(inverse) xInverseVar <<- inverse

  #function to get the inverseVar
  getInverse <- function() xInverseVar

  #List is returned with set, set, setInverse,getInverse functions
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## Takes input of makeCacheMatrix object, 
## if the inverse doesn't exist check by getInverse function, then set it 
## using the setInverse function, calculate the inverse using solve(matrixObject) function 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInverse <- x$getInverse()

        #Check if the given inverse is not null, if not null then return it
        if(!is.null(xInverse)) {
        print("Cached Value of Invertible matrix...")
        return(xInverse)
        }

        #comes here only if xInverse was found null, get matrix, use solve function, 
        #set to using setInverse function and return the xInverse object  
        matrixObj <- x$get()
        xInverse <- solve(matrixObj, ...)
        x$setInverse(xInverse)
        xInverse
}
