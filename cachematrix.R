## This functions create first a special "matrix" object that can cache its inverse.
## After this, the next function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## For this functions we assume that the matrix supplied is always invertible





## The first function, makeCacheMatrix creates a special "object", that stores a matrix and caches its inverse which is a list containing:

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix
## 4.  get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
              x <<- y
              m <<- NULL
            }
            get <- function () x
            setInverseMatrix <- function (InverseMatrix) m <<- InverseMatrix
            getInverseMatrix <- function () m
            list(set=set, get=get,
                 setInverseMatrix=setInverseMatrix,
                 getInverseMatrix=getInverseMatrix)

}


## The following function calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse matrix has already been calculated. If it has, the function returns the result from the cache

cacheSolve <- function(x, ...) {
        m <- x$getInverseMatrix()
        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
