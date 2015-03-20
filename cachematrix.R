## R Programming class on coursera Programming Assignment 2
## 
## Two functions to cache the inverse of a matrix
## 
## The inverse of a square matrix A is a matrix B such that the product of A and B is the identity matrix I which consists of all values 1.
## A matrix needs to be square to be invertible. Also the determinant of the matrix must not be 0 for the matrix to be invertible.
##
## Usage:
## ## create a 2x2 matrix with values 1 to 4
## a <- makeCacheMatrix(matrix(1:4,2,2))
## ## calculate and cache the inverse matrix
## cacheSolve(a)
## ## calling cacheSolve(a) again returns the cached value and does not calculate the inverse again

## Function makeCacheMatrix 
## creates a cachable matrix object to invert
## takes one argument x which is the matrix to invert
## x needs to be a square matrix and invertible
## returns a list of four functions: set, get, setinv, getinv
## set : sets the value of the matrix (not needed when makeCacheMatrix is called with the matrix x as an argument)
## get : gets the matrix
## setinv : sets the value of the inverted matrix
## getinv : gets the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # inv holds the inverse of matrix x and is initialized to NULL
  set <- function(y) { # set the value of matrix x
    x <<- y # the operator <<- assigns the matrix y to x in the parent environment
    inv <<- NULL # initialize inv in the parent environment to NULL
  }
  get <- function() x # return the matrix x
  setinv <- function(inverse) inv <<- inverse # set the values of the inverted matrix inv in the parent environment
  getinv <- function() inv # return the inverted matrix inv
  list(set = set, # return a list of the four functions
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a cacheable matrix object as an input and returns the inverted matrix
## Repeatedly calling cacheSolve with the same matrix will return the cached value inv instead of calling solve() again
## Inversion of the matrix is done with the built-in function solve()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## calculate and cache the inverse on the first call
  ## return the cached value on every subsequent call
  
  # get the cached inverse for matrix x with getinv
  m <- x$getinv()
  # if the inverse is not NULL, return the result of getinv
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if there is no cached value, calculate the inverse and cache it with setinv
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
