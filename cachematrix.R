## Title: Caching the Inverse of a Matrix
## Assignment 2 - R Programming - Coursera.org
## Date: August 22, 2019

## The functions compute the inverse of a matrix and cache the results so it can be recycled for later use.


## PART 1: makeCacheMatrix function
## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list( set = set, get = get, 
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}

## PART 2: cacheSolve function
## This fucntion computes the inverse of the "matrix" created by the above function.
## If an inverse has already been calculated and left unchanged, it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mtrx <- x$get()
  inv <- solve(mtrx, ...)
  x$set_inverse(inv)
  inv
}
