## These functions allow an inverse matrix to be cached and retrieved later

## Pass the matrix into the makeCacheMatrix function and assign it
makeCacheMatrix <- function(x = matrix()){
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     set.inv <- function(sol) inv <<- sol
     get.inv <- function() inv
     list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}

## Call the cacheSolve function on the object with the result of makeCacheMatrix
cacheSolve <- function(x){
     inv <- x$get.inv()
     if(!is.null(inv)){
          message("getting chached data")
          return <- inv
     }
     mtrx <- x$get()
     inv <- solve(mtrx)
     x$set.inv(inv)
     inv
}