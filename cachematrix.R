## Assumption: Matrix used will always be invertible

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
## It actually creates a list containing a function to
## set the value of the Matrix => achieved by set function
## get the value of the Matrix => achieved by get function
## set the value of the Inverse => achieved by setinv function
## get the value of the Inverse => achieved by getinv function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## retrieves the inverse from the cache.It prompts the message as "getting cached data" in that case.
## Built-in function "solve" as used below computes inverse of matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setinv(i)
  i
}

## Steps to execute/test the functions
## 1. create a 2x2 matrix by issuing  the command > mat <- makeCacheMatrix(matrix(1:4,2,2))
## 2. Check the value of the matrix by issuing the command > mat$get() ## This should return 2x2 matrix
## 3. Check the inverse of the matrix by issuing the command > mat$getinv() ## This should return empty because nothing has been cached till now.
## 4. Now, get the Inverse of Matrix by issuing the command > cacheSolve(mat) ## This should give you the inverse of the above matrix
## 5. If you issue the same command again, you will see the same result, plus a message "getting cached data"
## 6. Now, if you issue the exact same command as used in step 3 here, you will see the inverse of matrix ## This is because inverse has now been cached.
## 7. Changing the input i.e. Step 1 here will flush away the content and the results have to be cached again.
