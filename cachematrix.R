## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
# Allows to get & set a matrix (A) and its inverse (∀)
# 1. set the value of the matrix A
# 2. get the value of the matrix A
# 3. set the value of the inverse matrix ∀ 
# 4. get the value of the inverse matrix ∀

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## CacheSolve 
# receives a cached version of an inversed matrix for a created makeCacheMatrix object
# 1. recieve the inverse matrix m of original matrix x
# 2. if the inverse does already exist (is not NULL)
# 2a  return already calculated inversed matrix m
# 3. if m is NULL (no inverse existing), get the cachedMatrix (data)
# 4. calculate the inverse and save it as m
# 5. set the cached matrix in makeCacheMatrix to m with setinverse so it can be retrieved next time
# 6. return m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inversed matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
