## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The cache matrix object has four methods which can be used to interact with it:   
#•	the get() method, which retrieves the original matrix stored in the cache.
#•	the set(y) method, which updates the original matrix with a new value (y) and clears the cached inverse.
#•	the getinverse() method, which retrieves the cached inverse matrix.
#•	the set setinverse(inverse) method which stores a new inverse matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) {
    inv <<- inverse
  }
  return(list(
    set = set,
    get = get,
    getinverse = getinv,
    setinverse = setinv
  ))
}



## Write a short comment describing this function
# The cacheSolve function takes a cache matrix object (x) as input and does the following.
# •	It checks if the inverse matrix is already cached using x$getinverse().
# •	If the inverse matrix exists in the cache, it directly returns it.
# •	If the inverse matrix is not cached, it calculates the inverse of the original matrix using solve(x$get()).
# •	It stores the calculated inverse in the cache using x$setinverse(m).
# •	Finally, it returns the calculated inverse matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("Retrieving cached inverse")
    return(inverse)
  }
  m <- solve(x$get())
  x$setinverse(m)
  message("Computing inverse and caching it")
  return(m)
}


#3.  Testing the above functions

# Create a square matrix for testing
testMatrix <- matrix(c(1, 2, 3, 4), 2, 2)

print(testMatrix)
#      [,1] [,2]
#[1,]    1    3
#[2,]    2    4



# Create a cache matrix object and set the testMatrix
cacheMatrix <- makeCacheMatrix(testMatrix)

#Note: this object resides in memory


# Compute the inverse using cacheSolve
inverse <- cacheSolve(cacheMatrix)
#Computing inverse and caching it

print (inverse)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1  -0.5



# Retrieve the inverse from the cache
cachedInverse <- cacheMatrix$getinverse()

print(cachedInverse)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

