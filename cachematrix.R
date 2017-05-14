# May 14th, 2017
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly. The following two functions are used to cache the inverse 
# of a matrix.

# makeCacheMatrix function creates a list containing a function to
# 1. set value of matrix
# 2. get value of matrix
# 3. set value of inverse of matrix
# 4. get value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cachemean function returns the inverse of a matrix. It first checks if the inverse has already been 
# computed. If so, the function gets the result and skips the inversion calculation. If not valid, it 
# calculates the inverse of the matrix, and sets the value in the cache using the setinverse function.

# function assumes the matrix is always invertible

cacheSolve <- function(x, ...) {
        invs <- x$getinverse()
        if(!is.null(invs)) {
                message("getting cached data")
                return(invs)
        }
        data <- x$get()
        invs <- solve(data)
        x$setinverse(invs)
        invs
}


# Solution by theory:

#Find the inverse of the matrix A = ( 3 1
#                                     4 1)
#
#                       1            (2  -1
# Formula A^-1 = _________________ * 
#               ((3)(2) - (1)(4))    -4  3)
#
#                  1      (2  -1)           (1  -0.5
#              =  ___  *              0r                 as per example below on "cacheSolve(invs)" call
#                  2      -4   3)           -2   1.5 ) 



# Solution by cacheSolve(invs) function:
  
#  > x <- matrix(c(3,4,1,2),2,2)
#  >  invs <- makeCacheMatrix(x)
#  >  invs$get()
#           [,1] [,2]
#     [1,]    3    1
#     [2,]    4    2
#
#  > cacheSolve(invs)
#           [,1] [,2]
#     [1,]    1 -0.5
#     [2,]   -2  1.5
#  
#  > cacheSolve(invs)#
#    getting cached data
#           [,1] [,2]
#     [1,]    1 -0.5
#     [2,]   -2  1.5 
  
  
  
  
  
  
  
  
  




