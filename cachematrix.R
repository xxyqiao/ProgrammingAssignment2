## Put comments here that give an overall description of what your
## functions do
##The functions following can calulate the inverse of a matrix 
##and cache the inverse matrix in a cache matrix,
##if you need to get the inverse of same matrix as you did before,
##R will get the cache from the cache matrix 
##instead of calculate it again.
##So it can save time for you in a complex program.
##The right way to use it is as follows:
##input your matrix using makeCacheMatrix() function
##get the cache using cacheSolve() function
##e.g. 
##matrix<-makeCacheMatrix(matrix(1:4,2))
##cacheSolve(matrix)


##The makeCacheMatrix makes a matrix m to 
##cache the inverse matrix of the original input matrix x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                        ##create a set function that will set the input matrix to x
        x <<- y
        m <<- NULL
    }
    get <- function() x                         ##create a get function that will return value of input matrix x
    setinverse <- function(solve) m <<- solve   ##create a get function that will return value of input matrix x
    getinverse <- function() m                  ##create a getinverse function that will return the inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##The cacheSolve function first check if there is cache in the memery,
##if there is a cache matrix meet the requirement,
##which means you've calculated the same matrix before
##it will show "getting cached data" on the screen
##at the same time, it will get the cache and print it.
##if there is not matched cache,
##it will get the original matrix x using get() function,
##calculate the inverse of it,
##then assign it to m 
##and return m
cacheSolve <- function(x, ...) {                
    m <- x$getinverse()                         ##assign m to the inverse of x using getinverse function
    if(!is.null(m)) {                           ##if m is not null
        message("getting cached data")          ##print "getting cacheed data"
    }
    data <- x$get()                             ##assign data to get the input matrix x
    m <- solve(data, ...)                       ##assign m to get the inverse of it
    x$setinverse(m)                             ##cache m using setinverse function
    m                                           ## Return a matrix that is the inverse of 'x'
}

