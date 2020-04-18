## These two functions work together to first create a list of functions
##to store a cached matrix, then returns an inverse matrix either from the cache
##or it solves for the inverse matrix and then stores it in the cache. 

#Creates a list of functions "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setmatx <- function(matx) m <<- matx
                getmatx <- function() m
                list(set = set, get = get,
                     setmatx = setmatx,
                     getmatx = getmatx)
        }
        
## Returns a matrix that is the inverse of 'x' either from cache or solve
cacheSolve <- function(x, ...) {
                m <- x$getmatx()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)#solve(a) will return inverse of matrix(a)
                x$setmatx(m)
                m
        }
        
my_matrix<-makeCacheMatrix(test)
test<- matrix(data= c(4,2,7,6), nrow=2, ncol=2)