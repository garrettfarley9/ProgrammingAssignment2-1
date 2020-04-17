#make a list of each element being a function
makematx <- function(x = matrix()) {
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

#if the mean has been calculated, it returns the mean. if not, it calculates
#it and caches it
cachematx <- function(x, ...) {
        m <- x$getmatx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatx(m)
        m
}
test<- matrix(data= c(4,2,7,6), nrow=2, ncol=2)