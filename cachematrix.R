## Put comments here that give an overall description of what your
## functions do

## Creates a Matrix pseudo-object with getter and setter as well
## as a inverse matrix variable with it's correspondig getter
## and setter
makeCacheMatrix <- function(m = matrix())
{
    inv <- NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    get <- function() {
        m
    }
    setInv <- function(inverseMatrix) {
        inv <<- inverseMatrix
    }
    getInv <- function() {
        inv
    }
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)

}


## Takes a makeCacheMatrix pseudo-object and calculates it's
## inverse matrix if it doesn't exists yet. If it exists, it
## returns the cached (stored) calculated version
cacheSolve <- function(x, ...)
{
    mat <- x$getInv()
    
    if(!is.null(mat)){
        message("Cached matrix stored, returning")
        return (mat)
    } else {
        message("No cache stored, solving inverse matrix")
        data <- x$get()
        mat <- solve(data)
        x$setInv(mat)
        return(mat)
    }
}

a <- c(0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0)
m <- matrix(a, nrow=5, byrow=TRUE)
mPo <- makeCacheMatrix(m)