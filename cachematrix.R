## Creates a Matrix pseudo-object with getter and setter as well
## as a inverse matrix variable with it's correspondig getter
## and setter
makeCacheMatrix <- function(m = matrix())
{
    inv <- NULL             # Variable to hold the inverse matrix

    # Sets the matrix to a new value and sets the inverse to NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }

    #Gets the matrix currently stored
    get <- function() {
        m
    }

    # Sets the inverse matrix
    setInv <- function(inverseMatrix) {
        inv <<- inverseMatrix
    }

    #Gets the inverse matrix
    getInv <- function() {
        inv
    }

    #Publish the available functions
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
    # Stores a makeCacheMatrix object
    mat <- x$getInv()
    
    # Check if there is a inverse matrix stored
    if(!is.null(mat)){
        message("Cached matrix stored, returning")
        return (mat)
    } else {
        #When no cache is stored, calculate and set it
        message("No cache stored, solving inverse matrix")
        data <- x$get()
        mat <- solve(data)
        x$setInv(mat)
        return(mat)
    }
}