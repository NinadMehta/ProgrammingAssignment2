#TITLE  : R programming Assignment 2 -> Caching the Inverse of a Matrix
#AUTHOR : Ninad Mehta
#DATE   : 12/26/2015

# This function creates a special "matrix" object that can cache its inverse, which is really a list containing a functions to:
#1. Set the data for the matrix
#2. Get the data for the matrix
#3. Set the inverse of the matrix
#4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverseVar <- NULLQ
    #This function is used to set the data for the matrix
    set <- function(y) {
        x <<- y
        inverseVar <<- NULL
    }
    
    #This function is used to get the data for the matrix
    get <- function() x
    
    #This function is used to cache the data for the inverse of the matrix
    setInv <- function(inv) inverseVar <<- inv
    
    #This function is used to get the cached data for the inverse of the matrix
    getInv <- function() inverseVar
    
    #Returns the list of the functions
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


# This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inverseVar <- x$getInv()
    
    #Check if the inverse has already been done and retrieve the cached inverse matrix if it is done.
    if (!is.null(inverseVar)) {
        message("getting cached data")
        return(inverseVar)
    }
    
    #If inverse is not cached earlier, get the matrix data
    mat <- x$get()
    
    #Perform the matrix inverse and save the inverse in inverseVar
    inverseVar <- solve(mat, ...)
    
    #Call setInv to cache the inverse of matrix
    x$setInv(inverseVar)
    
    #Return data from inverse matrix
    inverseVar
}