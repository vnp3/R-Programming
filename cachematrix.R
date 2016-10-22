## Hello. The following code includes 2 functions - makeCacheMatrix and cacheSolve - where the collective
## goal is to calculate and 'cache' - store - the inverse of a user-inputted matrix, and then return the 
## inverse of that cache value. 

## makeCacheMatrix: function, with arguments to accept user-input and store/cache value for future 
##callbacks, as well as user-created sub-functions to calculate and retrieve the inverse value of
##cache value. 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setValue <- function(mtx) {
                x <<- mtx
                inverse <<- NULL
        }
        getValue <- function() return(mtx)
        setInv <- function(inv) inverse <<- inv
        getInv <- function() return(inverse)
        return(list(setValue = setValue, getValue = getValue, setInv = setInv, getInv = getInv))

}


## cacheSolve: function, inputting matrix from makeCacheMatrix, arguments for calculating inverse from
## makeCacheMatrix. Includes control argument, to test whether inverse has already been calculated. 
## If previously calculated inverse exists, cacheSolve will recall that cache value. 

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- mtx$getInverse
        ## Control: Has inverse already been calculated? Test by determining if value of inverse has
        ## been changed from initial call; if Value has changed, this function will return new inverse
        ## value, with the message "getting cached data".
        if (!is.null(inverse)) {
                return ("Getting Cached Data...")
                return (inverse)
        }
        ## If above statement is F (inverse is still NULL), these next lines utilize makeCacheMatrix 
        ## sub-arguments to solve for inverse matrix. After solving, the result is stored in inverse,
        ## as cache for potential future re-calls. 
        data <- mtx$get()
        inverse <- solve(data, ...)
        mtx$setInverse(inverse)
        return(inverse)
        
}
