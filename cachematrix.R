#This script contains two functions: makeCacheMatrix and cacheSolve.
#
#makeCacheMatrix is an object-function- that has a matrix as argument.The object 
#       contains four functions/methods: set,get,setInverse and getInverse 
#       that are accessed from a list in which each element is named as 
#       the function it calls. 
#       This function stores the matrix passed through the initialization 
#       argument in the variable mtxInput, and returns a list of functions:
#       -The 'set' function sets its argument into the variable mtxInput,
#        and sets mtxInv-stored in cacheSolve- to NULL
#       -The 'get'function returns mtxInput.
#       -The 'getInverse' returns mtxInv, a variable stored in cacheSolve
#       -The 'setInverse' stores its argument in mtxInv-cacheSolve-.
#
#cacheSolve is a function that uses a makeCacheMatrix object as input. Its
#       objective is to return, and store in mtxInv, the inverse of the matrix 
#       contained in the makeCacheMatrix used as input.It will only calculate
#       the inverse when it has not been calculated before.



#Stores an input matrix (argument) and returns a list containing a function to
#'get' returns input matrix-mtxInput
#'set' resets the input matrix-mtxInput, and reset mtxInv to NULL
#'getInverse' returns mtxInv from cacheSolve (inverse of mtxInput if
# cacheSolve already executed against current object-mtxInput- else NULL) 
#'setInverse' sets the argument into cacheSolve mtxInv
makeCacheMatrix <- function(mtxInput = matrix()) {
        
        mtxInv <- NULL
        
        set <- function(y) {
                mtxInput <<- y
                mtxInv <<- NULL
        }
        
        get <- function() mtxInput
        
        setInverse <- function(inv) mtxInv <<- inv
        
        getInverse <- function() mtxInv
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


#cacheSolve is a function that uses a makeCacheMatrix object as input. Its
#       objective is to return and store in mtxInv the inverse of the matrix 
#       contained in the makeCacheMatrix used as input.It will only calculate
#       the inverse and store it in mtxInv when it has not been calculated
#       before (if calculated before, it is already stored in mtxInv,and
#       the function gives a message that is getting cached data).
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        mtxInv <- x$getInverse()
        if(!is.null(mtxInv)) {
                message("getting cached data")
                return(mtxInv)
        }
        data <- x$get()
        mtxInv <- solve(data)
        x$setInverse(mtxInv)
        mtxInv

}
