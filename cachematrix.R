## 1. The makeCacheMatrix() takes in a matrix as an input and produces a 'cache' containing the matrix and four
## functions, namely, set, get, setinverse, and getinverse. This cache will be in the form of a list that contains
## the said data. 2. This list can be run as an argument in cacheSolve(). 3. The cacheSolve will call a function in 
## the list, getinverse(), to check if a matrix inverse is stored and return that value. If it is not present, 
## cacheSolve will calculate the matrix inverse after calling get() to obtain the matrix and store the inverse in the 
## cache/list by calling the setinverse(). 

## The first function will take a matrix as an input, store the matrix and four functions in the form of a list acting 
## as the cache. It will initialize 'mat.inv' as NULL to remove any previous value every time it is run with a matrix. 

makeCacheMatrix <- function(x = matrix()) {
        mat.inv <- NULL
        set <- function(){
                x <<- y
                mat.inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) mat.inv <<- inv
        getinverse <- function() mat.inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function is to take the list/cache, check if the matrix inverse is stored, and return that value with 
## the message "getting cached data". If not, this function will get the matrix from the list, solve it, store the 
## inverse in the list, and display that value. 


cacheSolve <- function(x, ...) {
        mat.inv <- cache$getinverse()
        if(!is.null(mat.inv)){
                message("getting cached data")
                return(mat.inv)
        }
        matrix.data <- x$get()
        calculate.inv <- solve(matrix.data)
        x$setinverse(calculate.inv)
        calculate.inv
        ## Return a matrix that is the inverse of 'x'
}
