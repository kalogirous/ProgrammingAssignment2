#This function has as input a matrix and outputs a special matrix 
#which is a list of four functions: setting and getting itself and
#setting and getting its inverse. The setting function in both cases 
#caches the matrix and its iverse, respectivelly
makeCacheMatrix <- function(x = matrix()) {
    #declare inverse matrix of 'x'
    inv.m <- NULL
    
    #cach x
    set <- function(y) {
        x <<- y
        inv.m <<- NULL
    }
    
    #retrieve cached matrix
    get <- function() x
    
    #cache inverse martrix of 'x'
    setinv <- function(inv.matrix) inv.m <<- inv.matrix
    
    #retrieve inverse martrix of 'x'
    getinv <- function() inv.m
    
    #create fuction's output as list of four functions
    list(set=set, get=get, setinv=setinv,getinv=getinv)
}

#Return a matrix that is the inverse of input martix 'x'
#The inverse matrix is calculated using the base function 
#solve with one argument (a)
cacheSolve <- function(x, ...) {
    #check if cached inverse matrix of 'x' exists    
    inv.matrix <- x$getinv()
    
    #if cached inverse matrix of 'x' exists then return it as the function's output
    if(!is.null(inv.matrix)) {
        message("Getting cached inverse matrix")
        return(inv.matrix)
    }
    
    #get the cached matrix
    data <- x$get()
    
    #caclulate the inverse of the matrix 'x'
    inv.matrix <- solve(data, ...)
    
    #cache the inverse of the matrix 'x'
    x$setinv(inv.matrix)
    
    #return the inverse of the matrix 'x'as the function's output
    inv.matrix
}

