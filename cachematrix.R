makeCacheMatrix <- function(x=matrix()) {
    
    s <- NULL
    
    set <- function(y) {   # sets the matrix
        
        x <<- y
        s <<- NULL
        
    }
     
    get<- function() x   # function to see matrix values
    setinverse <- function(inverse) s <<- inverse  # calculates the inverse
    getinverse <- function() s  # returns the inverse when called
    list(set = set, get = get, # lists the contents of the function
         setinverse = setinverse, getinverse = getinverse)
    
}

cacheSolve <- function(x, ...) {
    
    s <- x$getinverse()  # gets the inverse of the matrix
    
    if(!is.null(s)) {    # if it's null, gets cached data
        
        message("getting cached data")
        return(s)
        
    }
    
    data <- x$get()  # returns and stores matrix
    s<- solve(data, ...)  # calculates inverse
    x$setinverse(s) # sets the inverse
    s # displays the inverse
    
}