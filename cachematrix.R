## Function for Inverse Matrix

## First function building the Matrix to be inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(matriz){
        x <<- matriz
        inverse <<- NULL
        
    }
    get <- function(){
        ## return x
        x
    }
    
    ## Melhod the get set Inverse 
    setInverse <- function(inv) {
        inverse <<- inv 
    }
    
    getInverse <- function(){
        inverse
    }
    
    list(set= set, get= get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Function calculate inverse the special matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
        
        if(!is.null(inv)){
            message("Getting cached data")
            return(inv)
        }
        
    data <- x$get()
     
    inv <- solve(data)
      
    x$setInverse(inv)
    
    inv
}


#Example
#m <- makeCacheMatrix(rbind(c(2,4), c(0,5)))
#      [,1] [,2]
#[1,]    2    4
#[2,]    0    5
#
# cacheSolve(m)
#
#    [,1] [,2]
#[1,]  0.5 -0.4
#[2,]  0.0  0.2
