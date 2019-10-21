## Function makeCacheMatrix takes matrix x as an argument and creates an object that contains several functions
## for further manipulation and storing cache of the matrix inverse

## Function cacheSolve takes an object x, which is produced by makeCacheMatrix, and returns the inverse of the matrix
## stored in this object, either by reading the value stored in cache, or computing it 


## Takes a matrix and returns an enriched object for caching the inverse

makeCacheMatrix <- function(x=matrix()){
        inverse <- NULL
        setmatrix <- function(a){       # sets new matrix into the object, and erases old cached inverse
                x <<- a
                inverse <<- NULL
        }
        matrix <- function(){           # returns the matrix stored in the makeCacheMatrix object
                x
        }
        setinv <- function(i){          # sets externally computed inverse into the makeCacheMatrix object
                inverse <<- i
        }
        inv <- function(){              # returns cached inverse or default NULL
                inverse
        }
        list(setmatrix = setmatrix, matrix = matrix, setinv = setinv, inv = inv)
}


## Takes an makeCacheMatrix object and returns the inverse of matrix stored within it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$inv()                  	# stores cached inverse or null into inv
        if(!is.null(inv)){
                message("getting cached inverse")
        } else {
                inv <- solve(x$matrix())	# x$matrix() returns the matrix itself from the object created by makeCacheMatrix
                x$setinv(inv)           	# sets an inverse within the object x created by makeCacheMatrix
        }
        inv                             	# returns inv, either as stored cache or newly computed via the else statement  
}
