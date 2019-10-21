## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix takes an matrix as an argument and cre

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()){
	inverse <- NULL
  	setmatrix <- function(a){       # sets new matrix into the object, and erases old cached inverse
    		x <<- a
    		inverse <<- NULL
  	}
  	matrix <- function(){           # returns the matrix stored in the mkmatrix object
    		x
  	}
  	setinv <- function(i){          # sets externally computed inverse into the mkmatrix object
    		inverse <<- i
  	}
  	inv <- function(){              # returns cached inverse or default NULL
    		inverse
  	}
  	list(setmatrix = setmatrix, matrix = matrix, setinv = setinv, inv = inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$inv()                  	# stores cached inverse or null into inv
        if(!is.null(inv)){
                message("getting cached inverse")
        } else {
                inv <- solve(m$matrix())	# m$matrix() returns the matrix itself from the object created by mkmatrix
                x$setinv(inv)           	# sets an inverse within the object created by mkmatrix
        }
        inv                             	# returns inv, either as stored cache or newly computed via the else statement  
}



