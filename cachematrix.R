#  cachematrix.R
#  Coursera_Data_Science
#
#  Created by CEJ on 8/20/15.
#  Copyright (c) 2015 CEJ. All rights reserved.



## Put comments here that give an overall description of what your
## functions do

## This function that wraps a matrix into a list.  This list contains containing a function to
##       1. set the value of the matrix
##       2. get the value of the matrix
##       3. set the value of the inverse matrix
##       4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve is a extension of the normal solve function which "keeps a 
## cache of the mapping from arguments to results and, when calls with the
## same arguments are repeated often, has higher performance at the expense
## of higher memory use." [quote from: http://clojuredocs.org/search?q=memoize.]

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}

