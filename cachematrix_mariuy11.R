## Assignment2: Caching the Inverse of a Matrix

## First, we create a matrix that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) s<<- solve
        getmatrix<-function() s
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x=matrix(), ...) {
        s<-x$getmatrix()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        ## Return a matrix that is the inverse of 'x'
        matrix<-x$get()
        s<-solve(matrix, ...)
        x$setmatrix(s)
        s
}