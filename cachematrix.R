## This function will calculate the inverse of a Matrix
## but if the inverse was already calculated will take
## advantage of R lexical scoping to retrieve the inverse
## from a cache storage 

## makeCacheMatrix resembles RDPeng's makeVector function
## that build a list to pass thru cacheSolve in order to
## economize computation avoiding calculate the inverse of
## a Matrix thats already in the list $getinv, if there are
## no matrix in the list the function cacheSolve will calculate
## the inverse of he matrix that the user pass to the function.
## Also the cacheSolve has to verify if the $getinv inverse matrix
## is from the same matrix that the user pass

makeCacheMatrix <- function(x = matrix()) {

	invx<-NULL
	set<-function (y) {
		x<<-y       ## This is an assigment in an enviroment
		            ## outside the parent(current function)
                            ## enviroment
		xinv<<-NULL ## Exactly like this one
	}
	get<-function() x
	setinv<-function(solve) xinv<<-solve
	getinv<-function() xinv
	list(set<-set, get<-get, setinv<-setinv, getinv<-getinv) 
}


## This function use makeCacheMatrix in order to verify if the matrix
## that the user pass was passed before and then the inverse is on
## the $getinv list. In that case do no calculations and return the
## storaged matrix, else using the R base solve() calculate the inverse
## of the user matrix and storage it in the cache with $setinv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## in this case 'x' comes from makeCacheMatrix
	xinv<-x$getinv()
	## First verify if is an inverse already calculated
	## if is not calculated xinv is NULL
	if (!is.null(xinv)) {
		## If is already on the cache get it
		message("Getting cached data")
		return(m)
	}
	## Calculate the inverse
	m<-x$get()
	xinv<-solve(m)
	## Store in cache
	x$setinv(xinv)
	return(xinv)
}
