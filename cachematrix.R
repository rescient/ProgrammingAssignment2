#These functions calculate the inverse of Matrix and create and object that can be cache to save computation time#

#This first function called "makeCacheMatrix" creates a matrix object that can cache its inverse


makeCacheMatrix<- function (x=matrix()) {
	Minv<-NULL
	set<-function(y){
		x<<-y
		Minv<<-NULL
	}
	get<- function() x
	setinverse <- function(solve) Minv <<- solve
	getinverse <- function () Minv
	list(set = set, get = get, setinverse =setinverse, getinverse =getinverse)
}


#The second function"cacheSolve" computes the inverse of the matrix object returned by the previous function. If the inverse has been calculated before, then "cacheSolve" should retrieve the inverse from the cache instead of calculating it again#

cacheSolve<- function (x, ...){
	Minv <- x$getinverse()
	if(!is.null(Minv)) {
		message("getting cached data")
		return(Minv)
	}
	data <-x$get()
	Minv <- solve(data,...)
	x$setinverse(Minv)
	Minv
}

#Testing the functions as suggested in the discussion forum Thread:"Simple test matrices for the lexical scoping programming assignment " #

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1
m1 %*% n1
n1 %*% m1
solve(m1)
solve(n1)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object) #This second call retrives the the inverse of the matrix "m1" without recalculating but from the cache#