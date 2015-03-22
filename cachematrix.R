## Put comments here that give an overall description of what your
## functions do
#Pablo Roberto Figueroa
## Write a short comment describing this function
#This function have 4 function inside, the first and the second (set and get), the purspuse of these is to get or set the
#Matrix value like an object. 
#The setinver is taking the parameter, set this value to the variable m, that is the inver matrix.
#The getinver, in the beginning is null because nothing is set, but after that is called the setinver the value m has a value and now 
#this function get back the value of m.
makeCacheMatrix <- function(x =matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(inver) m <<- inver
  getinver <- function() m
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## Write a short comment describing this function
##This function at the begin, recibe the parameter X, that contains the function before described, then call the function getinver,
#but like it's describe above, at the beggining is null because nothing is set.
#Then the condition looks if the variable is null, the firts time is null, so it's call the function get, to get the value of the maxtrix
#and then solve the inverse. After that the function to set the inverse of the object X it's call.
#If not is the first time that the funciont cacheSolve is used, in the condition the value of m is not null so this value is returned.
cacheSolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinver(m)
  m
}
