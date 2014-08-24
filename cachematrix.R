## create a matrix object that will cache the inverse when the solve function is called

makeCacheMatrix <- function(a = matrix()) {
  cache_matrix <- NULL
  set <- function(b){
    a <<- b
    cache_matrix <<- NULL
  }
  get <- function() a
  setmatrix <- function(solve) cache_matrix <<- solve
  getmatrix <- function() cache_matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## compute the special matrix, passing the cached value if the inverse has already been calculated

cacheSolve <- function(a = matrix(), ...) {
  cache_matrix <- a$getmatrix()
  if(!is.null(cache_matrix)){
    message("give me all your cache!")
    return(cache_matrix)
  }
  matrix <- a$get()
  cache_matrix <- solve(matrix, ...)
  a$setmatrix(cache_matrix)
  cache_matrix
}