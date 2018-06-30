##

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Ok, so I have created a function called makeCacheMatrix, this function gets a matrix as its input
# The function is used to cache its inverse. If I only wanted to find the inverse of a matrix I could 
# use the solve function. However, this is to save time and computer power
# makeCacheMatrix is a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
invMatrix <- NULL # Defining invMatrix as NULL 

#This will set the value of the Matrix
setMatrix <- function(y) {
# the <<- operator is used to assign a value to an object that is not the same as the current enviroment
  
x <<- y
invMatrix <<- NULL  #invMatrix still defined as NULL
}
  
getMatrix <- function() x                              #function for getting the value of the matrix
setInverse <- function(inverse) invMatrix <<- inverse  #function to set the value of the invertible matrix
getInverse <- function() invMatrix                     #function to get the value of the invertible matrix

#Creating a list, this is so that we can use the $ operator later
list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

# Ok so this function caches the inverse of the matrix from the upper function. However, it is gonna 
#use and cache and print the message "Getting Cached Invertible Matrix" if the cache has already benn 
#calculated

cacheSolve <- function(x, ...) {
    
#get the value of the invertible matrix from the makeCacheMatrix function
#this is why the list was important in the upper function
invMatrix <- x$getInverse()
if(!is.null(invMatrix)) {                       #So if inverse matrix is not NULL (output TRUE)
message("Getting Cached Invertible Matrix")   #The function is gonna type Getting Cached Invertible Matrix 
return(invMatrix)                             #and return the already calculated invertible matrix
}
    
#if !is.null is FALSE and the value of the invertible matrix is NULL then  
MatrixData <- x$getMatrix()                     #This will get the matrix from the upper function
invMatrix <- solve(MatrixData, ...)             #This gives you the inverse of the matrix
x$setInverse(invMatrix)                         #set the value of the invertible matrix 
return(invMatrix)                               #return the invertible matrix
## Return a matrix that is the inverse of 'x'
}
  
Test2x2 <- matrix(1:4,2,2)
Test2x2
  
CacheMatrix <- makeCacheMatrix(Test2x2)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()
  
cacheSolve(CacheMatrix)
  
  
  
Test3x3 <- matrix (c(-1,2,3,-2,1,4,2,1,5),3,3)
Test3x3
  
CacheMatrix1 <- makeCacheMatrix(Test3x3)
CacheMatrix1$getMatrix()
CacheMatrix1$getInverse()
  
# Test if cached matrix and solved matrix is the same

cacheSolve(CacheMatrix1)
  
solve(Test3x3)
