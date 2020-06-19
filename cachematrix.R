## some instructions for testing your makeCacheMatrix and cacheSolve functions

This is from this post: "Simple test matrices for the lexical scoping programming assignment"

https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg


R session:

> # Matrices for testing the R functions 
> # makeCacheMatrix and cacheSolve
> # in the Coursera R Programming Course
> #
> # First, 
> # If you haven't read Leonard Greski's invaluable
> # [TIPS] Demystifying makeVector()  Post
> # be sure to do so.
> #
> # A simple matrix m1 with a simple matrix inverse n1
> # Define
 m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
 m1
      [,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> 
> # You can use m1 to test your 
> # makeCacheMatrix and cacheSolve functions.
> # Since the grading is done on the correctness of your 
> # makeCacheMatrix and cacheSolve functions and your 
> # comments on how they work, using this (or some other)   
> # test matrix to check your code 
> # before submitting it 
> # is OK relative to the Coursera Honor Code.
> # (Checking code with test cases is always a good idea.)
> #
> # m1 was constructed (using very simple linear algebra, so
> # no references are given, almost surely the examples
> # in this post have been given many times before) 
> # to have a simple marrix inverse, call it n1.
> # This means  m1 %*% n1 (%*% is matrix multiply in R) 
> # is the 2 row by 2 column Identity matrix I2
> I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
> I2
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> 
> # And so (by linear algebra) n1 %*% m1 is also equal I2.
> # (If n1 is the inverse of m1 then m1 is the inverse of n1.)
> # With m1 defined as above, n1 ( the inverse of m1) is
 n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
 n1
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
> # Checks:
> m1 %*% n1
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> 
> n1 %*% m1
     [,1] [,2]
[1,]    1    0
[2,]    0    1
> 
> solve(m1)
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
> solve(n1)
      [,1]  [,2]
[1,]  0.50 -1.00
[2,] -0.25  0.75
> 
> # So if you have programmed your functions 
> # correctly (in the file cachematrix.R),
> # (that, and your comments-explanation of how they work
> # are what you are graded on)
> # and sourced cachematrix.R so they are 
> # available in your R session workspace, then doing 
> #
> myMatrix_object <- makeCacheMatrix(m1)
> 
> # and then
> # cacheSolve(myMatrix_object)
> 
> # should return exactly the matrix n1
> cacheSolve(myMatrix_object)
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
> # calling cacheSolve again should retrieve (not recalculate)
> # n1
> cacheSolve(myMatrix_object)
getting cached data
     [,1] [,2]
[1,]    6    8
[2,]    2    4
> 
> # you can use the set function to "put in" a new matrix.
> # For example n2
> n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
> myMatrix_object$set(n2)
> # and obtain its matrix inverse by
> cacheSolve(myMatrix_object)
     [,1] [,2]
[1,]    3    7
[2,]    1    5
> 
> cacheSolve(myMatrix_object)
getting cached data
     [,1] [,2]
[1,]    3    7
[2,]    1    5
> 
#### Creation of the function makeCacheMatrix and CacheSolve################

makeCacheMatrix <-   function(x = numeric()){
   m <- NULL
   set <- function(y){
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get=get, 
        setinverse = setinverse,
        getinverse = getinverse)
} 
## In order to create the function we first had 
#to create the function makecacheMatrix to achieve the following results:
# set the value of the vector, get the value of the vector, set the value of the inverse and get the value of the inverse
#in this example the m represents the future value of the matrix and and with the data
#setinverse and getinverse we manage to calculate the inverse of the named matrix.


cacheSolve <- function(x, ...){
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cache data")
      return(m)
   }
   data <- x$get()
   m <- solve(data,...)
   x$setinverse(m)
   m
}
### Afterwords with create the function cacheSolve to check if the inverse has
# been created previously or it has to calculate it. We use solve() to obtain the 
#represented inverse of the named matrix. 
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
# Using the above example we manage to 


