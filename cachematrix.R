#Hi! I'm new to programming and I wrote the comments for someone with my (lack of) background. Maybe it's too wordy for you.
#These functions do exactly what the assignment wants them to do, which is first accepting a matrix as an input, then outputs its inverse while storing it for later access.

#This creates an object with a list of methods, one of which contains yourmatrix. E.g., haha <- makeCacheMatrix(yourmatrix) creates the list haha of which yourmatrix can be accessed by calling haha$get()
#This also stores the inverse of yourmatrix in a variable called MatInv, if it is already calculated by cacheSolve. If not, MatInv is NULL and you have to call cacheSolve(haha) to solve it.
makeCacheMatrix <- function(x = matrix()){
    #This initializes a variable MatInv (matrix inverse) to NULL, which will be set to the inverse of yourmatrix when calling the cacheSolve function.
    MatInv = NULL
    #This allows you to change the input matrix by calling haha$set(newmatrix), and reset MatInv to NULL for newmatrix.
    set <- function(y){
        x <<- y
        MatInv <<- NULL
    }
    #This returns yourmatrix by calling haha$get()
    get <- function(){x}
    #This is called by cacheSolve to set MatInv to the inverse of yourmatrix.
    setinv <- function(matrixinverse){MatInv <<- matrixinverse}
    #This returns the inverse of your matrix by calling haha$getinv()
    getinv <- function(){MatInv}
    #This returns the above methods as items in a list, which can then be called by haha$methodname, as described above.
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

#This function solves the inverse of yourmatrix when calling cacheSolve(haha), and caches it.
cacheSolve <- function(x, ...){
    #This looks into makeCacheMatrix to receive the value of MatInv in the list object haha.
    MatInv <- x$getinv()
    #If the MatInv received is NULL, this step is skipped; otherwise, it returns the current value of MatInv in haha with a message indicating it is cached data.
    if(!is.null(MatInv)){
        message("getting cached data...")
        return(MatInv)
    }
    #If the MatInv received is NULL, these following steps will calculate the inverse of yourmatrix and store it in MatInv in haha.
    #This puts yourmatrix in a variable yourMat.
    yourMat <-x$get()
    #This uses the function solve to calculate the inverse of yourMat, which is the same as yourmatrix, and put it in MatInv.
    MatInv <- solve(yourMat, ...)
    #This calls the setinv method in the makeCacheMatrix function, and sets the value of MatInv in haha to the result calculated by the line above.
    x$setinv(MatInv)
    #This returns the value of MatInv, which at this step is the inverse of yourmatrix.
    MatInv
}