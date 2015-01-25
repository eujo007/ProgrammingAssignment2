## The makeCacheMatrix method takes a matrix as an arguement and returns a list that has
## methods to set and get a matrix and set and get the Inverse of the matrix
## The cacheSolve method takes the special matrix (the list returned from makeCache Matrix)
## and tests to see if the inverse is available. If not it calculates the inverse and stores it
## int the special matrix by calling the setInverse function

## This function takes and stores a matrix. It returns a list that has functions that can
## modify the matrix that was provided as an arguement

makeCacheMatrix <- function(x = matrix())
{
    inverse <- NULL
    
    ##  Assign x the value of the incoming marix that will be used to calculate the inverse 
    set<-function(y)
    {
        ## Using the << assigns the value to the formal argument x 
        x<<- y
        
        ## The inverse of any prior matrix is no longer valid
        inverse<<- NULL
    }
    
    ## Get the matrix that that we want to calculate the inverse on
    get<- function()
    {
        x
    }
    
    ## A new inverse was calculated so set CacheMatrix's inverse
    ## Using the << assigns the value to the inverse variable defined above
    setInverse <- function(i)
    {
        inverse <<- i
    }
    
    ## Get the new inverse
    getInverse <- function()
    {
        inverse
    }
    
    ## This function returns this list
    ## When called using the $ operator and the name of the element you get the functions defined above
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   
}


## cacheSolve takes the special "matrix" above and calls methods on the matrix
## If the inverse for the matrix is null it calculates the inverse and caches it by calling the setInverse method
## Finally it returns the inverse

cacheSolve <- function(x, ...)
{
    ## When called with makeCacheMatrix as the argument the following invokes the getInverse method
    ## Assuming this is the first call it will return null
    inverse <- x$getInverse()
    
    if(!is.null(inverse))
    {
        print("Using the cached inverse matrix")
        return(inverse)
    }
    ## Return a matrix that is the inverse of 'x'
    ## Since inverse was null we need to get x and 
    temp<-x$get()
    ## then calculate the inverse
    inverse<-solve(temp)
    ## Now set the inverse so that the next time getInverse is called it is not null
    x$setInverse(inverse)
    
    ## Return the inverse
    return (inverse)
}
