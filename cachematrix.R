# makeCacheMatrix is a function that returns a list of functions
# It stores a martix and the cached inverse of the matrix
# and can be used to return both
makeCacheMatrix <- function(x = numeric()) 
	{
        m <- NULL
        # m holds the cached value
        # but since initially nothing is cached so m is set it to NULL

        set <- function(a)   # stores the matrix
	        	{
        	        x <<- a # storing matix in x
                        m <<- NULL #as we have a new martrix at hand now....so set cache to NULL
        		}

        get <- function()   # returns the stored matrix
			{
                	x
        		}
    	  setInverse <- function(z) #storing the inverse 
			{
                	m<<- z
       		        }

        getInverse <- function()  #returning the inverse
			{
                	m
        		}
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)		
	#a list whose elements are functions
	#since this is the last arguement of the funtion makeCacheMatrix, hence it is returned 
	}

cacheSolve <- function(y, ...) # This finds the inverse of a special matrix created with makeCacheMatrux
	{
        inverse <- y$getInverse()       # get the cached value
	  if(!is.null(inverse))         # if the cached value exists return it(i.e if m is not NULL)
	                {
	                message("getting cached data")
                 	return(inverse)
        		}
        		                # else get the matrix, find the inverse and store in m, return the inverse
        data <- y$getMatrix()
        inverse <- solve(data)
        y$setInverse(inverse)
        
        inverse
	}
