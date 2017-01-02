## mean vector example in assignement
#

#makeVector creates a special "vector", which is really a list of functions
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    x
  }
  
  setmean <- function(mean) {
    m <<- mean
  } 
  getmean <- function() {
    m
  }
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

#The following function calculates the mean of the special "vector" created with 
#the above function
cachemean <- function(x, ...) {
  #query x vectors' cache
  m <- x$getmean()
  
  #check if there was a cache of mean and get it 
  if(!is.null(m)) {
    message("getting cached data....")
    return(m)
  }
  
  #get data if there is no cache
  data <- x$get()
  #calculate mean
  m <- mean(data, ...)
  #set mean in the cache 
  x$setmean(m)
  #return mean 
  m
}
