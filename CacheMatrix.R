makeMatrix <- function(x=matrix())
  {
    inverse<-NULL
    set<-function(y)
      {
        x<<-y
        inverse<<-NULL
      }
    get<-function() x
    setinv<-function(inv)
      {  
        inverse<<-inv
      }
    getinv<-function() inv

    list( set=set, get=get, setinv=setinv, getinv=getinv)
  }
cacheinverse<- function(x,...)
  {
    inverse<-x$getinv()

    if(!is.null(inverse))
      {
        message("getting cached data")
        return(inverse)
      }

    matrix<-x$get()
    inverse<-solve(m,...)
    x$setinv(inverse)
    return(inverse)
  }
  
  
