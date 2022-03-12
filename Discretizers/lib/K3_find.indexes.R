K3_find.indexes <- function(data,lim.inf,lim.sup,h) {
  i<-1
  i.accepted <-1
  detected <- FALSE
  stop<-FALSE
  indexes<-c()
  max<-length(data)
  while (i<=max & stop == FALSE) {
    a <- data[i]-h
    b <- data[i]+h
#    print(paste("Iteration:",toString(i),"Lim.Inf",toString(lim.inf)," Lim.Sup:",toString(lim.sup)," a:",toString(a),"x:",toString(data[i]),"b:",toString(b)))
    if ( a == lim.inf ) {
#      print("C.00")
      indexes[i.accepted]=i
      detected<-TRUE
      i.accepted<-i.accepted+1
    }
    if ( b > lim.inf & b <  lim.sup & detected == FALSE) {
#      print("C.01")
      indexes[i.accepted]<-i
      detected<-TRUE
      i.accepted<-i.accepted+1
    }
    if ( a >  lim.inf & a < lim.sup & detected == FALSE) {
#      print("C.02")
      indexes[i.accepted]<-i
      detected<-TRUE
      i.accepted<-i.accepted+1
    }
    if ( a == lim.inf & b == lim.sup & detected == FALSE) {
#      print("C.03")
      indexes[i.accepted]<-i
      detected<-TRUE
      i.accepted<-i.accepted+1
    }
    if ( a < lim.inf & b > lim.sup & detected == FALSE) {
#      print("C.04")
      indexes[i.accepted]<-i
      detected<-TRUE
      i.accepted<-i.accepted+1
    }
    if ( b == lim.sup & detected == FALSE) {
#      print("C.05")
      indexes[i.accepted]<-i
      detected<-TRUE
      i.accepted<-i.accepted+1
    }
    if ( a > lim.sup ) stop = TRUE
    detected<-FALSE
    i <- i+1
    
  }
  return(indexes)
}