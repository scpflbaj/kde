##########################################################################################
### Name  : ew
### Input : Continuous input data vector.
###			Number of intervals
### Output: List of cut-points.
##########################################################################################
ew<-function(data, k) 
{ 
  w<-diff(r<-range(data))/k 
  cp=seq(r[1]+w,r[2]-w,w)
  dataDiscretized=splitter(data,cp)
  result=list(cutp=cp,Disc.data=dataDiscretized)	
  return(result)
}
