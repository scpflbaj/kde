##########################################################################################
### Name  : ef
### Input : Continuous input data vector.
###			Number of intervals
### Output: List of cut-points.
##########################################################################################
ef <- function(data, k) 
{ 
  cp<-unique(quantile(data, seq(1/k, 1-1/k, 1/k))) 
  dataDiscretized=splitter(data,cp)
  result=list(cutp=cp,Disc.data=dataDiscretized)
  return(result)
}