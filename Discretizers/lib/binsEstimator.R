##########################################################################################
### Name  : binsEstimator
### Input : Method code for estimating the number of intervals.
###			Number of instances
### Output: Number of recommended intervals.
##########################################################################################
binsEstimator<-function(method=1,numInstances) 
{ 
  result=0
  if (method==0) {
    result=round(sqrt(numInstances))
  }
  if (method==1) {
    result=round(round(1+log2(numInstances)))
  }
  return(result)
}