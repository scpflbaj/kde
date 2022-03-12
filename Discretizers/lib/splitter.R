##########################################################################################
### Name  : splitter
### Input : Vector data del atributo de valores continuos.
###			Vector policy con la politica de discretizacion
### Output: Vector discretizado.
##########################################################################################
splitter<-function(auxData,policy)
{
  numCP=length(policy)
  DISCRETIZED<-rep(0,length(auxData))
  nElements<-length(auxData)
  for (j in 1:nElements) {
    x=auxData[j]
    if (numCP>1) {
      for (k in (1:(numCP-1))) {
        aLim=policy[k]
        bLim=policy[k+1]
        diff=aLim-x
        if ( diff > 0.0 && k==1 ) {
          DISCRETIZED[j]=k
        }
        if ( x >= aLim && x < bLim ) {
          DISCRETIZED[j]=k+1
        }
      }
      cLim=policy[numCP]
      diff=x-cLim
      if ( diff>=0.0 ) {
        DISCRETIZED[j]=numCP+1 
      }      
    } else {
      if ( x< policy[1]) DISCRETIZED[j]=1
      else               DISCRETIZED[j]=2
    }
  }
  return (DISCRETIZED)
}