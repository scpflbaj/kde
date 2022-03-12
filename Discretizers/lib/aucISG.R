##########################################################################################
### Name  : aucISG
### Input : Vector x, vector c de clases (-1,+1), Num valores PLUS, Num valores MINUS
### Output: Valor AUC propuesto por Aritz.
##########################################################################################
aucISG<-function(x,c,NI_PLUS,NI_MINUS)
{
  result<-0
  numSamples<-length(x)
  myC<-as.numeric(levels(c)[c])
  for(i in 1:numSamples){
    if (myC[i]==1 && i > 1) {
      for(j in (i-1):1) {
        if (myC[j]==-1) result=result+1
      }
    }
  }
  NI_PLUS_new=NI_PLUS
  NI_MINUS_new=NI_MINUS
  if (NI_PLUS==0) NI_PLUS_new=1
  if (NI_MINUS==0) NI_MINUS_new=1
  result=result/(NI_PLUS_new*NI_MINUS_new)  
  return(result)
}