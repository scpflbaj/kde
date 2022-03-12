##########################################################################################
### Name  : E
### Input : Data vector.
###			    Cut point
### Output: Entropy.
##########################################################################################
E<-function(S,T) {
#  print("E::BEGIN")
  N<-length(S[,1])
  
  S1 <- S[1:T,]
  S2 <- S[(T+1):N,]

  N1 <- length(S1[,1])
  N2 <- length(S2[,1])
  
  N<-length(S[,1])
  
#  print(paste(" N1: ",toString(N1)," N2: ",toString(N2)," N: ",toString(N)))
  
  Result<-(N1/N)*Entropy(S1)+(N2/N)*Entropy(S2)
#  print(paste(" N1/N: ",toString(N1/N)," N2/N: ",toString(N2/N)," N: ",toString(N),"Entropy(S1):",toString(Entropy(S1)),"Entropy(S2):",toString(Entropy(S2))))
#  print("E::END")
  return(Result)
}

