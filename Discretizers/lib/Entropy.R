##########################################################################################
### Name  : Entropy
### Input : Data vector.
### Output: Entropy.
##########################################################################################
Entropy<-function(S) {
#  print("Entropy::BEGIN")
  N_PLUS  <- sum(S[,2])
  N_MINUS <- sum(S[,3])
  N       <- length(S[,1])
  
  N_PLUS_N  <- N_PLUS/N
  N_MINUS_N <- N_MINUS/N

#  print(N_PLUS_N)
#  print(N_MINUS_N)  
  
  Result<--1
  if ((N_PLUS_N==1 && N_MINUS_N==0)||
      (N_PLUS_N==0 && N_MINUS_N==1)  ) {
    Result<-1.0
  } else {
    PLUS  <- - N_PLUS_N  * log2( N_PLUS_N  )
    MINUS <- - N_MINUS_N * log2( N_MINUS_N )    
    Result<-PLUS+MINUS
#    print(paste("N",toString(N),"N_PLUS",toString(N_PLUS),"N_PLUS_N:", toString(N_PLUS_N),"N_MINUS",toString(N_MINUS) ,"N_MINUS_N: ",toString(N_MINUS_N),"PLUS:",toString(PLUS), " MINUS:",toString(MINUS)))
  }
  
#  print("Entropy::END")
  return(Result)
}