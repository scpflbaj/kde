##########################################################################################
### Name  : ProbEntropy
### Input : S - Data vector.
###			    T - Cut point index
### Output: Entropy.
##########################################################################################
ProbEntropy<-function(S) {
#  print("ProbEntropy::BEGIN")
  zaux<<-S
  cplist<-list()
  Gain<-0
  N<-length(S[,1])
#  print(N)
  A<-Entropy(S)
#print(S)
  cpp<-0
  for(T in 1:(N-1)) {
    B<-E(S,T)
#    print(paste("T:",toString(T)," Entropy(S):",toString(A)," E(S,T):",toString(B), "Entropy(S)-E(S):",toString(A-B)))
    if (A-B>Gain) {
      Gain <- A-B
      cpp  <- T
#      print(paste("T:",toString(T)," cpp:",toString(cpp)))
#      print(paste("E(T,S)=",toString(B)))
    }
  }
  itWorths=FALSE
  if (cpp>0) {
    
    S1<-S[  1:cpp,]
    S2<-S[(cpp+1):N,]
    cutPoint = as.numeric(((S[cpp,1]+S[cpp+1,1])/2))
    
    ##C<-2 ## Number of classes 
    ##c1<-C
    ##c2<-C
    C <-sum(S[,2])/N + sum(S[,3])/N
    c1<-sum(S[1:cpp    ,2])/length(S[1:cpp    ,2])+sum(S[1:cpp    ,3])/length(S[1:cpp    ,3])
    c2<-sum(S[(cpp+1):N,2])/length(S[(cpp+1):N,2])+sum(S[(cpp+1):N,3])/length(S[(cpp+1):N,3])
    
    BigDelta<-log2(3**C-2)-(C*Entropy(S)-c1*Entropy(S1)-c2*Entropy(S2))
    Delta<-log2(N-1)
    Threshold<-Delta/N+BigDelta/N
#    print(paste("c:",toString(C),"Entropy(S)",toString(Entropy(S))," c1:",toString(c1),"Entropy(S1)",toString(Entropy(S1))," c2:",toString(c2),"Entropy(S2)",toString(Entropy(S2))) )
    #print(paste("Cut-Point:",toString(cutPoint),"Threshold:",toString(Threshold)," Gain:",toString(Gain)," Delta:",toString(Delta/N),"BigDelta",toString(BigDelta/N)))              
    if (Gain > Threshold) {
      
#        print(paste("c:",toString(C),"Entropy(S)",toString(Entropy(S))," c1:",toString(c1),"Entropy(S1)",toString(Entropy(S1))," c2:",toString(c2),"Entropy(S2)",toString(Entropy(S2))) )
        
        cpList1 = ProbEntropy ( S1 )
        cpList2 = ProbEntropy ( S2 )
        

        
#        print(paste("Cut-Point",toString(cutPoint),"Entropy(S):",toString(A),"BigDelta",toString(BigDelta/N)," Delta:",toString(Delta)," Threshold:",toString(Threshold)," Gain:", toString(Gain)))              
        
        if (length(cpList1)!=0) {
          cplist=c(cplist,cpList1$cutp)  
        }
        cplist=c(cplist,cutPoint)
        if (length(cpList2)!=0) {
          cplist=c(cplist,cpList2$cutp)  
        }
        itWorths=TRUE
        cplist=list(unlist(cplist))
    }
  }
  if (itWorths==FALSE) {
    result=list(cutp=list())
  } else {
    print(cplist)
    dataDiscretized=splitter(S[,1],unlist(cplist))
    result=list(cutp=cplist,Disc.data=dataDiscretized)
  }
  zaux<<-result
#  print("ProbEntropy::END")
  return(result)
}