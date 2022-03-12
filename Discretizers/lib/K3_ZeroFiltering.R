K3_ZeroFiltering <- function(data,df,min,max,threshold) {
  DIGITS=6
  auxDF<-round(df[,-c(12,13,14)],digits=DIGITS)
  setIdx1<-which(  auxDF$df.sol.1 > auxDF$df.interval.start    & 
                   auxDF$df.sol.1 < auxDF$df.interval.stop     & 
                   auxDF$df.sol.1 > min                        & 
                   auxDF$df.sol.1 < max                        & 
                   auxDF$df.a    != 0                          & 
                   auxDF$df.b    != 0                          & 
                   auxDF$df.c    != 0 )     
  setIdx2<-which(  auxDF$df.sol.2 > auxDF$df.interval.start    & 
                   auxDF$df.sol.2 < auxDF$df.interval.stop     & 
                   auxDF$df.sol.2 > min                        & 
                   auxDF$df.sol.2 < max                        & 
                   auxDF$df.a    != 0                          & 
                   auxDF$df.b    != 0                          & 
                   auxDF$df.c    != 0 )    
  
  auxDF$df.num.solutions=0
  auxDF$df.num.solutions[setIdx1]=auxDF$df.num.solutions[setIdx1]+1
  auxDF$df.num.solutions[setIdx2]=auxDF$df.num.solutions[setIdx2]+1
  
  #print(auxDF)
  
  ##
  ## Marcar las soluciones
  ##
  solutions.1<-rep(0,length.out=length(auxDF[,1]))
  solutions.2<-rep(0,length.out=length(auxDF[,1]))
  
  solutions.1[setIdx1]=solutions.1[setIdx1]+1
  solutions.2[setIdx2]=solutions.2[setIdx2]+1

  dfFinal<-data.frame()
  auxNumSolutions = unique(sort(c(setIdx1,setIdx2)))
  val11Idx=c()
  val12Idx=c()
  val21Idx=c()
  val22Idx=c()  
  if ( length(auxNumSolutions)>0) {
    diffIntervals = auxNumSolutions
    dfFiltered = auxDF[c(diffIntervals),]
    dfFiltered$df.sign1=0
    dfFiltered$df.sign2=0
    dfFiltered$df.sign3=0
    solutions.1.filtered=rep(0,length(diffIntervals))
    solutions.2.filtered=rep(0,length(diffIntervals))
    
    solutions.1.filtered = solutions.1[c(diffIntervals)]
    solutions.2.filtered = solutions.2[c(diffIntervals)]

    for(i in diffIntervals) {
      if ( dfFiltered$df.sol.1 > dfFiltered$df.sol.2 ) {
        aux<-dfFiltered[i]$df.sol.2
        dfFiltered$df.sol.2[i]<-dfFiltered$df.sol.1[i]
        dfFiltered$df.sol.1[i]<-aux
      }
    }

    ## RULE 4: Debe haber un cambio de clase
    ### > Calculo de los signos
    ns1Idx = which(dfFiltered$df.num.solutions==1)
    ns2Idx = which(dfFiltered$df.num.solutions==2)
    if (length(ns1Idx)>0) {
      
      x.sol=0
      idx1=c()
      idx2=c()
      if (length(dfFiltered$df.sol.1[which(dfFiltered$df.sol.1>0)])>0) {
        idx1=which(solutions.1.filtered==1 & solutions.2.filtered==0)
        if (length(idx1)>0) {
          x.sol = unique(sort(unique(dfFiltered$df.sol.1[idx1])))
          x.sol.idx = idx1
          x.begin = dfFiltered$df.interval.start[x.sol.idx]
          x.end   = x.sol
          mdp1    = (x.begin+x.end)/2
          a       = dfFiltered$df.a[x.sol.idx] #ns1Idx
          b       = dfFiltered$df.b[x.sol.idx] #ns1Idx
          c       = dfFiltered$df.c[x.sol.idx] #ns1Idx
          f       = a*mdp1*mdp1 + b*mdp1 + c
          sign1   = sign ( f )
          x.begin = x.end
          x.end   = dfFiltered$df.interval.stop[x.sol.idx] # ns1Idx
          mdp2    = (x.begin+x.end)/2
          f       = a*mdp2*mdp2 + b*mdp2 + c
          sign2   = sign ( f )
          dfFiltered$df.sign1[x.sol.idx] = sign1
          dfFiltered$df.sign2[x.sol.idx] = sign2
          dfFiltered$df.sign3[x.sol.idx] = sign2
          aux11Idx = which ( dfFiltered$df.sign1[x.sol.idx] != dfFiltered$df.sign2[x.sol.idx] )
          val11Idx = idx1[aux11Idx]
        }
      }
      if (length(dfFiltered$df.sol.2[which(dfFiltered$df.sol.2>0)])>0) {
        idx2=which(solutions.1.filtered==0 & solutions.2.filtered==1)
        if (length(idx2)>0) {
          x.sol = unique(sort(unique(dfFiltered$df.sol.2[idx2])))
          x.sol.idx = idx2
          x.begin = dfFiltered$df.interval.start[x.sol.idx]
          x.end   = x.sol
          mdp1    = (x.begin+x.end)/2
          a       = dfFiltered$df.a[x.sol.idx] #ns1Idx
          b       = dfFiltered$df.b[x.sol.idx] #ns1Idx
          c       = dfFiltered$df.c[x.sol.idx] #ns1Idx
          f       = a*mdp1*mdp1 + b*mdp1 + c
          sign1   = sign ( f )
          x.begin = x.end
          x.end   = dfFiltered$df.interval.stop[x.sol.idx] # ns1Idx
          mdp2    = (x.begin+x.end)/2
          f       = a*mdp2*mdp2 + b*mdp2 + c
          sign2   = sign ( f )
          dfFiltered$df.sign1[x.sol.idx] = sign1
          dfFiltered$df.sign2[x.sol.idx] = sign1
          dfFiltered$df.sign3[x.sol.idx] = sign2
          aux12Idx = which ( dfFiltered$df.sign2[x.sol.idx] != dfFiltered$df.sign3[x.sol.idx] )
          val12Idx = idx2[aux12Idx]
        }
      }
    }
    if (length(ns2Idx)>0) {
      x.begin = dfFiltered$df.interval.start[ns2Idx]
      x.end   = dfFiltered$df.sol.1[ns2Idx]
      mdp1    = (x.begin+x.end)/2
      a       = dfFiltered$df.a[ns2Idx]
      b       = dfFiltered$df.b[ns2Idx]
      c       = dfFiltered$df.c[ns2Idx]
      f       = a*mdp1*mdp1 + b*mdp1 + c
      sign1   = sign ( f )
      x.begin = dfFiltered$df.sol.1[ns2Idx]
      x.end   = dfFiltered$df.sol.2[ns2Idx]
      mdp2    = (x.begin+x.end)/2
      f       = a*mdp2*mdp2 + b*mdp2 + c
      sign2  = sign ( f )
      x.begin = dfFiltered$df.sol.2[ns2Idx]
      x.end   = dfFiltered$df.interval.stop[ns2Idx]
      mdp3    = (x.begin+x.end)/2
      f       = a*mdp3*mdp3 + b*mdp3 + c
      sign3  = sign ( f )
      dfFiltered$df.sign1[ns2Idx] = sign1
      dfFiltered$df.sign2[ns2Idx] = sign2
      dfFiltered$df.sign3[ns2Idx] = sign3
      aux21Idx = which ( dfFiltered$df.sign1[ns2Idx] != dfFiltered$df.sign2[ns2Idx] )
      aux22Idx = which ( dfFiltered$df.sign2[ns2Idx] != dfFiltered$df.sign3[ns2Idx] )
      val21Idx=ns2Idx[aux21Idx]
      val22Idx=ns2Idx[aux22Idx]
    }
    dfFinal<-dfFiltered      
  }
  #print(dfFinal)
  valIdx=unique(sort(c(val11Idx,val12Idx,val21Idx,val22Idx)))
  cutPoints=c(dfFinal$df.sol.1[val11Idx])
  cutPoints=c(cutPoints,dfFinal$df.sol.2[val12Idx])
  cutPoints=c(cutPoints,dfFinal$df.sol.1[val21Idx])
  cutPoints=unique(sort(c(cutPoints,dfFinal$df.sol.2[val22Idx])))
  #print(cutPoints)
  if (length(cutPoints)>0) {
    candidateZeros=cutPoints
    ## RULE 5: Debe haber al menos n instancias entre los ceros, es decir, en cada intervalo.
    discData = splitter(data,candidateZeros)
    cpIdx<-rep(0,length.out=length(candidateZeros))
    for(cpi in 1:length(candidateZeros))
      if (length(which(discData==cpi)) < threshold ) cpIdx[cpi]=-1
      else cpIdx[cpi]=threshold
    zeros=candidateZeros[which(cpIdx!=-1)]
  } else {
    zeros=c()
  }
  return(zeros)
}