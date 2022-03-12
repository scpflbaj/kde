############################################################################
##
##  K2_density 
##    INPUT : Datos ordenados de las clases + y -
##    OUTPUT: Lista de ceros, 
##            Lista de intervalos con los valores de los coeficientes
############################################################################
source("lib/K3_Zeros.R")
source("lib/K3_ZeroFiltering.R")
source("lib/K3_find.indexes.R")

K3_density <- function(X_PLUS,X_MINUS,NI_PLUS,NI_MINUS,h_plus,h_minus,min,max,PROB_VECTOR_PLUS,PROB_VECTOR_MINUS) {
  
  #print("K2_density::BEGIN")
  
  X_PLUS_FILTERED  = X_PLUS [which(PROB_VECTOR_PLUS !=0)]
  X_MINUS_FILTERED = X_MINUS[which(PROB_VECTOR_MINUS!=0)]
  
  PROB_VECTOR_PLUS_FILTERED  = PROB_VECTOR_PLUS  [which(PROB_VECTOR_PLUS !=0)]
  PROB_VECTOR_MINUS_FILTERED = PROB_VECTOR_MINUS [which(PROB_VECTOR_MINUS!=0)]
  
  interval_PLUS_LEFT   <- X_PLUS_FILTERED -h_plus
  interval_PLUS_RIGHT  <- X_PLUS_FILTERED +h_plus
  interval_MINUS_LEFT  <- X_MINUS_FILTERED-h_minus
  interval_MINUS_RIGHT <- X_MINUS_FILTERED+h_minus  
  # print(interval_PLUS_LEFT   )
  # print(interval_PLUS_RIGHT  )
  # print(interval_MINUS_LEFT  )
  # print(interval_MINUS_RIGHT )
  intervals <- c(interval_PLUS_LEFT,interval_MINUS_LEFT,interval_PLUS_RIGHT,interval_MINUS_RIGHT)
  intervals <- unique(intervals)
  intervals <- sort(intervals)
  # print(intervals)
  num.intervals <- length(intervals)
  df.interval.start <- array(dim=c(num.intervals-1,1))
  df.interval.stop <- array(dim=c(num.intervals-1,1))  
  for(i in 1:(num.intervals-1)) {
    df.interval.start [i] <- intervals[i]
    df.interval.stop  [i] <- intervals[i+1]
  }
  df.a.plus  <- rep(0,length.out=num.intervals-1)
  df.b.plus  <- rep(0,length.out=num.intervals-1)
  df.c.plus  <- rep(0,length.out=num.intervals-1)
  df.a.minus <- rep(0,length.out=num.intervals-1)
  df.b.minus <- rep(0,length.out=num.intervals-1)
  df.c.minus <- rep(0,length.out=num.intervals-1)
  df.a <- rep(NA,length.out=num.intervals-1)
  df.b <- rep(NA,length.out=num.intervals-1)
  df.c <- rep(NA,length.out=num.intervals-1)
  df.sign1 <- rep(NA,length.out=num.intervals-1)
  df.sign2 <- rep(NA,length.out=num.intervals-1)
  df.sign3 <- rep(NA,length.out=num.intervals-1)  
  df.num.solutions <- rep(NA,length.out=num.intervals-1)
  df.sol.1 <- rep(NA,length.out=num.intervals-1)
  df.sol.2 <- rep(NA,length.out=num.intervals-1)
  
  df=data.frame(df.interval.start,df.interval.stop,
                df.a.plus,df.b.plus,df.c.plus,
                df.a.minus,df.b.minus,df.c.minus,
                df.a,df.b,df.c,
                df.sign1,df.sign2,df.sign3,
                df.num.solutions,
                df.sol.1,
                df.sol.2)
  zeros<-list()

  for(i in 1:(num.intervals-1)) {
#    print(paste("Iteration:",toString(i)))
    A_PLUS<-0
    B_PLUS<-0
    C_PLUS<-0
    A_MINUS<-0
    B_MINUS<-0
    C_MINUS<-0
    A<-0
    B<-0
    C<-0
    Solutions<-0
    Sol1<-0
    Sol2<-0    

    lim.inf <- df$df.interval.start[i]
    lim.sup <- df$df.interval.stop[i]

    FACTOR_PLUS <- 3/(4*h_plus*h_plus)
    X_i_PLUS <- K3_find.indexes (X_PLUS_FILTERED,lim.inf,lim.sup,h_plus)
    length_i_PLUS <- length(X_i_PLUS)
    if ( length_i_PLUS>0 ) {
      for(point in X_i_PLUS) {
        PROB_VALUE=PROB_VECTOR_PLUS_FILTERED[point]
        X=X_PLUS_FILTERED[point]
        A_PLUS <- PROB_VALUE * ( A_PLUS+(-1) )
        B_PLUS <- PROB_VALUE * ( B_PLUS+(2*X) )
        C_PLUS <- PROB_VALUE * ( C_PLUS+(h_plus*h_plus-X*X) )
      }
      CLASS_PLUS_PROB=sum(PROB_VECTOR_PLUS)/(sum(PROB_VECTOR_PLUS)+sum(PROB_VECTOR_MINUS))
      NI_PLUS_REAL=NI_PLUS*CLASS_PLUS_PROB
      FACTOR<-(FACTOR_PLUS/(NI_PLUS_REAL*h_plus))
      df$df.a.plus[i]=A_PLUS*FACTOR
      df$df.b.plus[i]=B_PLUS*FACTOR
      df$df.c.plus[i]=C_PLUS*FACTOR
    }

    FACTOR_MINUS <- 3/(4*h_minus*h_minus)
    X_i_MINUS <- K3_find.indexes (X_MINUS_FILTERED,lim.inf,lim.sup,h_minus)
    length_i_MINUS <- length(X_i_MINUS)
    if ( length_i_MINUS > 0 ) {
      for(point in X_i_MINUS) {
        PROB_VALUE=PROB_VECTOR_MINUS_FILTERED[point]
        X=X_MINUS_FILTERED[point]
        A_MINUS <- PROB_VALUE * ( A_MINUS+(-1) )
        B_MINUS <- PROB_VALUE * ( B_MINUS+(2*X) )
        C_MINUS <- PROB_VALUE * ( C_MINUS+(h_minus*h_minus-X*X) )
      }
      CLASS_MINUS_PROB=sum(PROB_VECTOR_MINUS)/(sum(PROB_VECTOR_PLUS)+sum(PROB_VECTOR_MINUS))
      NI_MINUS_REAL=NI_MINUS*CLASS_MINUS_PROB      
      FACTOR<-(FACTOR_MINUS/(NI_MINUS_REAL*h_minus))
      
      df$df.a.minus[i]=A_MINUS*FACTOR
      df$df.b.minus[i]=B_MINUS*FACTOR
      df$df.c.minus[i]=C_MINUS*FACTOR
    }
    
    A<-df$df.a.plus[i]-df$df.a.minus[i]
    B<-df$df.b.plus[i]-df$df.b.minus[i]
    C<-df$df.c.plus[i]-df$df.c.minus[i]
    
#    print(paste("A+:",toString(df$df.a.plus[i])," ","B+:",toString(df$df.b.plus[i])," ","C+:",toString(df$df.c.plus[i])))    
#    print(paste("A-:",toString(df$df.a.minus[i])," ","B-:",toString(df$df.b.minus[i])," ","C-:",toString(df$df.c.minus[i])))
#    print(paste("A:",toString(A)," ","B:",toString(B)," ","C:",toString(C)))

    df$df.a[i]=round(A,digits=4)
    df$df.b[i]=round(B,digits=4)
    df$df.c[i]=round(C,digits=4)    
    
    #print("## Correction in progress")
    #print(paste("A:",toString(abs(df$df.a[i]))," ","B:",toString(abs(df$df.b[i]))," ","C:",toString(abs(df$df.c[i]))))
    
    A_abs=abs(df$df.a[i])
    B_abs=abs(df$df.b[i])
    C_abs=abs(df$df.c[i])
    
    if ( A_abs<1e-5 & !(A_abs==0)) df$df.a[i]=0
    if ( B_abs<1e-5 & !(B_abs==0)) df$df.b[i]=0
    if ( C_abs<1e-5 & !(C_abs==0)) df$df.c[i]=0
    
    A<-df$df.a[i]
    B<-df$df.b[i]
    C<-df$df.c[i]
    zeros <- K3_Zeros(A,B,C)
    num.zeros <- length(zeros)
    if (num.zeros>0) {
      df$df.num.solutions[i]=num.zeros
      df$df.sol.1[i]=zeros[1]
      if (num.zeros==2) {
        df$df.sol.2[i]=zeros[2]
      }
    } else {
      df$df.num.solutions[i]=0
      df$df.sol.1[i]=0
      df$df.sol.2[i]=0
    }
  }
  #print("K2_density::END")
  return(df)
}
