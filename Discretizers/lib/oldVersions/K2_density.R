############################################################################
##
##  K2_density 
##    INPUT : Datos ordenados de las clases + y -
##    OUTPUT: Lista de ceros, 
##            Lista de intervalos con los valores de los coeficientes
############################################################################
source("lib/K2_Zeros.R")
source("lib/K2_ZeroFiltering.R")
source("lib/K2_find.indexes.R")

K2_density <- function(X_PLUS,X_MINUS,NI_PLUS,NI_MINUS,h_plus,h_minus,min,max,PROB_VECTOR_PLUS,PROB_VECTOR_MINUS) {
  interval_PLUS_LEFT <- X_PLUS-h_plus
  interval_PLUS_RIGHT <- X_PLUS+h_plus
  interval_MINUS_LEFT <- X_MINUS-h_minus
  interval_MINUS_RIGHT <- X_MINUS+h_minus  
  intervals <- c(interval_PLUS_LEFT,interval_MINUS_LEFT,interval_PLUS_RIGHT,interval_MINUS_RIGHT)
  intervals <- unique(intervals)
  intervals <- sort(intervals)
  
  print("**")
  print(intervals)
  print("**")
  
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
    X_i_PLUS <- K2_find.indexes (X_PLUS,lim.inf,lim.sup,h_plus)
    length_i_PLUS <- length(X_i_PLUS)
    if ( length_i_PLUS>0 ) {
      for(point in 1:length(X_i_PLUS)) {
        
        PROB_VALUE=PROB_VECTOR_PLUS[point]
        
        A_PLUS <- PROB_VALUE * ( A_PLUS+(-1) )
        B_PLUS <- PROB_VALUE * ( B_PLUS+(2*X_PLUS[X_i_PLUS[point]]) )
        C_PLUS <- PROB_VALUE * ( C_PLUS+(h_plus*h_plus-X_PLUS[X_i_PLUS[point]]*X_PLUS[X_i_PLUS[point]]) )
        
#        print(paste(toString(A_PLUS),"-",toString(B_PLUS),"-",toString(C_PLUS)))
      }
      FACTOR<-(FACTOR_PLUS/(NI_PLUS*h_plus))
      
      FACTOR_PROB = 1/sum(PROB_VECTOR_PLUS)
      
      df$df.a.plus[i]=A_PLUS*FACTOR*FACTOR_PROB
      df$df.b.plus[i]=B_PLUS*FACTOR*FACTOR_PROB
      df$df.c.plus[i]=C_PLUS*FACTOR*FACTOR_PROB
    }
    
    FACTOR_MINUS <- 3/(4*h_minus*h_minus)
    X_i_MINUS <- K2_find.indexes (X_MINUS,lim.inf,lim.sup,h_minus)
    length_i_MINUS <- length(X_i_MINUS)
    if ( length_i_MINUS > 0 ) {
      for(point in 1:length(X_i_MINUS)) {
        
        PROB_VALUE=PROB_VECTOR_MINUS[point]
        
        A_MINUS <- PROB_VALUE * ( A_MINUS+(-1) )
        B_MINUS <- PROB_VALUE * ( B_MINUS+(2*X_MINUS[X_i_MINUS[point]]) )
        C_MINUS <- PROB_VALUE * ( C_MINUS+(h_minus*h_minus-X_MINUS[X_i_MINUS[point]]*X_MINUS[X_i_MINUS[point]]) )
      }
      FACTOR<-(FACTOR_MINUS/(NI_MINUS*h_minus))
      
      FACTOR_PROB = 1/sum(PROB_VECTOR_MINUS)
      
      df$df.a.minus[i]=A_MINUS*FACTOR*FACTOR_PROB
      df$df.b.minus[i]=B_MINUS*FACTOR*FACTOR_PROB
      df$df.c.minus[i]=C_MINUS*FACTOR*FACTOR_PROB
    }
    A<-df$df.a.plus[i]-df$df.a.minus[i]
    B<-df$df.b.plus[i]-df$df.b.minus[i]
    C<-df$df.c.plus[i]-df$df.c.minus[i]
    
    df$df.a[i]=A
    df$df.b[i]=B
    df$df.c[i]=C
    
    zeros <- K2_Zeros(A,B,C)
    num.zeros <- length(zeros)
    if (num.zeros>0) {
      df$df.num.solutions[i]=num.zeros
      df$df.sol.1[i]=zeros[1]
      if (num.zeros==2) {
        df$df.sol.2[i]=zeros[2]
      }
    }
  }
  return(df)
}

# x_plus = c(1,1,1,1,5,5,5,5)
# x_minus = c(3,3,3,3,7,7,7,7)
# 
# min <- min(x_plus,x_minus)
# max <- max(x_plus,x_minus)
# 
# h_ref=2
# h_plus=h_ref
# n_plus=length(x_plus)
# n_minus=length(x_minus)
# fraction=n_plus/n_minus
# h_minus=fraction*h_plus
# 
# min_X=min(x_plus,x_minus)-max(h_plus,h_minus)
# max_X=max(x_plus,x_minus)+max(h_plus,h_minus)
# 
# df1 <- K2_density(x_plus,x_minus,length(x_plus),length(x_minus),h_plus,h_minus,min_X,max_X)
# df1.filtered <- K2_ZeroFiltering(c(x_plus,x_minus),df1,min,max)
# # 
# K2_PlotDensity(df1,min,max,num.points = 1000)
# K2_PlotDensity(df1.filtered,min,max,num.points = 1000)
# 
# x_plus = c(43.536999,32.428146,4.704181,86.543053,35.619991,96.786597,16.885177,56.639274,88.148435,69.984368)
# x_minus = c(98.25115,47.16505,34.26199,77.13294,61.23612,10.02633,84.74071,60.44801,81.22854,47.12621)
# 
# x_p=sort(x_plus)
# x_n=sort(x_minus)
# 
# min.x=min(x_p,x_n)
# max.x=max(x_p,x_n)
# 
# h_ref=3.5
# h_plus=h_ref
# n_plus=length(x_plus)
# n_minus=length(x_minus)
# fraction=n_plus/n_minus
# h_minus=fraction*h_plus
# kernel_samples=100
# 
# 
# t1 <- Sys.time()
# df2 <- K2_density(x_p,x_n,length(x_p),length(x_n),h_plus,h_minus,1-h_plus,5+h_plus)
# df2.filtered <- K2_ZeroFiltering(c(x_p,x_n),df2,min.x,max.x)
# t2 <- Sys.time()
# print(t1)
# print(t2)
# print(t2-t1)
# K2_PlotDensity(df2,min.x,max.x)
# K2_PlotDensity(df2.filtered,min.x,max.x)
# tmp1=df2.filtered$df.sol.1[which(df2.filtered$df.num.solutions>0)]
# tmp2=df2.filtered$df.sol.2[which(df2.filtered$df.num.solutions>0)]
# tmp11=tmp1[which(is.na(tmp1)==FALSE)]
# tmp21=tmp2[which(is.na(tmp2)==FALSE)]
# zeros=sort(c(tmp1,tmp2))
# print(zeros)

