K2_ZeroFiltering <- function(data,df,min,max,threshold) {
  NUM_DIGITS<-6
  NUM_DIGITS2<-6
  items.removed<-0
  df$df.sol.1<-round(df$df.sol.1,digits=NUM_DIGITS)
  df$df.sol.2<-round(df$df.sol.2,digits=NUM_DIGITS)  
  df.filtered <- df
  num.intervals <- length(df$df.interval.start)
  ## Eliminar soluciones repetidas
  # repeated.solutions<-which(df.filtered$df.sol.1==df.filtered$df.sol.2)
  # for(i in repeated.solutions) {
  #   df.filtered$num.solutions[i]<-1
  #   df.filtered$df.sol.2[i]<-NA
  # }
  
  for(interval in 1:num.intervals) {
    if (is.na(df$df.num.solutions[interval]) == FALSE) {
      if (df$df.num.solutions[interval] > 0) {
        start <- round(df$df.interval.start[interval],digits=NUM_DIGITS)
        stop <- round(df$df.interval.stop[interval],digits=NUM_DIGITS)
        x.zero <- round(df$df.sol.1[interval],digits=NUM_DIGITS)
        
        ## RULE 1: La solucion 1 debe estar dentro del intervalo
        if (x.zero <= start | x.zero > stop | x.zero <= min | x.zero >= max) {
          #print(paste("RULE 1:",toString(df.filtered$df.sol.1[interval])))
          df.filtered$df.num.solutions[interval] <- df.filtered$df.num.solutions[interval] - 1
          df.filtered$df.sol.1[interval] <- NA
          items.removed <- items.removed + 1
        }
        
        if (df$df.num.solutions[interval] > 1 ) {
          x.zero <- round(df$df.sol.2[interval],digits=4)
          ## RULE 2: La solucion 2 debe estar dentro del intervalo
          if (x.zero <= start | x.zero > stop | x.zero < min | x.zero > max ) {
            #print(paste("RULE 2:",toString(df.filtered$df.sol.2[interval])))
            df.filtered$df.num.solutions[interval] <- df.filtered$df.num.solutions[interval] - 1 
            df.filtered$df.sol.2[interval] <- NA
            items.removed <- items.removed + 1
          } else {
              ## RULE 3: La segunda solucion debe ser diferente de la primera
              if ( df.filtered$df.num.solutions[interval] == 2 ) {
                if ( df.filtered$df.sol.1[interval] == df.filtered$df.sol.2[interval] ) {
                  #print(paste("RULE 3:",toString(df.filtered$df.sol.1[interval])))
                  df.filtered$df.num.solutions[interval] <- df.filtered$df.num.solutions[interval] - 1
                  df.filtered$df.sol.2[interval] <- NA
                  items.removed <- items.removed + 1
                }
              } 
            # }           
          }
        }
      }      
    }
  }
  print(":>")
  print(items.removed)
  ## RULE 4: Debe haber un cambio de clase
  for(interval in 1:num.intervals) {
    A <- df.filtered$df.a[interval]
    B <- df.filtered$df.b[interval]
    C <- df.filtered$df.c[interval]
    if ( is.na(df.filtered$df.num.solutions[interval])==FALSE) {
      if ( df.filtered$df.num.solutions[interval] == 0 ) {
        x <- (df.filtered$df.interval.start[interval]+df.filtered$df.interval.stop[interval])/2
        y <- round(A*x*x + B*x + C,digits=6)    
        signo=sign(y)
        df.filtered$df.sign1[interval] <- signo
        df.filtered$df.sign2[interval] <- signo
        df.filtered$df.sign3[interval] <- signo
      } 

      if ( df.filtered$df.num.solutions[interval] == 1 ) {
        a <- df.filtered$df.interval.start[interval]
        if ( is.na(df.filtered$df.sol.1[interval])==FALSE ) {
          b <- df.filtered$df.sol.1[interval]  
        } else {
          b <- df.filtered$df.sol.2[interval]
        }
        x <- (a+b)/2
        y <- round(A*x*x + B*x + C,digits=NUM_DIGITS2)
        signo <- sign(y)
        df.filtered$df.sign1[interval] <- signo
        
        x <- (b+df.filtered$df.interval.stop[interval])/2
        y <- round(A*x*x + B*x + C,digits=6)    
        signo <- sign(y)
        df.filtered$df.sign2[interval] <- signo
        df.filtered$df.sign3[interval] <- signo 
        if ( df.filtered$df.sign1[interval] == df.filtered$df.sign2[interval] ) {
          print(paste("RULE 4:",toString(df.filtered$df.sol.1[interval])))
          if (df.filtered$df.num.solutions[interval]>0) {
            df.filtered$df.num.solutions[interval] <- df.filtered$df.num.solutions[interval] - 1            
          }
          df.filtered$df.sol.1[interval] <- NA
          items.removed <- items.removed + 1
        }
      } else {
        if ( df.filtered$df.num.solutions[interval] == 2 ) {
          x <- (df.filtered$df.interval.start[interval]+df.filtered$df.sol.1[interval])/2
          y <- round(A*x*x + B*x + C,digits=NUM_DIGITS2)    
          signo=sign(y)
          df.filtered$df.sign1[interval]=signo
          
          x <- (df.filtered$df.sol.1[interval]+df.filtered$df.sol.2[interval])/2
          y <- round(A*x*x + B*x + C,digits=NUM_DIGITS2)    
          signo=sign(y)
          df.filtered$df.sign2[interval]=signo   
          
          ## Analizar si se acepta el zero
          if ( df.filtered$df.sign1[interval]==df.filtered$df.sign2[interval] ) {
            print(paste("RULE 5:",toString(df.filtered$df.sol.1[interval])))
            if (df.filtered$df.num.solutions[interval]>0) {
              df.filtered$df.num.solutions[interval] <- df.filtered$df.num.solutions[interval] - 1            
            }
            df.filtered$df.sol.1[interval] <- NA
            items.removed <- items.removed + 1
          }
          x <- (df.filtered$df.sol.2[interval]+df.filtered$df.interval.stop[interval])/2
          y <- round(A*x*x + B*x + C,digits=NUM_DIGITS2)    
          signo=sign(y)
          df.filtered$df.sign3[interval]=signo    
          
          ## Analizar si se acepta el zero
          if ( df.filtered$df.sign2[interval]==df.filtered$df.sign3[interval] ) {
            print(paste("RULE 6:",toString(df.filtered$df.sol.1[interval])))
            df.filtered$df.num.solutions[interval] <- df.filtered$df.num.solutions[interval] - 1
            df.filtered$df.sol.2[interval] <- NA
            items.removed <- items.removed + 1
          }
        }
      }
    }
  }
  ## RULE 5: Debe haber al menos n instancias entre los ceros, es decir, en cada intervalo.
  sol1=df$df.sol.1[which(is.na(df.filtered$df.sol.1)==FALSE)]
  sol2=df$df.sol.2[which(is.na(df.filtered$df.sol.2)==FALSE)]
  zeros=sort(c(sol1,sol2))
  
  ##
  ## Si el ultimo cero es el ultimo punto: eliminarlo como solucion valida
  ##  
  if ( length(which(zeros[length(zeros)]==df.filtered$df.interval.stop[length(df.filtered$df.interval.stop)]))>0 ) {
      removable.indexes.sol.1<-which(zeros[length(zeros)]==df.filtered$df.sol.1)
      removable.indexes.sol.2<-which(zeros[length(zeros)]==df.filtered$df.sol.2)
      df.filtered$df.sol.1[removable.indexes.sol.1]<-NA
      df.filtered$df.sol.2[removable.indexes.sol.2]<-NA
      df.filtered$df.num.solutions[removable.indexes.sol.1]<-df.filtered$df.num.solutions[removable.indexes.sol.1]-1      
      df.filtered$df.num.solutions[removable.indexes.sol.2]<-df.filtered$df.num.solutions[removable.indexes.sol.2]-1
  }
  sol1=df$df.sol.1[which(is.na(df.filtered$df.sol.1)==FALSE)]
  sol2=df$df.sol.2[which(is.na(df.filtered$df.sol.2)==FALSE)]
  zeros=sort(c(sol1,sol2))
  ##
  ##
  ##
  if (length(zeros)>1) {
    for(zero.index in 1:(length(zeros)-1)) {
      if (length(which(data >= zeros[zero.index] & data < zeros[zero.index+1])) < threshold) {
        index=which(df.filtered$df.sol.1==zeros[zero.index] | df.filtered$df.sol.2==zeros[zero.index])
        local.length<-length(df.filtered[,1])
        df.filtered$df.num.solutions[1:local.length] <- 0
        df.filtered$df.sol.1[1:local.length]        <- NA
        df.filtered$df.sol.2[1:local.length]        <- NA
      } 
    }
  }
  ## Posible parche que no estaba exigiendo puntos en el ultimo intervalo.
  ## Es necesario probar las dos condiciones!! Sin parche y con parche
  if (length(which(data >= zeros[length(zeros)])) < threshold ) {
    index=which(df.filtered$df.sol.1==zeros[length(zeros)] | df.filtered$df.sol.2==zeros[length(zeros)])
    local.length<-length(df.filtered[,1])
    df.filtered$df.num.solutions[1:local.length] <- 0
    df.filtered$df.sol.1[1:local.length]        <- NA
    df.filtered$df.sol.2[1:local.length]        <- NA
  } 
  
  print(paste("Filtered zeros:",toString(items.removed)))
  return(df.filtered)
}