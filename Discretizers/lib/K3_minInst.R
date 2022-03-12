############################################################################
##
##  K2_minInst 
##    INPUT : Vector data  (Atributo)
##			  Vector class (Clase)
##			  Vector zeros (Secuencia de discretizacion)
##
##    OUTPUT: Lista del numero de instancias de la clase minoritaria y el total local.
############################################################################
K3_minInst <- function(data,class,zeros) {
  min<-list()
  min[[1]]<-c(0) ## Menor
  min[[2]]<-c(0) ## Total
  min[[3]]<-c(0) ## Signo
  min[[4]]<-c(0) ## Mayor
  min[[5]]<-zeros ## Puntos de corte
  ni<-0
  ni_plus<-0
  ni_minus<-0

  indexes  = which(data < zeros[1])
  ni       = length(indexes)
  ni_plus  = length(which(class[indexes]== 1))
  ni_minus = length(which(class[indexes]==-1))
  if (ni_plus  < ni_minus ) {
    min[[1]][1]<-ni_plus
    min[[3]][1]<-1
    min[[4]][1]<-ni_minus
  }
  if (ni_minus < ni_plus  ) {
    min[[1]][1]<-ni_minus
    min[[3]][1]<--1
    min[[4]][1]<-ni_plus
  }
  min[[2]][1]<-ni
  
  for(zero.index in 1:(length(zeros)-1)) {
  	indexes  = which(data >= zeros[zero.index] & data < zeros[zero.index+1])
  	ni       = length(indexes)
  	ni_plus  = length(which(class[indexes]== 1))
  	ni_minus = length(which(class[indexes]==-1))
  	if (ni_plus  < ni_minus ) {
  	  min[[1]][zero.index+1]<-ni_plus
  	  min[[3]][zero.index+1]<-1
  	  min[[4]][zero.index+1]<-ni_minus
  	}
  	if (ni_minus < ni_plus  ) {
  	  min[[1]][zero.index+1]<-ni_minus
  	  min[[3]][zero.index+1]<--1
  	  min[[4]][zero.index+1]<-ni_plus
  	}
  	if (is.na(min[[2]][zero.index+1])) min[[2]][zero.index+1]<-0
  	min[[2]][zero.index+1]<-min[[2]][zero.index+1]+ni
  }
  
  indexes  = which(data >= zeros[length(zeros)])
  ni       = length(indexes)
  ni_plus  = length(which(class[indexes]== 1))
  ni_minus = length(which(class[indexes]==-1))
  if (ni_plus  < ni_minus ) {
    min[[1]][length(zeros)+1]<-ni_plus
    min[[3]][length(zeros)+1]<-1
    min[[4]][length(zeros)+1]<-ni_minus    
  }
  if (ni_minus < ni_plus  ) {
    min[[1]][length(zeros)+1]<-ni_minus
    min[[3]][length(zeros)+1]<--1
    min[[4]][length(zeros)+1]<-ni_plus        
  }
  min[[2]][length(zeros)+1]<-ni
  
  return(min)
}

## Caso 1: Probar con todos los intervalos equiprobables
## Caso 2: Probar con 3/4 para una clase.

## Test case 1: 24 instancias, 4 intervalos
# x=seq(from=1,to=24,by=1)
# y=c(-1,-1,-1,1,1,1,-1,-1,-1,1,1,1,-1,-1,-1,1,1,1,-1,-1,-1,1,1,1)
# zeros<-c(0.5,6.5,12.5,18.5)
# print(K2_minInst(x,y,zeros))

## Test case 2: 24,instancias, 4 intervalos (3/4)
# x=seq(from=1,to=24,by=1)
# y=c(-1,1,1,1,1,1,-1,1,1,1,1,1,-1,1,1,1,1,1,-1,1,1,1,1,1)
# zeros<-c(0.5,6.5,12.5,18.5)
# print(K2_minInst(x,y,zeros))