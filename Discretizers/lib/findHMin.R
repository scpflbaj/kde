##########################################################################################
### Name  : find_h_min
### Input : Vector de valores del atributo
### Output: Minimo valor de h
##########################################################################################
find_h_min<-function(z)
{
  z<-sort(z)
  min=abs(z[1]-z[length(z)])
  D_h=min
  for(i in 1:(length(z)-1)) {
    a=z[i]
    b=z[i+1]
    D_h=abs(a-b)
    if (D_h<min && D_h>0) min=D_h
  }
  return(min) ## ORIGINAL: Funcionamiento correcto min*2
} 