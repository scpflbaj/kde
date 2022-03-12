##########################################################################################
### Name  : find_h_max
### Input : Vector de valores del atributo
### Output: Maximo valor de h
##########################################################################################
find_h_max<-function(z)
{
  h_max<-0
  min_z<-min(z)
  max_z<-max(z)
  h_max<-(max_z-min_z)/2
  return(h_max)
}
