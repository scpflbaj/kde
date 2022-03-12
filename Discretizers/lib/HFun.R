##########################################################################################
### Name  : HFunQ
### Input : Vector de valores del atributo, Numero de valores de h
### Output: X1,Y1 Original (X=H de alta resolucion,Y=Nº de puntos del entorno)
###         Y1,X1 Cambio de los ejes
###         X2,Y2 Cambio de escala en X y de proporcion 
###   		 (X2=Y1 escalado a numHs valores)
###    		 (Y2=X1 escalado a [0,1] y numHs valores) 	
##########################################################################################
HFun<-function(z,numHs)
{
  Fun     = list()
  z       = sort(z)
  h_min   = find_h_min(z)
  h_max   = find_h_max(z)
  h_vect  = seq(from=h_min,to=h_max,length.out=numHs)
  if ( h_min>=h_max ) {
    h_vect = seq(from=h_min/numHs,to=h_max,length.out=numHs)
    print(h_min)
    print(h_max)
    print("HFun::ERR02: h min >= h max")
  }
  print(h_vect)
  Fun = list(H=h_vect)
  return(Fun)
}
