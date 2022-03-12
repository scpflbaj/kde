mcd<-function(data, class1,class2,MCD_MODE) 
{ 

  
  CLASS_DATA_PLUS  =  1
  CLASS_DATA_MINUS = -1
  N=length(data)
  cp=c()
  ## Ordenar
  idx=order(data)
  
#  print(data)
#  print(data[idx])
#  print(class1[idx])
#  print(class2[idx])
  
  ## discretizar
#  print(paste(toString(data[idx[1]])," ",toString(class1[idx[1]]),"-",toString(toString(class2[idx[1]]))))
  k=1
  if (MCD_MODE==2) {
    if (class1[idx[1]] >= class2[idx[1]]) {
      majoritoryClass=CLASS_DATA_PLUS
    } else {
      majoritoryClass=CLASS_DATA_MINUS
    }    
  } else {

  }
  

  if ( MCD_MODE==2) {
    for(i in 2:N) {
      #    print(paste(toString(data[idx[i-1]])," ",toString(data[idx[i]])," ",toString(majoritoryClass),"-",toString(class1[idx[i]]),"-",toString(toString(class2[idx[i]]))))
      if (class1[idx[i]] >= class2[idx[i]]) {
        instantMajoritory=CLASS_DATA_PLUS
        if (majoritoryClass!=instantMajoritory) {
          majoritoryClass=CLASS_DATA_PLUS
          cp[k]=(data[idx[i-1]]+data[idx[i]])/2
          k=k+1
          #        print(paste("==>",toString(majoritoryClass)))
        }
      }
      if (class1[idx[i]] < class2[idx[i]]) {
        instantMajoritory=CLASS_DATA_MINUS
        if (majoritoryClass!=instantMajoritory) {
          majoritoryClass=CLASS_DATA_MINUS
          cp[k]=(data[idx[i-1]]+data[idx[i]])/2
          k=k+1
          #        print(paste("==>",toString(majoritoryClass)))
        }
      }
    }    
  }
  if (MCD_MODE==1) {
    class1Numeric=as.numeric(levels(class1[idx])[class1[idx]])
#    print(class1Numeric)
    
    majoritoryClass=class1[idx[1]]
    for(i in 2:N) {
      #    print(paste(toString(data[idx[i-1]])," ",toString(data[idx[i]])," ",toString(majoritoryClass),"-",toString(class1[idx[i]]),"-",toString(toString(class2[idx[i]]))))
      if (class1Numeric[idx[i]] >= class1Numeric[idx[i-1]]) {
        instantMajoritory=CLASS_DATA_PLUS
        if (majoritoryClass!=instantMajoritory) {
          majoritoryClass=CLASS_DATA_PLUS
          cp[k]=(data[idx[i-1]]+data[idx[i]])/2
          k=k+1
          #        print(paste("==>",toString(majoritoryClass)))
        }
      }
      if (class1Numeric[idx[i]] < class1Numeric[idx[i-1]]) {
        instantMajoritory=CLASS_DATA_MINUS
        if (majoritoryClass!=instantMajoritory) {
          majoritoryClass=CLASS_DATA_MINUS
          cp[k]=(data[idx[i-1]]+data[idx[i]])/2
          k=k+1
          #        print(paste("==>",toString(majoritoryClass)))
        }
      }
    }
  }
  
  dataDiscretized=splitter(data,cp)
  result=list(cutp=cp,Disc.data=dataDiscretized)	
  return(result)
}

#x  = c(0.471,0.662,0.378,0.764, 0.413, 0.627,0.489,0.427,0.231,0.364,0.493,0.378,0.556,0.200,0.613,0.453,0.507,0.271,0.556,0.187)
#y11 = c(0.823538,0.735034,0.936872,0.856727,0.629039,0.575029,0.528504,0.882292,0.829251,0.098849,0.876434,0.081285,0.953034,0.513039,0.611669,0.497377,0.673327,0.521077,0.677134,0.246028)
#y12 = c(0.176462,0.264966,0.063128,0.143273,0.370961,0.424971,0.471496,0.117708, 0.170749,0.901151,0.123566,0.918715,0.046966,0.486961,0.388331,0.502623,0.326673,0.478923,0.322866,0.753972 )
#y21 = c(1,1,1,1,1,1,1,1,1,-1,1,-1,1,-1,1,-1,1,1,1,-1)
