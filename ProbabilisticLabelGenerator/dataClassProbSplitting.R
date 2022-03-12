  ## Clearing workspace
  rm(list=ls())
  
  ## Incluir Librerias
  library(RWeka)
  library(caret)
  library(klaR)
  library(stringr)
  library(ROCR)
  
  source("../noisyLabelling/lib/extractSimpleFN.R")
  source("../noisyLabelling/lib/extractSimplePATH.R")

  ## Control randomization
  set.seed(13579)
    
  ## Parametros de BETA
  ALPHA<-0.15
  BETA <-1
  
  ## Cargar fichero original bbdd18
  InputFolder  = "D:/OneDrive - IKERLAN S.COOP/[01.PhD]/[03.GITHUB]/data/NL00Performance/"
  OutputFolder = "D:/OneDrive - IKERLAN S.COOP/[01.PhD]/[03.GITHUB]/data/SIMLNL00/06.PerformanceDatasets/"
  
  ## Crear un fichero con los resultados de los cambios de clase esperado
  strTmp=Sys.time()
  strTmp=str_replace_all(strTmp," ","-")
  strTmp=str_replace_all(strTmp,":","-")
  strTmp=paste("A",ALPHA,"-B",BETA,"-",str_replace_all(strTmp," ","-"),".csv")
  validationFile = str_replace_all(paste(OutputFolder,str_replace_all(strTmp," ","")),"/ A0","/A0")
  
  #sink(validationFile)
  
  
  DBList       = seq(from=35,to=35)#  25)
  REPETITIONS  = seq(from=1,to=1)#  10)
  FOLDS        = seq(from=1,to=1)#  10)
  CHANGES      = 0
  N            = 0
  
  for( dbID in DBList)
  {
    for( repetition in REPETITIONS )
    {
     
      for( fold in FOLDS ) {
        
        print(dbID)
        
        CHANGES=0
        
        trainFile  = paste(InputFolder,extractSimpleFN (dbID,repetition,fold,"train"),sep="")
        testFile   = paste(InputFolder,extractSimpleFN (dbID,repetition,fold,"test" ),sep="")
        
        print(trainFile)
        print(testFile)
        
        dbTrain = read.arff(trainFile)
        dbTest  = read.arff(testFile)
         
        db = rbind(dbTrain,dbTest)
  
        N = length(db[,1])
        PRED = rbeta(N,shape1=ALPHA,shape2=BETA)
        
        # print(length(which(PRED>0.5)))
  
        C_PLUS  <- rep(0,length.out=N)
        C_MINUS <- rep(0,length.out=N)
              
        classPos = length(db[1,])
        
        ## Identificar las clases positivas
        idxPLUS  = which(db[,classPos]== 1) 
        ## Identificar las clases negativas
        idxMINUS = which(db[,classPos]==-1)
        
        ## Asignar en C_PLUS las probabilidades identificadas de PRED
        C_PLUS[idxPLUS]   = abs(1-PRED[idxPLUS])
        C_MINUS[idxPLUS]  = abs(C_PLUS[idxPLUS]-1)
        ## Asignar en C_MINUS 1-CPLUS identificadas de PRED
        C_MINUS[idxMINUS] = abs(1-PRED[idxMINUS])
        C_PLUS[idxMINUS]  = abs(C_MINUS[idxMINUS]-1)
        
        Y=c()
        for ( i in 1:length(db[,1]) ) {
          if ( C_PLUS[i]>= C_MINUS[i] ) Y[i]=1
          else Y[i]=-1
        }
        
        N_PLUS_CHANGES  =  length(which(Y [idxPLUS ] != db [idxPLUS ,classPos]))
        N_MINUS_CHANGES =  length(which(Y [idxMINUS] != db [idxMINUS,classPos]))
        
        CHANGES=N_PLUS_CHANGES+N_MINUS_CHANGES
  
        Y=factor(Y,levels=c(-1,1))
  
        trainSize = length(dbTrain[,1])
        testSize  = length(dbTest [,1])
  
        DataX.Train = db[1:trainSize                       ,-classPos]
        DataX.Test  = db[(trainSize+1):(trainSize+testSize),-classPos]
  
        Data.PY.Train = cbind(C_PLUS[1:trainSize],C_MINUS[1:trainSize])
        Data.Y.Train  = Y[1:trainSize]
        Data.PY.Test  = cbind(C_PLUS[(trainSize+1):(trainSize+testSize)],C_MINUS[(trainSize+1):(trainSize+testSize)])      
        Data.Y.Test   = Y[(trainSize+1):(trainSize+testSize)]
  
  
        Data.PTrain = cbind(DataX.Train,Data.PY.Train)
        Data.Train  = cbind(DataX.Train,Data.Y.Train )
        Data.PTest  = cbind(DataX.Test ,Data.PY.Test )
        Data.Test   = cbind(DataX.Test ,Data.Y.Test  )
  
        Data.P.Names = c(names(db[-classPos]),c("CLASS_PLUS","CLASS_MINUS"))
        Data.Names   = c(names(db[-classPos]),c("class"))
  
        db.Train.POut = data.frame ( Data.PTrain )
        db.Train.Out  = data.frame ( Data.Train  )
        db.Test.POut  = data.frame ( Data.PTest  )
        db.Test.Out   = data.frame ( Data.Test   )
  
        names ( db.Train.POut ) = Data.P.Names
        names ( db.Train.Out  ) = Data.Names
        names ( db.Test.POut  ) = Data.P.Names
        names ( db.Test.Out   ) = Data.Names
  
        db.Train.POut.FILENAME = paste(OutputFolder,"P",extractSimpleFN (dbID,repetition,fold,"train"),sep="")
        db.Train.Out.FILENAME  = paste(OutputFolder,"R",extractSimpleFN (dbID,repetition,fold,"train"),sep="")
        db.Test.POut.FILENAME  = paste(OutputFolder,"P",extractSimpleFN (dbID,repetition,fold,"test" ),sep="")
        db.Test.Out.FILENAME   = paste(OutputFolder,"R",extractSimpleFN (dbID,repetition,fold,"test" ),sep="")
  
        print(db.Train.POut.FILENAME )
        print(db.Train.Out.FILENAME  )
        print(db.Test.POut.FILENAME  )
        print(db.Test.Out.FILENAME   )
        
        write.arff(db.Train.POut, db.Train.POut.FILENAME )
        write.arff(db.Train.Out , db.Train.Out.FILENAME  )
        write.arff(db.Test.POut , db.Test.POut.FILENAME  )
        write.arff(db.Test.Out  , db.Test.Out.FILENAME   )
        fileCon <- file(validationFile, "a")
        strLine=paste(toString(dbID),",",toString(repetition),",",toString(fold),", N (",toString(N),"),C (",toString(CHANGES),"), P=",toString(CHANGES/N))
        print(strLine)
        writeLines(strLine,con=fileCon)
        close(fileCon)
      }
    }
  }
  