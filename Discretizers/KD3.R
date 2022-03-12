      ## Clearing workspace
      rm(list=ls())
      t1=Sys.time()
      ## Incluir Librerias
      library(RWeka)
      library(caret)
      library(klaR)
      library(stringr)
      library(ROCR)

      ## Incluir Funciones
      source("lib/logging.R")
      source("lib/findHMin.R")
      source("lib/findHMax.R")
      source("lib/HFun.R")
      source("lib/splitter.R")
      source("lib/K3_density.R")
      source("lib/K3_ZeroFiltering.R")
      source("lib/K3_minInst.R")
      source("../data.splitting/CreateContinuousStratifiedCrossFolds.R")
      
      OriginalPATH=getwd()
      setwd("../classifiers")
      source("cNB.R")
      source("df2cDF.R")
      setwd(OriginalPATH)
      
      FILE=paste(format(Sys.Date(),"%y/%m/%d"),",",format(Sys.time(), "%X"),".log")
      FILE<-str_replace_all(FILE,fixed(","),"")
      FILE<-str_replace_all(FILE,fixed("  "),".")
      FILE<-str_replace_all(FILE,fixed("/"),".")
      FILE<-str_replace_all(FILE,fixed(":"),".")
      FILE<-str_replace_all(FILE,fixed(" "),"")
      ## Phase 1>> Inicializacion del RUN
      ## -----------------------------------------------------------
      ## Step 1. Cargar Run File, el formato sera la fecha.
      args <- commandArgs(trailingOnly = TRUE)
      if (length(args)==0) {
        RunFile           = "../../../data/PDatabase.10000.csv" # ORIGINAL: args[1]
        RUNINFO           = read.csv(RunFile)
        InputFolder       = "../../../data/SIMLNL00/06.PerformanceDatasets/"# ORIGINAL
        #InputFolder       = ""# TEST
        # 
        PTrainFile        = "P10000.01.01.train.arff" # ORIGINAL: args[3]
        PTestFile         = "P10000.01.01.test.arff"  # ORIGINAL: args[4]
        RTrainFile        = "R10000.01.01.train.arff" # ORIGINAL: args[5]
        RTestFile         = "R10000.01.01.test.arff" # ORIGINAL: args[6]
        
        vars              = RUNINFO[ ,4]
        # classPos          = RUNINFO[1,5]
        #LogPath           = "../../../logs/" #ORIGINAL: ORIGINAL
        LogPath           = ""
        #OutputFolder      = "../../../output/discretized/" # ORIGINAL
        OutputFolder      = ""
        numHs             = 100 # ORIGINAL: as.numeric(args[9])        
      } else {
        RunFile           = args[1]
        RUNINFO           = read.csv(RunFile)
        InputFolder       = args[2]
        # 
        PTrainFile        = args[3]
        PTestFile         = args[4]
        RTrainFile        = args[5]
        RTestFile         = args[6]
        
        vars              = RUNINFO[ ,4]
        # classPos          = RUNINFO[1,5]
        LogPath           = args[7]
        OutputFolder      = args[8] 
        numHs             = as.numeric(args[9])
      }

      print(">>> BEGIN SUMMARY <<<")
      print(RunFile)
      print(RUNINFO)
      print(InputFolder)
      print(PTrainFile)
      print(PTestFile)
      print(RTrainFile)
      print(RTestFile)
      print(vars)
      print(LogPath)
      print(OutputFolder)
      print(numHs)
      print(">>> END SUMMARY <<<")
    
      #InputFolder    = "D:/OneDrive - IKERLAN S.COOP/[01.PhD]/[03.GITHUB]/data/SIMLNL00/Beta.03"
      #TrainFile      = "P10.01.01.train.arff"
      #TestFile       = "P10.01.01.test.arff"
      #vars           = c(21)
      numClassValues = 2
      #LogPath        = "D:/"
      #OutputFolder   = "D:/OneDrive - IKERLAN S.COOP/[01.PhD]/[03.GITHUB]/data/SIMLNL00/"
      #numHs          = 100

	    print(PTrainFile)
	    print(PTestFile)
	    print(RTrainFile)
	    print(RTestFile)

      logging(LogPath,FILE,"Phase 1","BEGIN","Parameter initialization")
      logging(LogPath,FILE,"Phase 1","Step 1: Log File",FILE)
      
      ## Step 2. Imprimir resumen del Run. - LOG
      logging(LogPath,FILE,"Phase 1","Step 2: Data folder",InputFolder)
      logging(LogPath,FILE,"Phase 1","Step 2: Variable vector",toString(vars))   
      ## RUNS
      ## Step 3. Leer/almacenar TRAIN - LOG
      FQTrainFile<-str_replace_all(paste(InputFolder,PTrainFile),fixed(" "),"")
      print(FQTrainFile)
      db                = read.arff(FQTrainFile)
      data              = db[]
      data[is.na(data)] = 0
      varsTotal         = length(data[1,])-2
      classPos          = varsTotal+1
      
      removableVars     = rep(0,length.out=varsTotal)
      
      dataDISCRETIZED   = data[,1:(varsTotal+1)]
      names(dataDISCRETIZED)[classPos]="class"
      TrainNumInstances = length(data[,1])
      TrainNumVariables = length(data[1,])
      
      logging(LogPath,FILE,"Phase 1","Step 2: Class position",toString(classPos))
      logging(LogPath,FILE,"Phase 1","Step 2: Number of Hs",toString(numHs))   
      logging(LogPath,FILE,"Phase 1","Step 3: Train File",PTrainFile)
      logging(LogPath,FILE,"Phase 1","Step 3: Train File Instances",toString(TrainNumInstances))
      logging(LogPath,FILE,"Phase 1","Step 3: Train File Variables",toString(TrainNumVariables))
      ## Step 4. Leer/almacenar TEST - LOG
      FQTestFile<-str_replace_all(paste(InputFolder,RTestFile),fixed(" "),"")
      dbTest            = read.arff(FQTestFile)
      dataTest          = dbTest[]
      dataTest[is.na(dataTest)] = 0
      dataTestDISCRETIZED = dataTest[]
      names(dataTestDISCRETIZED)[classPos]="class"
      TestNumInstances = length(dataTest[,1])
      TestNumVariables = length(dataTest[1,])
      
      logging(LogPath,FILE,"Phase 1","Step 3: Test File",RTestFile)
      logging(LogPath,FILE,"Phase 1","Step 3: Test File Instances",toString(TestNumInstances))
      logging(LogPath,FILE,"Phase 1","Step 3: Test File Variables",toString(TestNumVariables))      

      ## Step 4. Leer/almacenar TEST - LOG
      FQPTestFile<-str_replace_all(paste(InputFolder,PTestFile),fixed(" "),"")
      print(InputFolder)
      print(FQPTestFile)
      dbPTest            = read.arff(FQPTestFile)
      dataPTest          = dbPTest[]
      dataPTest[is.na(dataPTest)] = 0
      dataPTestDISCRETIZED = dataPTest[]
      names(dataPTestDISCRETIZED)[classPos]="class"
      PTestNumInstances = length(dataPTest[,1])
      PTestNumVariables = length(dataPTest[1,])
      
      logging(LogPath,FILE,"Phase 1","Step 3: PTest File",PTestFile)
      logging(LogPath,FILE,"Phase 1","Step 3: PTest File Instances",toString(PTestNumInstances))
      logging(LogPath,FILE,"Phase 1","Step 3: PTest File Variables",toString(PTestNumVariables))                  
      # Step 5. Creacion de variables globales - LOG
  
      n.rows=length(vars)
      n.cols=numHs
      H = array(dim=c(n.rows,n.cols))
      print(H)
      HReal = array(dim=c(length(vars)))
      strACC_Method=c("Approximate","Exact")
      ACC_METHOD = 2 ## 0 - Approximate, 1 - Exact
      ACC_CHANGE_CRITERIA = 0 ## 0 > Method, 1 >= Method
      ACC_CH_CRIT=c(">",">=")  
      ACC = array(dim=c(length(vars),numHs))
      Best.ACC = array(dim=c(length(vars)))
      HRange = 1.0
      Exponent = 1.0
      DP = vector("list",length(vars))
      logging(LogPath,FILE,"Phase 1","Step 4: ACC Method",toString(ACC_METHOD))
      logging(LogPath,FILE,"Phase 1","Step 4: Array H","Created")
      logging(LogPath,FILE,"Phase 1","Step 4: Array ACC","Created")
      logging(LogPath,FILE,"Phase 1","Step 4: H range",toString(HRange))
      logging(LogPath,FILE,"Phase 1","Step 4: Exponent",toString(Exponent))
      logging(LogPath,FILE,"Phase 1","Step 5: Global variables creation","SUCCESS!")      
      ## Step 6. Calcular los vectores H para todas las variables indicadas. - LOG
#      print(length(vars))
#      print(numHs)
      for(i in 1:length(vars)) {
        for(j in 1:numHs) {
          H  [i,j]=0 
          ACC[i,j]=0
        }
        Best.ACC[i]=0
      }
      logging(LogPath,FILE,"Phase 1","Step 6: H matrix","Initialized to zero")
      cat(paste("Vars >>",toString(vars)))
      for( var in vars)
      {
        z=data[,var]
        hconfig=HFun(z,numHs)
        logging(LogPath,FILE,"Phase 1","Step 7: Estimating H values",paste("Variable", toString(var)))
        logging(LogPath,FILE,"Phase 1","Step 7: Estimating H values",paste("Variable", toString(hconfig$H)))     
        title=paste(PTrainFile,"/ Var",toString(var),"-",names(data[var]))
        H[which(vars==var),]=hconfig$H
        HReal[which(vars==var)]=length(which(unique(hconfig$H)!=0))
        if (length(unique(is.na(H[which(vars==var),])))>1) {
          logging(LogPath,FILE,"Phase 1","Step 7: NA Value DETECTED","ERROR")  
          NAIndexes=which(is.na(H[which(vars==var),]))
          if (NAIndexes>1) H[which(vars==var),NAIndexes]=H[which(vars==var),NAIndexes-1]
          else H[which(vars==var),1]=H[which(vars==var),2]
          logging(LogPath,FILE,"Phase 1","Step 7: NA Value CORRECTED","REPLACED")
        }  
      }
      strHFile=paste(LogPath,"H.Values.",FILE)
      strHFile=str_replace_all(strHFile,fixed(" "),"")
      print(strHFile)
      HFile<-file(strHFile,"a")
      logging(LogPath,FILE,"Phase 1","Step 8: Writting H values","Started")      
      for( var in vars) {
        
        # ORIGINAL: for( h in 1:numHs)
        for( h in 1:HReal[which(vars==var)])
        {
          strLine=paste(toString(var),",",toString(H[which(vars==var),h]))
          strLine=str_replace_all(strLine,fixed(" "),"")
          write(strLine,HFile)
        }
      
      }
      close(HFile)
      
     logging(LogPath,FILE,"Phase 1","Step 8: Writting H values","Finished")
     logging(LogPath,FILE,"Phase 1","END  ","Parameters READY")
     logging(LogPath,FILE,"Phase 2","Discretization Policy Calculation ","STARTED")
     for(var in vars) {
        bestACC=0
        REPETITIONS=5
        for(repetition in 1:REPETITIONS) {
          folds=5
          df=CreateStratifiedCrossFolds(length(data[,1]),folds)
          for(i in 1:folds) {
            print(paste("Variable: ",toString(var)," Repetition: ",toString(repetition)," Fold: ",toString(i)))
            C_PLUS_POS   = length(data[1,]) - 1
            C_MINUS_POS  = C_PLUS_POS + 1        
            
            TRAIN_DF          = data[-df$InstanceId[df$Fold==i],var]
            TEST_DF           = data[ df$InstanceId[df$Fold==i],var]
            
            CLASS_DATA_PLUS        = data[-df$InstanceId[df$Fold==i],C_PLUS_POS]
            CLASS_DATA_MINUS       = data[-df$InstanceId[df$Fold==i],C_MINUS_POS]
            
            CLASS_DATA_TEST_PLUS   = data[df$InstanceId[df$Fold==i],C_PLUS_POS]
            CLASS_DATA_TEST_MINUS  = data[df$InstanceId[df$Fold==i],C_MINUS_POS]
            
            CLASS_DATA        = c()
            CLASS_DATA_TEST   = c()
            
            for(z in 1:length(data[-df$InstanceId[df$Fold==i],1])) {
              item_plus=CLASS_DATA_PLUS[z]
              item_minus=CLASS_DATA_MINUS[z]
              if (item_plus >= item_minus) CLASS_DATA[z]=1
              else CLASS_DATA[z]=-1
            }
            CLASS_DATA=factor(CLASS_DATA,levels=c(-1,1))
            
            for(z in 1:length(data[df$InstanceId[df$Fold==i],1])) {
              item_plus=CLASS_DATA_TEST_PLUS[z]
              item_minus=CLASS_DATA_TEST_MINUS[z]
              if (item_plus >= item_minus) CLASS_DATA_TEST[z]=1
              else CLASS_DATA_TEST[z]=-1
            }
            CLASS_DATA_TEST=factor(CLASS_DATA,levels=c(-1,1))
            
            temp            = cbind(TRAIN_DF,CLASS_DATA)
            temp_class      = as.numeric(levels(CLASS_DATA))[CLASS_DATA]

            X          = temp[,1]
            #print(X[1:10])
            
            X_PLUS     = X    # No se puede dsicriminar por positivo o negativo
            X_MINUS    = X    # No se puede dsicriminar por positivo o negativo
            
            Y          = temp_class           
            
            TEST_DF         = dataTest[,var]
            CLASS_DATA_TEST = factor(dataTest[,C_PLUS_POS],levels=c(-1,1))

            temp_test       = cbind(TEST_DF,CLASS_DATA_TEST)
            temp_class_test = as.numeric(levels(CLASS_DATA_TEST))[CLASS_DATA_TEST]
            X_TEST          = temp_test[,1]
            Y_TEST          = temp_class_test

            NI_PLUS    = length(X)
            NI_MINUS   = length(X)
            
            #print(HReal[which(vars==var)])
            ####
            #### Medicion del tiempo: START
            ####
            t_start=Sys.time()
            for(h in 1:HReal[which(vars==var)])
            {
              
              #print(paste("START > ",toString(t_start)))
#              print(paste("Variable: ",toString(var)," Repetition: ",toString(repetition)," Fold: ",toString(i), " H: ",toString(h)))
              
              kernel_samples       = 2^(floor(log2(10.1*TrainNumInstances)))
              kernel_samples       = max(kernel_samples,4096)

              X_PLUS_PROB  = data[-df$InstanceId[df$Fold==i],C_PLUS_POS ]
              X_MINUS_PROB = data[-df$InstanceId[df$Fold==i],C_MINUS_POS]
              
              h_plus = H[which(vars==var),h]
              fraction = NI_PLUS/NI_MINUS
              fraction = sum(X_PLUS_PROB)/sum(X_MINUS_PROB)
              #fraction = sum(X_MINUS_PROB)/sum(X_PLUS_PROB)
              
              h_minus = h_plus*fraction

              #print(paste("FRACTION=",toString(fraction),"H+:",toString(h_plus)," SUM(P+)=",toString(sum(X_PLUS_PROB))," H-:",toString(h_minus)," SUM(P-)=",toString(sum(X_MINUS_PROB))))
              
              min_X=min(X)
              max_X=max(X)
              
              instanceThreshold<-log2((NI_PLUS+NI_MINUS)+1)
              instanceThreshold<-1        
              #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Threshold  ")  ,toString(instanceThreshold))

              df2 <- K3_density(X_PLUS,X_MINUS,NI_PLUS,NI_MINUS,h_plus,h_minus,min_X,max_X,X_PLUS_PROB,X_MINUS_PROB)
              ZEROS=K3_ZeroFiltering(c(X_PLUS,X_MINUS),df2,min_X,max_X,instanceThreshold)
              ISG.AUC<-0
              numZeros=length(ZEROS)
              #print(numZeros)
              if (numZeros>0) {
                ## TRAIN DISCRETIZATION
                #print(ZEROS)
                DX.TRAIN = splitter(X,ZEROS)
                Levels.Temp = seq(from=1,to=numZeros+1)
                Length.Train.Temp = length(DX.TRAIN)
                
                auxVar.Train.Temp                                                       = seq(from=1,to=(Length.Train.Temp+numZeros+1))
                auxVar.Train.Temp[1:Length.Train.Temp]                                  = DX.TRAIN
                auxVar.Train.Temp[(Length.Train.Temp+1):(Length.Train.Temp+numZeros+1)] = Levels.Temp
                auxVar.Train.Temp                                                       = factor(auxVar.Train.Temp,labels=Levels.Temp)
                
                DX.TRAIN = auxVar.Train.Temp[1:Length.Train.Temp]

                ##
                intervalStats = K3_minInst(X,Y,ZEROS)
                #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Statistics  ")  ,toString(intervalStats))
                #print(intervalStats)
                ## TEST DISCRETIZATION
                
                DX.TEST = splitter(X_TEST,ZEROS)
                
                Length.Test.Temp=length(DX.TEST)
                
                auxVar.Test.Temp                                                        = seq(from=1,to=(Length.Test.Temp+numZeros+1))
                auxVar.Test.Temp[1:Length.Test.Temp]                                    = DX.TEST
                auxVar.Test.Temp[(Length.Test.Temp+1):(Length.Test.Temp+numZeros+1)]    = Levels.Temp
                auxVar.Test.Temp                                                        = factor(auxVar.Test.Temp,labels=Levels.Temp)   
                
                DX.TEST=auxVar.Test.Temp[1:Length.Test.Temp]

                Y_TEST[which(is.na(Y_TEST))]=-1
                
                
                Y      = factor(Y     ,levels=c(-1,1))
                Y_TEST = factor(Y_TEST,levels=c(-1,1))

                ## Hay que construir dataframes con dos variables clase con probabilidad
                ##
                ## Construir Train
                ## Construir Test
                DF.TRAIN=data.frame(DX.TRAIN,CLASS_DATA_PLUS,CLASS_DATA_MINUS)
                names(DF.TRAIN) = c("X","CLASS_PLUS","CLASS_MINUS")

                DF.TEST = data.frame(DX.TEST,Y_TEST)
                names(DF.TEST ) = c("X","TEST_Y_DISCRETIZED")
                # print("DX.TEST")
                #print(DX.TEST)
                # print("Y_TEST")
                #print(Y_TEST)
                # print("DF.TEST")
                #print(DF.TEST)
                cDF<-df2cDF(DF.TRAIN)
                NB<-learn(cDF)
                #print("predict")
                performance<-predict(NB,DF.TEST)
                ACC.Temp=as.numeric(performance[[4]])
                #print(ACC.Temp)
              } else {
                ACC.Temp=0
                ISG.AUC<-0
              }
              #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","AUC.ISG[var,h] "),toString(ISG.AUC))      
              #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","AUC.CAR[var,h] "),toString(ACC.Temp))
              #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Zeros  ") ,toString(ZEROS))
              #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Num zeros ") ,toString(numZeros))   
              #print(bestACC)
              #print(ACC.Temp)
              #print(bestACC)
              #print(ACC.Temp)
              if (is.nan(ACC.Temp)) ACC.Temp=0
              if (bestACC < ACC.Temp) {
                bestACC=ACC.Temp
                DP[[which(vars==var)]]=ZEROS
                Best.ACC[which(vars==var)]=ACC.Temp
                #print(paste("Variable: ",toString(var)," Repetition: ",toString(repetition)," Fold: ",toString(i), " H: ",toString(h)," Best AUC:",toString(bestACC)))
#                print(paste("Best ACC:",toString(bestACC)))
                #repetition<-1
                #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Best Policy          "),toString(DP[[which(vars==var)]]))
                #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Best Policy (Metric) "),toString(bestACC))
                #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Best Policy (H +   ) "),toString(h_plus))
                #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Best Policy (H -   ) "),toString(h_minus))                
              }
              ####
              #### Medicion del tiempo: STOP
              ####

              #print(paste("STOP  > ",toString(t_stop)))
              ACC[which(vars==var),h]=ACC.Temp
              
              
            }
            t_stop=Sys.time()
            print(paste("TOTAL = ",toString(t_stop-t_start)))
            #if (!(sd(ACC[which(vars==var),])!=0)) {
            #  logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","Variable NOT USEFUL "),"TRUE")    
            #}
            #logging(LogPath,FILE,"Phase 2",paste("<",toString(var),",",toString(repetition),",",toString(i),",",toString(h),">","H Iteration"),"TRUE")
          }
        }
      }
      ## END-01 Fin
      ## Phase 2>> Resultados: Conjunto de politicas de discretizacion
     
     ## 
     ## Identificar que variables NO tienen puntos de corte.
     ## 
     ## newVars
     for(var in vars) {
       X_Zeros = DP[[which(vars==var)]]
       numZeros   = length(X_Zeros)
       if (numZeros==0) {
         removableVars[var]=1
         print(paste("Variable: ",toString(var)," EXCLUDED"))
       }
     }
     
     #logging(LogPath,FILE,"Phase 2","Discretization Policy Calculation ","FINISHED")     
      ##  Discretizacion del TRAIN - LOG
      ##  Discretizacion del TEST - LOG
     policyFile<-str_replace_all(paste(OutputFolder,PTrainFile),fixed("train.arff"),"policy.csv")
     policyFile<-str_replace_all(policyFile,fixed(" "),"")
     f<-file(description = policyFile,open = "w")
      for(var in vars)
      {
        CLASS_DATA = data[,classPos]
        X          = data[,var]
        X_Zeros    = DP[[which(vars==var)]]
        X_TEST     = dataTest[,var]
        numZeros   = length(X_Zeros)
        
#        print(X_Zeros)
#        print(var)
        
        if (numZeros>0) {
#          print(numZeros)
	        myZeros<-c(min(X,X_TEST),X_Zeros,max(X,X_TEST))          
          cat(str_replace_all(paste(toString(var),",",toString(myZeros),"\n"),fixed(" "),""),file=f)
          
          DX = splitter(X,X_Zeros)
          myLevels=seq(from=1,to=numZeros+1)
          myLength=length(DX)
          auxVar=seq(from=1,to=(myLength+numZeros+1))
#          print(auxVar)
          auxVar[1:myLength]=DX
          auxVar[(myLength+1):(myLength+numZeros+1)]=myLevels
          auxVar=factor(auxVar,labels=myLevels)
          dataDISCRETIZED[,var]=auxVar[1:myLength]  
          #logging(LogPath,FILE,"Phase 3",paste("[",toString(var),"]","TRAIN Variable AUC         "),toString(Best.ACC[which(vars==var)]))          
          #logging(LogPath,FILE,"Phase 3",paste("[",toString(var),"]","TRAIN Variable Policy      "),toString(X_Zeros))
          #logging(LogPath,FILE,"Phase 3",paste("[",toString(var),"]","TRAIN Variable Original    "),toString(X))
          #logging(LogPath,FILE,"Phase 3",paste("[",toString(var),"]","TRAIN Variable Discretized "),toString(dataDISCRETIZED[,var]))              
          
#          print("Pre-Split Test")
#          print(X_Zeros)
#          print(X_TEST)
          DX_TEST = splitter(X_TEST,X_Zeros)
#          print("Post-Split Test")
          myLevels=seq(from=1,to=numZeros+1)
          myLength=length(DX_TEST)
          auxVar=seq(from=1,to=(myLength+numZeros+1))
          auxVar[1:myLength]=DX_TEST
          auxVar[(myLength+1):(myLength+numZeros+1)]=myLevels
          auxVar=factor(auxVar,labels=myLevels)   
          dataTestDISCRETIZED[,var]=auxVar[1:myLength]
#          print(dataTestDISCRETIZED)

          #logging(LogPath,FILE,"Phase 3",paste("[",toString(var),"]","TEST  Variable Original    "),toString(X_TEST))
          #logging(LogPath,FILE,"Phase 3",paste("[",toString(var),"]","TEST  Variable Discretized "),toString(dataTestDISCRETIZED[,var]))                        
        }
      }
     close(f)
      #logging(LogPath,FILE,"Phase 3","Discretizing Database ","FINISHED")
      
      ## Phase 3>> Resultados: BBDD (Variables seleccionadas) discretizada.
      ##
      ## Phase 4>> Aprendizaje del NB
      ##
#      CLASS_DATA      = data[,classPos]
#      temp            = cbind(data[,var],CLASS_DATA)
#      temp_class      = as.numeric(levels(CLASS_DATA))[CLASS_DATA] 
#      Y               = temp_class      
      
#      CLASS_DATA_TEST = dataTest[,length(vars)+1]
#      temp_class      = as.numeric(levels(CLASS_DATA_TEST))[CLASS_DATA_TEST] 
#      Y_Test          = temp_class
      
      
      ## Calculo de la clase mayoritaria[,-((varsTotal+1):(varsTotal+length(numClassValues)))]
      TEST_Y_DISCRETIZED = dataTestDISCRETIZED[, varsTotal+1]
      aux=CLASS_DATA
      
      for(i in 1:length(CLASS_DATA)) {
        if ( data[i,C_PLUS_POS] >= data[i,C_MINUS_POS] ) aux[i]=1
        else aux[i]=-1
      }
      CLASS_DATA=aux
      CLASS_DATA=factor(CLASS_DATA,levels=c(-1,1))
      
      dataDISCRETIZED     [,varsTotal+1] = CLASS_DATA
      dataTestDISCRETIZED [,varsTotal+1] = CLASS_DATA_TEST

      TRAIN_X_DISCRETIZED = dataDISCRETIZED     [,-((varsTotal+1):(varsTotal+length(numClassValues)))]
      excludedVars=length(which(removableVars==1))
      if (excludedVars >0) TRAIN_X_DISCRETIZED = TRAIN_X_DISCRETIZED [,-which(removableVars==1)]
      TRAIN_Y_DISCRETIZED = dataDISCRETIZED     [, varsTotal+1  ]
      
      TEST_X_DISCRETIZED  = dataTestDISCRETIZED [,-((varsTotal+1):(varsTotal+length(numClassValues)))]
      excludedVars=length(which(removableVars==1))
      if (excludedVars >0) TEST_X_DISCRETIZED  = TEST_X_DISCRETIZED  [,-which(removableVars==1)]
      TEST_Y_DISCRETIZED  = dataTestDISCRETIZED [, varsTotal+1  ]
      
      ## Generar los ARFFs con etiquetas de clase
      TRD<-data.frame(TRAIN_X_DISCRETIZED,TRAIN_Y_DISCRETIZED)
      newTrainFile<-str_replace_all(RTrainFile,fixed(".arff"),"")
      write.arff(TRD,paste(OutputFolder,newTrainFile,".DISCRETIZED.KERNEL.arff",sep=""))
      
      TSD<-data.frame(TEST_X_DISCRETIZED,TEST_Y_DISCRETIZED)
      colnames(TSD)<-colnames(TRD)
      newTestFile<-str_replace_all(RTestFile,fixed(".arff"),"")
      
      FQFN=paste(OutputFolder,newTestFile,".DISCRETIZED.KERNEL.arff",sep="")
      write.arff(TSD,FQFN)
      t2=Sys.time()
      
      ## Generar los ARFFs con probabilidades de clase
      TRAIN_Y_DISCRETIZED = data      [,(varsTotal+1):(varsTotal+2)]
      TEST_Y_DISCRETIZED  = dataPTest [,(varsTotal+1):(varsTotal+2)]
      TRD_PROB<-data.frame(TRAIN_X_DISCRETIZED,TRAIN_Y_DISCRETIZED)
      if (excludedVars >0) {
        colnames(TRD_PROB)=colnames(db)[-(which(removableVars==1))]
      } else {
        colnames(TRD_PROB)<-colnames(db)
      }
      
      newTrainFile<-str_replace_all(PTrainFile,fixed(".arff"),"")
      write.arff(TRD_PROB,paste(OutputFolder,newTrainFile,".DISCRETIZED.KERNEL.PROB.arff",sep=""))
      TSD_PROB<-data.frame(TEST_X_DISCRETIZED,TEST_Y_DISCRETIZED)
      if (excludedVars >0) {
        colnames(TSD_PROB)=colnames(db)[-(which(removableVars==1))]
      } else {
        colnames(TSD_PROB)<-colnames(db)
      }
      newTestFile<-str_replace_all(PTestFile,fixed(".arff"),"")
      FQFN=paste(OutputFolder,newTestFile,".DISCRETIZED.KERNEL.PROB.arff",sep="")
      write.arff(TSD_PROB,FQFN)
      t2=Sys.time()      
      
      print(t1)
      print(t2)
       
