##########################################################################################
### Name  : discretizer
### Input : Method code for estimating the number of intervals.
###			Input folder
###			Train file
###			Test file
###			Output folder
###			Vector of variables to be discretized
###			Class position
###			Logging folder
### Output: Train/test discretized files according to method.
##########################################################################################
## Incluir Librerias
library(RWeka)
library(caret)
library(stringr)
library(discretization)

## Incluir Funciones
source("lib/logging.R")
source("lib/splitter.R")
source("lib/ef.R")
source("lib/ew.R")
source("lib/mcd.R")
source("lib/binsEstimator.R")

discretizerProb <- function(METHOD,InputFolder,TrainFile,TestFile,OutputFolder,vars,classPosVector,LogPath)
{
	DATE=format(Sys.Date(),"%y/%m/%d")
	TIME=format(Sys.time(), "%X")

	FILE=paste(DATE,",",TIME,".log")
	FILE<-str_replace_all(FILE,fixed(","),"")
	FILE<-str_replace_all(FILE,fixed("  "),".")
	FILE<-str_replace_all(FILE,fixed("/"),".")
	FILE<-str_replace_all(FILE,fixed(":"),".")
	FILE<-str_replace_all(FILE,fixed(" "),"")

	## Phase 1>> Initialization
	## -----------------------------------------------------------
	## Step 1. Load configuration from parameters
	logging(LogPath,FILE,"Phase 1","BEGIN","Parameter initialization")
	logging(LogPath,FILE,"Phase 1","Step 1: Log File",FILE)
	logging(LogPath,FILE,"Phase 1","Step 1: Algorithm",METHOD)

	## Step 2. Imprimir resumen del Run. - LOG
	logging(LogPath,FILE,"Phase 1","Step 2: Train Data file",TrainFile)
	logging(LogPath,FILE,"Phase 1","Step 2: Test  Data file",TestFile)
	logging(LogPath,FILE,"Phase 1","Step 2: Variable vector",toString(vars))
	logging(LogPath,FILE,"Phase 1","Step 2: Class position",toString(classPosVector))

	# Step 3. Creacion de variables globales - LOG
	FQTrainFile<-str_replace_all(paste(InputFolder,TrainFile),fixed(" "),"")
	db                = read.arff(FQTrainFile)
	data              = db[]
	data[is.na(data)] = 0
	dataDISCRETIZED   = data
	TrainNumInstances = length(data[,1])
	TrainNumVariables = length(data[1,])
	CLASS_DATA        = data[,c(classPosVector)]
	classDataNames    = names(data)

	logging(LogPath,FILE,"Phase 2","Step 1: Train File",TrainFile)
	logging(LogPath,FILE,"Phase 2","Step 2: Train File Instances",toString(TrainNumInstances))
	logging(LogPath,FILE,"Phase 2","Step 3: Train File Variables",toString(TrainNumVariables))

	FQTestFile<-str_replace_all(paste(InputFolder,TestFile),fixed(" "),"")
	dbTest            = read.arff(FQTestFile)
	dataTest          = dbTest[]
	dataTest[is.na(dataTest)] = 0
	dataTestDISCRETIZED = dataTest
	TestNumInstances = length(dataTest[,1])
	TestNumVariables = length(dataTest[1,])
	CLASS_DATA_TEST = dataTest[,classPosVector]
	classDataTestNames    = names(data)
	logging(LogPath,FILE,"Phase 2","Step 4: Test File",TestFile)
	logging(LogPath,FILE,"Phase 2","Step 5: Test File Instances",toString(TestNumInstances))
	logging(LogPath,FILE,"Phase 2","Step 6: Test File Variables",toString(TestNumVariables))

	removableVars=rep(0,length.out=length(vars))
	i=1
	NonDiscretizable=""
	Intervals=""
	for(var in vars)
	{
	  alpha=0.5
    del=0.05
	
    MCD_MODE=length(classPosVector)
    print(MCD_MODE)
    
#	  if (METHOD == "AMEVA"      ) discretization = disc.Topdown(data[c(var,classPos)],method=3)
#	  if (METHOD == "CACC"       ) discretization = disc.Topdown(data[c(var,classPos)],method=2)
#	  if (METHOD == "CAIM"       ) discretization = disc.Topdown(data[c(var,classPos)],method=1)
#	  if (METHOD == "CHI2"       ) discretization = chi2        (data[c(var,classPos)],alpha,del)
#	  if (METHOD == "CHIM"       ) discretization = chiM        (data[c(var,classPos)],alpha)
    if (METHOD == "EF"         ) discretization = ef          (data[,var],binsEstimator(1,length(data[,var])))
	  if (METHOD == "EW"         ) discretization = ew          (data[,var],binsEstimator(1,length(data[,var])))
    if (METHOD == "MCD"        ) {
      if (MCD_MODE == 2)  discretization = mcd         (data[,var],data[,classPosVector[1]],data[,classPosVector[2]],MCD_MODE)
      if (MCD_MODE == 1)  discretization = mcd         (data[,var],data[,classPosVector[1]],data[,classPosVector[1]],MCD_MODE)
    }
#	  if (METHOD == "EXTENDCHI2" ) discretization = extendChi2  (data[c(var,classPos)],alpha)
#	  if (METHOD == "MDLP"       ) discretization = mdlp        (data[c(var,classPos)])
#	  if (METHOD == "MODCHI2"    ) discretization = modChi2     (data[c(var,classPos)],alpha)
	  
    ZEROS=discretization$cutp[[1]]
    print(paste(toString(var),">> ",toString(ZEROS)))

    if ( is.character(ZEROS) ) {
      if ( ZEROS=="All") {
        ZEROS=data[,var]	  
        print(data[1:10,var])
      } else {
        print(ZEROS)
      }
    }    
    
    if (length(ZEROS)>0 & is.numeric(ZEROS)) {
	    
      dataDISCRETIZED    [,var] = splitter(data    [,var],ZEROS)
	    dataTestDISCRETIZED[,var] = splitter(dataTest[,var],ZEROS)
	    
	    myLevels=seq(from=1,to=length(ZEROS)+1)
	    myLength=length(dataTestDISCRETIZED[,var])
	    auxVar=seq(from=1,to=(myLength+length(ZEROS)+1))
	    auxVar[1:myLength]=dataTestDISCRETIZED[,var]
	    auxVar[(myLength+1):(myLength+length(ZEROS)+1)]=myLevels
	    auxVar=factor(auxVar,labels=myLevels)
	    dataTestDISCRETIZED[,var]=auxVar[1:myLength]
	    
	    myLength=length(dataDISCRETIZED[,var])
	    auxVar=seq(from=1,to=(myLength+length(ZEROS)+1))
	    auxVar[1:myLength]=dataDISCRETIZED[,var]
	    auxVar[(myLength+1):(myLength+length(ZEROS)+1)]=myLevels
	    auxVar=factor(auxVar,labels=myLevels)
	    dataDISCRETIZED[,var]=auxVar[1:myLength]        
	    Intervals=paste(Intervals,toString(length(ZEROS)+1))
	    logging(LogPath,FILE,"Phase 2",paste("Step 9:",METHOD,"Valid Variable         "),toString(var))
	    logging(LogPath,FILE,"Phase 2",paste("Step 9:",METHOD,"Discretization Sequence"),toString(ZEROS))
	    
	  } else {

	    removableVars[var]=var
	    logging(LogPath,FILE,"Phase 2",paste("Step 9:",METHOD,"Invalid Variable"),toString(var))
	  
	  }
	  
	  i=i+1
	  
	}

	nonDiscretized=which(removableVars!=0)
	dataDISCRETIZED     [,classPosVector] = CLASS_DATA
	dataTestDISCRETIZED [,classPosVector] = CLASS_DATA_TEST

	TRAIN_X_DISCRETIZED = dataDISCRETIZED[,-c(classPosVector,nonDiscretized)]
	TRAIN_Y_DISCRETIZED = dataDISCRETIZED[, classPosVector]

	TEST_X_DISCRETIZED  = dataTestDISCRETIZED[,-c(classPosVector,nonDiscretized)]
	TEST_Y_DISCRETIZED  = dataTestDISCRETIZED[,classPosVector]

	## Generar los ARFFs
	TRD<-data.frame(TRAIN_X_DISCRETIZED,TRAIN_Y_DISCRETIZED)
	names(TRD)<-classDataNames
	newTrainFile<-str_replace_all(TrainFile,fixed(".arff"),"")
	write.arff(TRD,paste(OutputFolder,newTrainFile,".DISCRETIZED.",METHOD,".arff",sep="")) 
	TSD<-data.frame(TEST_X_DISCRETIZED,TEST_Y_DISCRETIZED)
	names(TSD)<-classDataTestNames
	colnames(TSD)<-colnames(TRD)
	newTestFile<-str_replace_all(TestFile,fixed(".arff"),"")
	write.arff(TSD,paste(OutputFolder,newTestFile,".DISCRETIZED.",METHOD,".arff",sep=""))
}