##########################################################################################
### Name  : extractSimpleFN
### Input : dataset ID
###			repetition
###			fold
###			type (train/test)
### Output: String with the fully qualified pathname of file
###
##########################################################################################
extractSimpleFN <- function(dbID,REPETITION,FOLD,TYPE)
{
  db<-c()
  db[1]="01."
  db[2]="02."
  db[3]="03."
  db[4]="04."
  db[5]="05."
  db[6]="06."
  db[7]="07."
  db[8]="08."
  db[9]="09."
  db[10]="10."
  db[11]="11."
  db[12]="12."
  db[13]="13."
  db[14]="14."
  db[15]="15."
  db[16]="16."
  db[17]="17."
  db[18]="18."
  db[19]="19."
  db[20]="20."
  db[21]="21."
  db[22]="22."
  db[23]="23."
  db[24]="24."
  db[25]="25."
  
  db[26]="1000."
  db[27]="2000."
  db[28]="3000."
  db[29]="4000."
  db[30]="5000."
  db[31]="6000."
  db[32]="7000."
  db[33]="8000."
  db[34]="9000."
  db[35]="10000."
  
  dbPath<-db[dbID]
  train<-sprintf("%02d",dbID)
  fold<-sprintf("%02d",FOLD)
  if (TYPE=="train") type<-".train.arff"
  if (TYPE=="test") type<-".test.arff"
  FN<-paste(fold,type)
  
  rnREPETITION<-sprintf("%02d",REPETITION)
  FQFN<-paste(db[dbID],toString(rnREPETITION),".",FN)
  FQFN<-str_replace_all(string=FQFN, pattern=" ", repl="")
  return(FQFN)
}