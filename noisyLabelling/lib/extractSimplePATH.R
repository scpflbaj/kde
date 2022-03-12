##########################################################################################
### Name  : extractSimplePATH
### Input : dataset ID
###			repetition
###			fold
### Output: String with the fully qualified folder
###
##########################################################################################
extractSimplePATH <- function(dbID,REPETITION,FOLD)
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
  
  rnREPETITION<-sprintf("%02d",REPETITION)
  FQFN<-paste(db[dbID],".",toString(rnREPETITION),".")
  FQFN<-str_replace_all(string=FQFN, pattern=" ", repl="")
  return(FQFN)
}