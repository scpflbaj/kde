##########################################################################################
### Name  : extractPATH
### Input : dataset ID
###			repetition
###			fold
### Output: String with the fully qualified folder
###
##########################################################################################
extractPATH <- function(dbID,REPETITION,FOLD)
{
  db<-c()
  db[1]="01.Horse.Colic"
  db[2]="02.Credit.Aproval"
  db[3]="03.German.Credit"
  db[4]="04.Pima.Indian.Diabetes"
  db[5]="05.Haberman.Survival"
  db[6]="06.Ionosphere"
  db[7]="07.Bupa.Liver.Disorders"
  db[8]="08.Sonar.Mines.vs.Rocks"
  db[9]="09.SPECT.Heart"
  db[10]="10.Banknote.Authentication"
  db[11]="11.Blood.Transfusion"
  db[12]="12.Climate.Simulation.Crashes"
  db[13]="13.Planning.Relax"
  db[14]="14.Appendicitis"
  db[15]="15.SA.Heart"
  db[16]="16.Musk1"
  db[17]="17.Parkinsons"
  db[18]="18.Badges"
  db[19]="19.Glass2"
  db[20]="20.Indian.Liver.Patient"
  db[21]="21.Vertebral.Column"
  db[22]="22.Mamographic.Mass"
  db[23]="23.Cylinder.Bands"
  db[24]="24.Heart.Desease.Hungarian"
  db[25]="25.Leukemia.Haslinger"
  db[26]="26.AD.01"
  db[27]="27.AD.02"
  
  FQFN<-paste(db[dbID],"/FINAL/",toString(REPETITION),"/")
  FQFN<-str_replace_all(string=FQFN, pattern=" ", repl="")
  return(FQFN)
}