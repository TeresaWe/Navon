list.dirs(path=getwd()) # to list subfolders/subdirs and use this info to go into the subfolders within a loop
# e.g. Sub-folders
#sub.folders <- list.dirs(parent.folder, recursive=TRUE)[-1]
filenames <- list.files(path=getwd()) # to list all files in a folder, ggf. define type of the file


#####FIRST set Navonfolder as Wd!

setwd("~/Teresa/files/Navon")

sub.folders <- list.files(path=getwd(), pattern="[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}",include.dirs = TRUE)
for (j in sub.folders) {
  path<-paste(getwd(),"/",j,sep="")
  filenames = list.files(path=path,pattern="[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_Navon_[0-9]{4}_[A-Za-z]{3}_[0-9]{2}_[0-9]{4}.csv")
  for (i in filenames) {
    name<-substr(j,1,10)
    name<-paste(name,"_Navon", sep="")
    assign(name,read.csv(paste(path,"/",i,sep=""))) #read in the table and name as "name"
  }
}