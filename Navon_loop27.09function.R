#working directory
setwd("~/Teresa/files/Navon")
#packages
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
# first read in ALL .csv tables (local and global)!
# e.g. AR20RED171_AGLT <- read.csv("~/files/AGLT/AR20RED171_AGLT_2017_Jul_05_1408.csv")
##loop over all AGLT result files##

file_list_Navon<-ls(pattern="[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_Navon{1}")

Navonloop<-function(filelist){
  Navonresults<-numeric(8*length(filelist)) 
  for (i in 1:length(filelist)) {
    tableNavon<-get(filelist[i])
    L_con <- Navon_L_con(tableNavon)
    L_incon<-Navon_L_incon(tableNavon)
    G_con<-Navon_G_con(tableNavon)
    G_incon<-Navon_G_incon(tableNavon)
    Navonresults[((8*i)-7):(8*i)]<- c(L_con,L_incon,G_con,G_incon)
    i=i+1
  }
  dim(Navonresults)<-c(8,length(file_list_Navon))
  rownames(Navonresults)<-c("vis_L_con_corr", "vis_L_con_meanRT",
                           "vis_L_incon_corr", "vis_L_incon_meanRT","vis_G_con_corr", 
                          "vis_G_con_meanRT","vis_G_incon_corr", "vis_G_incon_meanRT")
  colnames(Navonresults)<-substr(file_list_Navon, start=1, stop=10) 
  Navonresults<-t(Navonresults)
  assign("Navonresults", Navonresults, envir=globalenv())
  return (Navonresults)
}
Navonloop(file_list_Navon)
