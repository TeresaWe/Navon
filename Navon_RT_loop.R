#working directory
setwd("~/files/Navon")
#packages
library(dplyr)
# first read in ALL .csv tables (local and global)!
# e.g. AR20RED171_AGLT <- read.csv("~/files/AGLT/AR20RED171_AGLT_2017_Jul_05_1408.csv")
##loop over all AGLT result files##

file_list_Navon<-ls(pattern="[A-Z]{2}[0-9]{2}[A-Z]{3}[0-9]{3}_Navon{1}")

NavonRTloop<-function(filelist){
  NavonRT<-numeric(length(filelist)) 
  for (i in 1:length(filelist)) {
    tableNavon<-get(filelist[i])
    L_con <- Navon_Lcon_RT(tableNavon)
    L_incon<-Navon_Linc_RT(tableNavon)
    G_con<-Navon_Gcon_RT(tableNavon)
    G_incon<-Navon_Ginc_RT(tableNavon)
    NavonRT[((320*i)-319):(320*i)]<- c(L_con,L_incon,G_con,G_incon)
    i=i+1
  }
  dim(NavonRT)<-c(320,length(file_list_Navon))
  #rownames(Navonresults)<-c("vis_L_con_corr", "vis_L_con_meanRT",
   #                         "vis_L_incon_corr", "vis_L_incon_meanRT","vis_G_con_corr", 
   #                         "vis_G_con_meanRT","vis_G_incon_corr", "vis_G_incon_meanRT")
  colnames(NavonRT)<-substr(file_list_Navon, start=1, stop=10) 
  NavonRT<-t(NavonRT)
  assign("NavonRT", NavonRT, envir=globalenv())
  return (NavonRT)
}
NavonRTloop(file_list_Navon)
