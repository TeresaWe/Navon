#script to calculate the SACS measures from AGLTresults table

# SACS = Z(%)-Z(RT) Speed accuracy composite scores
# high score--> efficient performance, low score --> poor performance

# calculate this for each congruency separation condition  and all trials 

library("dplyr", lib.loc="/usr/local/lib/R/site-library")
Navonresults<-data.frame(Navonresults)
######### L_con ##############

ACC<-Navonresults[,1]/40
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-Navonresults[,2]
SACS_Lcon=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)

######### L_incon ##############

ACC<-Navonresults[,3]/40
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-Navonresults[,4]
SACS_Linc=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)


######### G_con ##############

ACC<-Navonresults[,5]/40
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-Navonresults[,6]
SACS_Gcon=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)


######### G_incon ##############

ACC<-Navonresults[,7]/40
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-Navonresults[,8]
SACS_Ginc=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)


######### all ###############

ACC<-(Navonresults[,1]+Navonresults[,3]+Navonresults[,5]+Navonresults[,7])/160
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-(Navonresults[,2]+Navonresults[,4]+Navonresults[,6]+Navonresults[,8])/4
SACS_all=qnorm(ACC)-qnorm(RT)
rm(ACC,RT)

Navon_SACS<-cbind(SACS_Lcon,SACS_Linc,SACS_Gcon,SACS_Ginc,SACS_all)
Navon_SACS<-data.frame(Navon_SACS)
colnames(Navon_SACS)<-c("vis_SACS_Lcon","vis_SACS_Linc","vis_SACS_Gcon","vis_SACS_Ginc","vis_SACS_all")
rownames(Navon_SACS)<-rownames(Navonresults)

#ggf

Navonresults<-cbind(Navonresults,Navon_SACS)
