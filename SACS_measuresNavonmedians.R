#script to calculate the SACS measures from AGLTresults table

# SACS = Z(%)-Z(RT) Speed accuracy composite scores
# high score--> efficient performance, low score --> poor performance

# calculate this for each congruency separation condition  and all trials 

library("dplyr", lib.loc="/usr/local/lib/R/site-library")
#Navon_medianresults<-data.frame(Navon_medianresults)
######### L_con ##############
ACC_Lcon<-Navon_medianresults$vcorrect_Lcon/40
ACC_Gcon<-Navon_medianresults$vcorrect_Gcon/40
ACC_Linc<-Navon_medianresults$vcorrect_Linc/40
ACC_Ginc<-Navon_medianresults$vcorrect_Ginc/40



mean_ACC<-mean(c(ACC_Lcon,ACC_Gcon,
                 ACC_Linc,ACC_Ginc))
sd_ACC<-sd(c(ACC_Lcon,ACC_Gcon,
             ACC_Linc,ACC_Ginc))
mean_RT<-mean(c(Navon_medianresults$vMAD_Lcon,Navon_medianresults$vMAD_Gcon,
                 Navon_medianresults$vMAD_Linc,Navon_medianresults$vMAD_Ginc),na.rm=TRUE)
sd_RT<-sd(c(Navon_medianresults$vMAD_Lcon,Navon_medianresults$vMAD_Gcon,
             Navon_medianresults$vMAD_Linc,Navon_medianresults$vMAD_Ginc),na.rm=TRUE)



ACC<-Navon_medianresults$vcorrect_Lcon/40
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-Navon_medianresults$vMAD_Lcon
SACS_Lcon=((ACC-mean_ACC)/sd_ACC)-((RT-mean_RT)/sd_RT)
rm(ACC,RT)

######### L_incon ##############

ACC<-Navon_medianresults$vcorrect_Linc/40
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-Navon_medianresults$vMAD_Linc
SACS_Linc=((ACC-mean_ACC)/sd_ACC)-((RT-mean_RT)/sd_RT)
rm(ACC,RT)


######### G_con ##############

ACC<-Navon_medianresults$vcorrect_Gcon/40
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-Navon_medianresults$vMAD_Gcon
SACS_Gcon=((ACC-mean_ACC)/sd_ACC)-((RT-mean_RT)/sd_RT)
rm(ACC,RT)


######### G_incon ##############

ACC<-Navon_medianresults$vcorrect_Ginc/40
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-Navon_medianresults$vMAD_Ginc
SACS_Ginc=((ACC-mean_ACC)/sd_ACC)-((RT-mean_RT)/sd_RT)
rm(ACC,RT)


######### all ###############

ACC<-(Navon_medianresults$vcorrect_Lcon+Navon_medianresults$vcorrect_Linc+Navon_medianresults$vcorrect_Gcon+Navon_medianresults$vcorrect_Ginc)/160
ACC[ACC==1]<-0.95
ACC[ACC==0]<-0.0001
RT<-(Navon_medianresults$vMAD_Lcon+Navon_medianresults$vMAD_Linc+Navon_medianresults$vMAD_Gcon+Navon_medianresults$vMAD_Ginc)/4
SACS_all=((ACC-mean_ACC)/sd_ACC)-((RT-mean_RT)/sd_RT)
rm(ACC,RT)

Navon_SACSmedian<-cbind(SACS_Lcon,SACS_Linc,SACS_Gcon,SACS_Ginc,SACS_all)
Navon_SACSmedian<-data.frame(Navon_SACSmedian)
colnames(Navon_SACSmedian)<-c("med_vis_SACS_Lcon","med_vis_SACS_Linc","med_vis_SACS_Gcon","med_vis_SACS_Ginc","vis_SACS_all")

Navon_medianresults<-cbind(Navon_medianresults,Navon_SACSmedian) 

