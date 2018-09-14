library(tidyr)
library(ggplot2)
#Navon exclude cases (missudnerstood L/G=5)+EEG_exclusions(5)!
allresults_Navon<-subset(allresults, VP_Code!="OS12GRO173") #change back to all if exclusions eeg not needed,#or change to other EEG_results table (eeg. delta)
allresults_Navon<-subset(allresults_Navon, VP_Code!="EL01KES159")
allresults_Navon<-subset(allresults_Navon, VP_Code!="YL24DRO161")
allresults_Navon<-subset(allresults_Navon, VP_Code!="RI15SON966")
allresults_Navon<-subset(allresults_Navon, VP_Code!="AZ17FRA106")
EL01EIN659


##mixed hlm 2(inc/con)x2(L/G)x 2(AP/RP) visuell
allresults_longvis <- gather(allresults_Navon, time, value, vis_L_con_meanRT, vis_L_incon_meanRT,vis_G_con_meanRT,vis_G_incon_meanRT)
LG<-character(236)
LG[1:118]<-"L"
LG[119:236]<- "G"
congruency<-character(236)
congruency[1:59]<-"inc"
congruency[119:177]<- "inc"
congruency[60:118]<-"con"
congruency[178:236]<-"con"
allresults_longvis <- data.frame(allresults_longvis,LG,congruency)
#lmer(LG ~timeLG+(timeLG|AP.0.RP.1.AP.)+timeL+(timeL|AP.0.RP.1.AP.)+timeG+(timeG|AP.0.RP.1.AP.)+AP.0.RP.1.AP., data = allresults_long2)
#mixed ANOVA
#aov_vis <- aov(value ~ AP.0.RP.1.AP.*LG*congruency+Error(code/(LG*congruency)), data=allresults_longvis)
pairwise.t.test(allresults_longvis$value,allresults_longvis$LG)
summary(aov_vis)
boxplot(value ~ LG*congruency* AP.0.RP.1.AP., data=allresults_longvis, main= "Navon")
#plot error bars
means_vis<-tapply(X=allresults_longvis$value,
                  INDEX=list(allresults_longvis$congruency,allresults_longvis$LG,
                             allresults_longvis$AP.0.RP.1.AP.),FUN=mean)
sd_vis<-tapply(X=allresults_longvis$value,
               INDEX=list(allresults_longvis$congruency,allresults_longvis$LG,
                          allresults_longvis$AP.0.RP.1.AP.),FUN=sd)
length_xvis<-tapply(X=allresults_longvis$value,
                    INDEX=list(allresults_longvis$congruency,allresults_longvis$LG,
                               allresults_longvis$AP.0.RP.1.AP.),FUN=length)
se_vis<-sd_vis/sqrt(length_xvis)
descr_vis<-cbind(group=c(1,2,3,4,5,6,7,8),means_vis,se_vis)
#descr_vis<-cbind(descr_vis,congruency=c("con","inc","con","inc","con","inc","con","inc"),pitch=c("RP","RP","RP","RP","AP","AP","AP","AP"),focus=c("G","G","L","L","G","G","L","L"),stringsAsFactors=FALSE)
rownames(descr_vis)<-c("G_con_0","G_inc_0","L_con_0","L_inc_0",
                       "G_con_1","G_inc_1","L_con_1","L_inc_1")
descr_vis<-data.frame(descr_vis)
j<-ggplot(descr_vis,aes(x=group,y=means_vis,ymin=means_vis-1.96*se_vis,ymax=means_vis+1.96*se_vis))
j+geom_point(size=3)+geom_errorbar(color=c("darkblue","darkblue","blue","blue","darkgreen","darkgreen","#00CC33","#00CC33"),size=0.7, width=0.5)+theme_classic()+ labs(x="Condition",
                                                                                                                                                                       y= "Reaction-times", 
                                                                                                                                                                       title="Navon",subtitle="local (L) vs. global (G), congruent (con) vs. incongruent (inc), by group (RP=blue vs. AP=green)")+
  coord_cartesian(ylim=c(0.35,0.65))+
  xlim(labels=c("G_con_RP","G_inc_RP","L_con_RP","L_inc_RP",
                "G_con_AP","G_inc_AP","L_con_AP","L_inc_AP"))+
  theme(axis.text.x=element_text(color="black", size=10),
        axis.text.y=element_text(color="black", size=10),
        axis.title=element_text(face="bold",size=12),
        title=element_text(size=13),panel.grid.major.y = element_line(colour = "grey"))+
  annotate(geom="text",x=7.5,y=-1.8, label="Means & Confidence Intervals")
##lm explaining local results!
