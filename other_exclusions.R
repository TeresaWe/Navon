####################################################################################
##################################################################################
#exclusions
allresults_Navon<-subset(allresults_Navon, VP_Code!="OS12GRO173") #change back to all if exclusions eeg not needed,#or change to other EEG_results table (eeg. delta)
allresults_Navon<-subset(allresults_Navon, VP_Code!="EL01KES159")
allresults_Navon<-subset(allresults_Navon, VP_Code!="YL24DRO161")
allresults_Navon<-subset(allresults_Navon, VP_Code!="RI15SON966")
allresults_Navon<-subset(allresults_Navon, VP_Code!="AZ17FRA106")
##excluded because of RT distributions
allresults_Navon<-subset(allresults_Navon, VP_Code!="EI13WOH163")
allresults_Navon<-subset(allresults_Navon, VP_Code!="US24GRU177")
allresults_Navon<-subset(allresults_Navon, VP_Code!="AR07AND453")
allresults_Navon<-subset(allresults_Navon, VP_Code!="IS17THE058")
allresults_Navon<-subset(allresults_Navon, VP_Code!="LA06STR103")

histBy(allresults_Navon,"medianG_con","AP.0.RP.1.AP.")
histBy(allresults_Navon,"medianL_con","AP.0.RP.1.AP.")
histBy(allresults_Navon,"medianL_inc","AP.0.RP.1.AP.")
histBy(allresults_Navon,"medianL_con","AP.0.RP.1.AP.")
allresults_Navonlong <- gather(allresults_Navon, condition, median, medianL_con,medianL_inc,medianG_con,medianG_inc)
LG<-character(220)
LG[1:110]<-"L"
LG[111:220]<- "G"
congruency<-character(220)
congruency[1:55]<-"inc"
congruency[111:165]<- "inc"
congruency[56:110]<-"con"
congruency[166:220]<-"con"
allresults_longvis <- data.frame(allresults_Navonlong,LG,congruency)
aov_vis <- aov(median ~ AP.0.RP.1.AP.*LG*congruency+Error(code/(LG*congruency)), data=allresults_longvis)
boxplot(median ~ LG*congruency* AP.0.RP.1.AP., data=allresults_longvis, main= "Navon")
############################################################################################################################

#exclusions
allresults_Navon<-subset(allresults_Navon, VP_Code!="OS12GRO173") #change back to all if exclusions eeg not needed,#or change to other EEG_results table (eeg. delta)
allresults_Navon<-subset(allresults_Navon, VP_Code!="EL01KES159")
allresults_Navon<-subset(allresults_Navon, VP_Code!="YL24DRO161")
allresults_Navon<-subset(allresults_Navon, VP_Code!="RI15SON966")
allresults_Navon<-subset(allresults_Navon, VP_Code!="AZ17FRA106")
##excluded because of RT distributions
allresults_Navon<-subset(allresults_Navon, VP_Code!="EI13WOH163")
allresults_Navon<-subset(allresults_Navon, VP_Code!="US24GRU177")
allresults_Navon<-subset(allresults_Navon, VP_Code!="AR07AND453")
allresults_Navon<-subset(allresults_Navon, VP_Code!="IS17THE058")
allresults_Navon<-subset(allresults_Navon, VP_Code!="LA06STR103")
allresults_Navon<-subset(allresults_Navon, VP_Code!="AR23LUT171")
allresults_Navon<-subset(allresults_Navon, VP_Code!="AT26IMM177")
allresults_Navon<-subset(allresults_Navon, VP_Code!="ON27GRO171")

histBy(allresults_Navon,"medianG_con","AP.0.RP.1.AP.")
histBy(allresults_Navon,"medianL_con","AP.0.RP.1.AP.")
histBy(allresults_Navon,"medianL_inc","AP.0.RP.1.AP.")
histBy(allresults_Navon,"medianL_con","AP.0.RP.1.AP.")
allresults_Navonlong <- gather(allresults_Navon, condition, median, medianL_con,medianL_inc,medianG_con,medianG_inc)
LG<-character(212)
LG[1:106]<-"L"
LG[107:212]<- "G"
congruency<-character(212)
congruency[1:53]<-"inc"
congruency[107:159]<- "inc"
congruency[54:106]<-"con"
congruency[160:212]<-"con"
allresults_longvis <- data.frame(allresults_Navonlong,LG,congruency)
aov_vis <- aov(median ~ AP.0.RP.1.AP.*LG*congruency+Error(code/(LG*congruency)), data=allresults_longvis)
boxplot(median ~ LG*congruency* AP.0.RP.1.AP., data=allresults_longvis, main= "Navon")

#########################################################################################################################

