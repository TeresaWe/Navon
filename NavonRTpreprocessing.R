#Workspace NavonRT2607
#new scripts to save raw RT values per subject and trial for all trials in Navon (incorrect answers --> NA)
NavonRT
#matched with allresults to obtain only 64 participants, plus additional variables
allresults<- read_csv("~/files/allresults12.06.csv")
index<-match(x=allresults$VP_Code, table=rownames(NavonRT))
resultsindex<-rbind(NavonRT[index,])
#cbind tables together
NavonRT_allresults<-cbind(allresults,resultsindex) 
#only RT values but only for 64 participants (without additional variables)
NavonRTreduced<-resultsindex
NavonRTreduced[NavonRTreduced==0]<-NA #remove all unusual RT (=zero)
NavonRTreduced[NavonRTreduced>1]<-NA

hist(NavonRTreduced)
hist(NavonRTreduced[,1:80])
hist(NavonRTreduced[,81:160])
hist(NavonRTreduced[,161:240])
hist(NavonRTreduced[,241:320])
shapiro.test(NavonRTreduced)
for (i in 1:64){
  print(shapiro.test(NavonRTreduced[i,]))
  i=i+1
} #Normalverteilung für alle Probanden abgelehnt (except one)
NavonRTreduced_t<-t(NavonRTreduced)
NavonRTreduced_t<-data.frame(NavonRTreduced_t)
ggplot(gather(NavonRTreduced_t), aes(value)) + 
  geom_histogram(binwidth=0.01)+facet_wrap(~key, scales = 'free_x')

#exclusions
allresults_Navon<-subset(allresults, VP_Code!="OS12GRO173") #change back to all if exclusions eeg not needed,#or change to other EEG_results table (eeg. delta)
allresults_Navon<-subset(allresults_Navon, VP_Code!="EL01KES159")
allresults_Navon<-subset(allresults_Navon, VP_Code!="YL24DRO161")
allresults_Navon<-subset(allresults_Navon, VP_Code!="RI15SON966")
allresults_Navon<-subset(allresults_Navon, VP_Code!="AZ17FRA106")
#filter values according to subjects distribution (median and deviation from median), across all conditions
copy<-NavonRTreduced
for (i in 1:64){
  abw=0
  count=0
  median<-median(NavonRTreduced[i,],na.rm=TRUE)
  print(median)
  for (j in 1:320){
    if (is.na(NavonRTreduced[i,j])==TRUE){
      j=j+1
      abw=abw
    }
    else {
    diff<-abs(NavonRTreduced[i,j]-median)
    abw=abw+diff
    #print(diff)
    j=j+1
    count=count+1
    }
  }
  MAD=abw/count #mittlere abweichung vom Median
  print(MAD)
  values<-NavonRTreduced[i,]
  values[values>(median+2*MAD)]<-NA
  values[values<(median-2*MAD)]<-NA
  NavonRTreduced[i,]<-values
  i=i+1
}
NavonRTreduced_t<-t(NavonRTreduced)
NavonRTreduced_t<-data.frame(NavonRTreduced_t)
ggplot(gather(NavonRTreduced_t), aes(value)) + 
  geom_histogram(binwidth=0.01)+facet_wrap(~key, scales = 'free_x')

#
Navonmedians<-numeric(4*64)
for (i in 1:64){
  medianL_con<-median(NavonRTreduced[i,1:80],na.rm=TRUE)
  medianL_inc<-median(NavonRTreduced[i,81:160],na.rm=TRUE)
  medianG_con<-median(NavonRTreduced[i,161:240],na.rm=TRUE)
  medianG_inc<-median(NavonRTreduced[i,241:320],na.rm=TRUE)
  Navonmedians[((4*i)-3):(4*i)]<- c(medianL_con,medianL_inc,medianG_con,medianG_inc)
  i=i+1
}
dim(Navonmedians)<-c(4,64)
colnames(Navonmedians)<-rownames(NavonRTreduced)
rownames(Navonmedians)<-c("medianL_con","medianL_inc","medianG_con","medianG_inc")
Navonmedians<-t(Navonmedians)


plot(allresults_Navon$medianG_con,allresults_Navon$medianG_inc)
plot(allresults_Navon$medianL_con,allresults_Navon$medianL_inc)
plot(allresults_Navon$medianG_con,allresults_Navon$medianL_inc)
plot(allresults_Navon$medianL_con,allresults_Navon$medianG_inc)
plot(allresults_Navon$medianL_inc,allresults_Navon$medianG_inc)
plot(allresults_Navon$medianL_con,allresults_Navon$medianG_con)
plot(NavonRTreduced[1,],NavonRTreduced[2,])

###calculate counting trials after RT exclusions and deviations from median
vMAD_Lcon<-numeric(0)
vMAD_Linc<-numeric(0)
vMAD_Gcon<-numeric(0)
vMAD_Ginc<-numeric(0)
vcorrect_Lcon<-numeric(0)
vcorrect_Linc<-numeric(0)
vcorrect_Gcon<-numeric(0)
vcorrect_Ginc<-numeric(0)
for (i in 1:64){
  abw=0
  count=0
  median<-median(NavonRTreduced[i,],na.rm=TRUE)
  print(median)
  for (j in 1:80){
    if (is.na(NavonRTreduced[i,j])==TRUE){
      j=j+1
      abw=abw
    }
    else {
      diff<-abs(NavonRTreduced[i,j]-median)
      abw=abw+diff
      #print(diff)
      j=j+1
      count=count+1
    }
  }
  vMAD_Lcon[i]=abw/count #mittlere abweichung vom Median
  vcorrect_Lcon[i]<-count
  count=0
  abw=0
  for (j in 81:160){
    if (is.na(NavonRTreduced[i,j])==TRUE){
      j=j+1
      abw=abw
    }
    else {
      diff<-abs(NavonRTreduced[i,j]-median)
      abw=abw+diff
      #print(diff)
      j=j+1
      count=count+1
    }
  }
  vMAD_Linc[i]=abw/count #mittlere abweichung vom Median
  vcorrect_Linc[i]<-count
  count=0
  abw=0
  for (j in 161:240){
    if (is.na(NavonRTreduced[i,j])==TRUE){
      j=j+1
      abw=abw
    }
    else {
      diff<-abs(NavonRTreduced[i,j]-median)
      abw=abw+diff
      #print(diff)
      j=j+1
      count=count+1
    }
  }
  vMAD_Gcon[i]=abw/count #mittlere abweichung vom Median
  vcorrect_Gcon[i]<-count
  count=0
  abw=0
  for (j in 241:320){
    if (is.na(NavonRTreduced[i,j])==TRUE){
      j=j+1
      abw=abw
    }
    else {
      diff<-abs(NavonRTreduced[i,j]-median)
      abw=abw+diff
      #print(diff)
      j=j+1
      count=count+1
    }
  }
  vMAD_Ginc[i]=abw/count #mittlere abweichung vom Median
  vcorrect_Ginc[i]<-count
  i=i+1
}
Navon_medianresults<-data.frame(Navonmedians,vMAD_Lcon,vMAD_Linc,vMAD_Gcon,vMAD_Ginc,
                               vcorrect_Lcon,vcorrect_Linc,vcorrect_Gcon,vcorrect_Ginc)

allresults_Navon<-cbind(allresults, Navon_medianresults)  

