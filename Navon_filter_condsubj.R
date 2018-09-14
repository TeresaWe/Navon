### filter RT times within conditions
#only RT values but only for 64 participants (without additional variables)
NavonRTreduced<-resultsindex
NavonRTreduced[NavonRTreduced==0]<-NA #remove all unusual RT (=zero)
NavonRTreduced[NavonRTreduced>1]<-NA
##Lcon
copy<-NavonRTreduced
Lconcorr<-numeric(0)
for (i in 1:64){
  abw=0
  count=0
  median<-median(NavonRTreduced[i,1:80],na.rm=TRUE)
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
  MAD=abw/count #mittlere abweichung vom Median
  print(MAD)
  Lconcorr[i]<-count
  values<-NavonRTreduced[i,1:80]
  values[values>(median+2*MAD)]<-NA
  values[values<(median-2*MAD)]<-NA
  NavonRTreduced[i,1:80]<-values
  i=i+1
}
NavonRTreduced_t<-t(NavonRTreduced)
NavonRTreduced_t<-data.frame(NavonRTreduced_t[1:80,])
ggplot(gather(NavonRTreduced_t), aes(value)) + 
  geom_histogram(binwidth=0.01)+facet_wrap(~key, scales = 'free_x')



##Linc
Linccorr<-numeric(0)
for (i in 1:64){
  abw=0
  count=0
  median<-median(NavonRTreduced[i,81:160],na.rm=TRUE)
  print(median)
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
  MAD=abw/count #mittlere abweichung vom Median
  print(MAD)
  Linccorr[i]<-count
  values<-NavonRTreduced[i,81:160]
  values[values>(median+2*MAD)]<-NA
  values[values<(median-2*MAD)]<-NA
  NavonRTreduced[i,81:160]<-values
  i=i+1
}
NavonRTreduced_t<-t(NavonRTreduced)
NavonRTreduced_t<-data.frame(NavonRTreduced_t[81:160,])
ggplot(gather(NavonRTreduced_t), aes(value)) + 
  geom_histogram(binwidth=0.01)+facet_wrap(~key, scales = 'free_x')

##Gcon
Gconcorr<-numeric(0)
for (i in 1:64){
  abw=0
  count=0
  median<-median(NavonRTreduced[i,161:240],na.rm=TRUE)
  print(median)
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
  MAD=abw/count #mittlere abweichung vom Median
  print(MAD)
  Gconcorr[i]<-count
  values<-NavonRTreduced[i,161:240]
  values[values>(median+2*MAD)]<-NA
  values[values<(median-2*MAD)]<-NA
  NavonRTreduced[i,161:240]<-values
  i=i+1
}
NavonRTreduced_t<-t(NavonRTreduced)
NavonRTreduced_t<-data.frame(NavonRTreduced_t[161:240,])
ggplot(gather(NavonRTreduced_t), aes(value)) + 
  geom_histogram(binwidth=0.01)+facet_wrap(~key, scales = 'free_x')


##Ginc
Ginccorr<-numeric(0)
for (i in 1:64){
  abw=0
  count=0
  median<-median(NavonRTreduced[i,241:320],na.rm=TRUE)
  print(median)
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
  MAD=abw/count #mittlere abweichung vom Median
  print(MAD)
  Ginccorr[i]<-count
  values<-NavonRTreduced[i,241:320]
  values[values>(median+2*MAD)]<-NA
  values[values<(median-2*MAD)]<-NA
  NavonRTreduced[i,241:320]<-values
  i=i+1
}
NavonRTreduced_t<-t(NavonRTreduced)
NavonRTreduced_t<-data.frame(NavonRTreduced_t[241:320,])
ggplot(gather(NavonRTreduced_t), aes(value)) + 
  geom_histogram(binwidth=0.01)+facet_wrap(~key, scales = 'free_x')


######################################
#######################################
######################################
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
Navonmedians2<-t(Navonmedians)

allresults_Navon2<-cbind(allresults, Navonmedians2,Lconcorr,Linccorr,Gconcorr,Ginccorr)  
