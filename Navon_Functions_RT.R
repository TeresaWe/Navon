library(dplyr)
###Functions###
#Mean of congruent Trials, local Block, Navon (not practice)
z=0
count=0
Navon_Lcon_RT<-function(Navontable){
  z=numeric(0)
  count=0
  if (length(Navontable)==60){
    Navontable_L<-dplyr::filter(Navontable,local_Block.thisTrialN==0)
  }
  else{
    Navontable_L<-dplyr::filter(Navontable,local_block.thisTrialN==0)
  }
  congruent<-as.numeric(as.character(Navontable_L$congruent))
  KeyL.rt<-as.numeric(as.character(Navontable_L$KeyL.rt))
  KeyL.corr<-as.numeric(as.character(Navontable_L$KeyL.corr))
  for (i in 5:84) { #skip practice trials
    if (congruent[i]==1 & KeyL.corr[i]==1){ #only congruent and correct trials
      z[i-4]<-KeyL.rt[i]
      count=count+1
    }
    else{
      #z<-z
      count=count+0
      z[i-4]<-NA
    }
    i=i+1
    assign("vis_L_con_RT", z, envir=globalenv())
    assign("vis_L_con_corr", count, envir=globalenv())
    assign("vis_L_con_meanRT", mean(z,na.rm=TRUE), envir=globalenv())
  }
  return (vis_L_con_RT)
}


#Mean of incongruent Trials, local Block, Navon (not practice)
z=0
count=0
Navon_Linc_RT<-function(Navontable){
  z=numeric(0)
  count=0
  if (length(Navontable)==60){
    Navontable_L<-dplyr::filter(Navontable,local_Block.thisTrialN==0)
  }
  else{
    Navontable_L<-dplyr::filter(Navontable,local_block.thisTrialN==0)
  }
  congruent<-as.numeric(as.character(Navontable_L$congruent))
  KeyL.rt<-as.numeric(as.character(Navontable_L$KeyL.rt))
  KeyL.corr<-as.numeric(as.character(Navontable_L$KeyL.corr))
  for (i in 5:84) { #skip practice trials
    if (congruent[i]==0 & KeyL.corr[i]==1){ #only congruent and correct trials
      z[i-4]<-KeyL.rt[i]
      count=count+1
    }
    else{
      #z=z+0
      count=count+0
      z[i-4]<-NA
    }
    i=i+1
    assign("vis_L_incon_RT", z, envir=globalenv())
    assign("vis_L_incon_corr", count, envir=globalenv())
    assign("vis_L_incon_meanRT", vis_L_incon_RT/vis_L_incon_corr, envir=globalenv())
  }
  return (c(vis_L_incon_RT))
}

#Mean of congruent Trials, global Block, Navon (not practice)

z=0
count=0
Navon_Gcon_RT<-function(Navontable){
  z=numeric(0)
  count=0
  Navontable_G<-dplyr::filter(Navontable,global_block.thisTrialN==0)
  congruent<-as.numeric(as.character(Navontable_G$congruent))
  KeyG.rt<-as.numeric(as.character(Navontable_G$KeyG.rt))
  KeyG.corr<-as.numeric(as.character(Navontable_G$KeyG.corr))
  for (i in 5:84) { #skip practice trials
    if (congruent[i]==1 & KeyG.corr[i]==1){ #only congruent and correct trials
      z[i-4]<-KeyG.rt[i]
      count=count+1
    }
    else{
      #z=z+0
      count=count+0
      z[i-4]<-NA
    }
    i=i+1
    assign("vis_G_con_RT", z, envir=globalenv())
    assign("vis_G_con_corr", count, envir=globalenv())
    assign("vis_G_con_meanRT", vis_G_con_RT/vis_L_con_corr, envir=globalenv())
  }
  return (c(vis_G_con_RT))
}



#Mean of incongruent Trials, global Block, Navon (not practice)

z=0
count=0
Navon_Ginc_RT<-function(Navontable){
  z=numeric(0)
  count=0
  Navontable_G<-dplyr::filter(Navontable,global_block.thisTrialN==0)
  congruent<-as.numeric(as.character(Navontable_G$congruent))
  KeyG.rt<-as.numeric(as.character(Navontable_G$KeyG.rt))
  KeyG.corr<-as.numeric(as.character(Navontable_G$KeyG.corr))
  for (i in 5:84) { #skip practice trials
    if (congruent[i]==0 & KeyG.corr[i]==1){ #only congruent and correct trials
      z[i-4]<-KeyG.rt[i]
      count=count+1
    }
    else{
      #z=z+0
      count=count+0
      z[i-4]<-NA
    }
    i=i+1
    assign("vis_G_incon_RT", z, envir=globalenv())
    assign("vis_G_incon_corr", count, envir=globalenv())
    assign("vis_G_incon_meanRT", vis_G_incon_RT/vis_L_incon_corr, envir=globalenv())
  }
  return (c(vis_G_incon_RT))
}