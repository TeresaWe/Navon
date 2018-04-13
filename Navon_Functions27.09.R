
library("dplyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.3")
###Functions###
#Mean of congruent Trials, local Block, Navon (not practice)
z=0
count=0
Navon_L_con<-function(Navontable){
  vec<-numeric()
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
  for (i in 5:80) { #skip practice trials
    if (congruent[i]==1 & KeyL.corr[i]==1){ #only congruent and correct trials
      vec[i]<-KeyL.rt[i]
      count=count+1
    }
    else{
      vec<-vec
      count=count+0
    }
    i=i+1
    assign("vis_L_con_RT", z, envir=globalenv())
    assign("vis_L_con_corr", count, envir=globalenv())
    assign("vis_L_con_meanRT", mean(vec,na.rm=TRUE), envir=globalenv())
  }
  return (c(vis_L_con_corr,vis_L_con_meanRT))
}


#Mean of incongruent Trials, local Block, Navon (not practice)
z=0
count=0
Navon_L_incon<-function(Navontable){
  z=0
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
  for (i in 5:80) { #skip practice trials
    if (congruent[i]==0 & KeyL.corr[i]==1){ #only congruent and correct trials
      z=z+KeyL.rt[i]
      count=count+1
    }
    else{
      z=z+0
      count=count+0
    }
    i=i+1
    assign("vis_L_incon_RT", z, envir=globalenv())
    assign("vis_L_incon_corr", count, envir=globalenv())
    assign("vis_L_incon_meanRT", vis_L_incon_RT/vis_L_incon_corr, envir=globalenv())
  }
  return (c(vis_L_incon_corr,vis_L_incon_meanRT))
}

#Mean of congruent Trials, global Block, Navon (not practice)

z=0
count=0
Navon_G_con<-function(Navontable){
  z=0
  count=0
  Navontable_G<-dplyr::filter(Navontable,global_block.thisTrialN==0)
  congruent<-as.numeric(as.character(Navontable_G$congruent))
  KeyG.rt<-as.numeric(as.character(Navontable_G$KeyG.rt))
  KeyG.corr<-as.numeric(as.character(Navontable_G$KeyG.corr))
  for (i in 5:80) { #skip practice trials
    if (congruent[i]==1 & KeyG.corr[i]==1){ #only congruent and correct trials
      z=z+KeyG.rt[i]
      count=count+1
    }
    else{
      z=z+0
      count=count+0
    }
    i=i+1
    assign("vis_G_con_RT", z, envir=globalenv())
    assign("vis_G_con_corr", count, envir=globalenv())
    assign("vis_G_con_meanRT", vis_G_con_RT/vis_L_con_corr, envir=globalenv())
  }
  return (c(vis_G_con_corr,vis_G_con_meanRT))
}



#Mean of incongruent Trials, global Block, Navon (not practice)

z=0
count=0
Navon_G_incon<-function(Navontable){
  z=0
  count=0
  Navontable_G<-dplyr::filter(Navontable,global_block.thisTrialN==0)
  congruent<-as.numeric(as.character(Navontable_G$congruent))
  KeyG.rt<-as.numeric(as.character(Navontable_G$KeyG.rt))
  KeyG.corr<-as.numeric(as.character(Navontable_G$KeyG.corr))
  for (i in 5:80) { #skip practice trials
    if (congruent[i]==0 & KeyG.corr[i]==1){ #only congruent and correct trials
      z=z+KeyG.rt[i]
      count=count+1
    }
    else{
      z=z+0
      count=count+0
    }
    i=i+1
    assign("vis_G_incon_RT", z, envir=globalenv())
    assign("vis_G_incon_corr", count, envir=globalenv())
    assign("vis_G_incon_meanRT", vis_G_incon_RT/vis_L_incon_corr, envir=globalenv())
  }
  return (c(vis_G_incon_corr,vis_G_incon_meanRT))
}