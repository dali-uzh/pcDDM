rm(list=ls())
library(gtools)

# change the folder where to take the data with all fixations 
load("/Users/dbao/google_drive_sherry/multiattribute_discounting/data/Data/data_fix.RData") 

#=============================================================================
# for each subject create a data matrix with the fixation location and duration

for (s in unique(data$subnum)){

  dat<-data[data$subnum==s,]
  
  dat$RT<-dat$rt*1000
  
  lower_bound<-0 #lower RT bound in ms
  upper_bound<-500000 #upper RT bound in ms 500s
  
  ####throw out "fast guesses" and really long outliers
  dat<-dat[dat$rt>lower_bound & dat$rt<upper_bound,]
  dmaxfix<-max(dat$NumFixations) #maximum number of fixations (use the dat to specify)
  
  ####fixate left = 2 and fixate right = 1
  dat$roi <- 1+dat$FixateLeft
  dat$choice <- dat$leftchosen
  
  ####how many trials total?
  temp<-dat[dat$FixationNum==1,]
  dtrials<-nrow(temp) 
  rm(temp)
  
  ####create a dataset with only middle fixations (we use this later for drawing random middle fixations)
  temp<-dat[(dat$FixationNum>1 & dat$NumFixations>1),]
  
  ####prepare dat matrix
  data_mat<-matrix(0,nrow=dtrials,ncol=(3+dmaxfix*2)) 
  #choice,rt,condition,roi,fixation lengths,fix duration
  n=0
  for (i in 1:nrow(dat)){
    if (dat$FixationNum[i]==1){
      n<-n+1
      data_mat[n,1]<-dat$choice[i]
      data_mat[n,2]<-dat$RT[i]
      data_mat[n,3]<-dat$condition[i]
      
      data_mat[n,4]<-dat$roi[i] # 1st fixation's roi
      data_mat[n,(4+dmaxfix)]<-dat$fixationTime[i] # 1st fixation's fixation time
    }else if (dat$FixationNum[i]>1){
      data_mat[n,(3+dat$FixationNum[i])]<-dat$roi[i] # later fixations' roi
      data_mat[n,(3+dmaxfix+dat$FixationNum[i])]<-dat$fixationTime[i] # later fixations' fixation time
    }
  }
   
  ####fill in missing fixation locations
  prois<-c(1,2) #possible rois
  for (i in 1:nrow(data_mat)){
    for (j in 4:(3+dmaxfix) ) {
      if (data_mat[i,j]==0) {
        data_mat[i,j]= prois[prois!=data_mat[i,j-1]] #sample(prois[prois!=data_mat[i,j-1]],1)
      }
    }
  }
    
  ####fill in missing fixation durations
  pfixmat<-sample(temp$fixationTime,dmaxfix*nrow(data_mat),replace=TRUE)
  dim(pfixmat)<-c(nrow(data_mat),dmaxfix)
  #matrix with 1s for missing fixations and 0s for non-missing fixations
  fix_mat<-ceiling((1-data_mat[1:nrow(data_mat),(4+dmaxfix):ncol(data_mat)])/100000000)
  data_mat[1:nrow(data_mat),(4+dmaxfix):ncol(data_mat)]<-data_mat[1:nrow(data_mat),(4+dmaxfix):ncol(data_mat)]+fix_mat*pfixmat

  #remove everything but data_mat
  rm(dat,dmaxfix,pfixmat,temp)
  sub <- toString(s)
  
  # change the folder where to save the data matrix
  save(data_mat,file = paste(c("/Users/dbao/google_drive_sherry/multiattribute_discounting/data/Modeling/recovery/aDDM_2w1theta/simulationInput/data_preppedAll_sub_", toString(sub),".RData"), collapse = "") )

}
