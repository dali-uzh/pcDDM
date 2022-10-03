rm(list=ls())
library(gtools)
setwd("C:/Users/DALI/Desktop/pcDDM")

# change the folder where to take the data with all fixations 

#input_folder <- "dataSets/Stewart_etal_2016/" 
#input_folder <- "dataSets/Gaia_Hare_2021/"
input_folder <- "dataSets/Smith_Krajbich_2018/"
mydata <-  'krajbich_risk'




data <- read.csv(paste(input_folder, mydata, "_data.csv", sep=""))

recovery_folder <- "do_recovery"
output_folder <- file.path(recovery_folder,"sim_data")

#=============================================================================
# for each subject create a data matrix with the fixation location and duration

for (s in unique(data$subject)){ # s is real subject name

  dat<-data[data$subject==s,]
  
  #dat$RT<-dat$rt*1000 # original code. rt is in sec, RT in ms
  
  lower_bound<-0 #lower RT bound in ms
  upper_bound<-500000 #upper RT bound in ms 500s
  
  ####throw out "fast guesses" and really long outliers
  dat<-dat[dat$RT>lower_bound & dat$RT<upper_bound,]
  dmaxfix<-max(dat$fix_total) #maximum number of fixations (use the dat to specify)
  
  ####fixate A = 2 and fixate B = 1
  dat$roi <- 1+dat$roi_A
  dat$choice <- dat$choice_A
  
  ####how many trials total?
  temp<-dat[dat$fix_idx==1,]
  dtrials<-nrow(temp) 
  rm(temp)
  
  ####create a dataset with only middle fixations (we use this later for drawing random middle fixations)
  temp<-dat[(dat$fix_idx>1 & dat$fix_total>1),]

  ####prepare dat matrix
  data_mat<-matrix(0,nrow=dtrials,ncol=(3+dmaxfix*2)) 
  # columns of data_mat:
  # choice, rt, condition, maxFixLength pairs (roi, fixDur)  
  n=0
  for (i in 1:nrow(dat)){ # i & n == each trial
    if (dat$fix_id[i]==1){
      n<-n+1
      data_mat[n,1]<-dat$choice[i]
      data_mat[n,2]<-dat$RT[i]
      #data_mat[n,3]<-dat$condition[i]
      data_mat[n,3]<-1 # not used in aDDM_simulation
      
      data_mat[n,4]<-dat$roi[i] # 1st fixation's roi
      data_mat[n,(4+dmaxfix)]<-dat$fixDur[i] # 1st fixation's fixation time
    }else if (dat$fix_id[i]>1){
      data_mat[n,(3+dat$fix_id[i])]<-dat$roi[i] # later fixations' roi
      data_mat[n,(3+dmaxfix+dat$fix_id[i])]<-dat$fixDur[i] # later fixations' fixation time
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
  ####---------------------------------------------------
  # Q1 sample RT from entire subject pool???? 
  ####---------------------------------------------------
  pfixmat<-sample(temp$fixDur,dmaxfix*nrow(data_mat),replace=TRUE) 
  dim(pfixmat)<-c(nrow(data_mat),dmaxfix)
  #matrix with 1s for missing fixations and 0s for non-missing fixations
  fix_mat<-ceiling((1-data_mat[1:nrow(data_mat),(4+dmaxfix):ncol(data_mat)])/100000000)
  data_mat[1:nrow(data_mat),(4+dmaxfix):ncol(data_mat)]<-data_mat[1:nrow(data_mat),(4+dmaxfix):ncol(data_mat)]+fix_mat*pfixmat

  #remove everything but data_mat
  rm(dat,dmaxfix,pfixmat,temp)
  sub <- toString(s)
  
  # change the folder where to save the data matrix
  save(data_mat,file = paste(c(output_folder, "/sim_fix/", mydata ,"_sub_", toString(sub),".RData"), collapse = "") )

}
