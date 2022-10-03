rm(list=ls())
setwd("C:/Users/DALI/Desktop/pcDDM")

# ===================================================
# folders
input_folder <- "result_fitting"
mydata <-  'krajbich_risk'
typename <- "optionTheta_riskAve1"
filename <- paste(typename, "_", mydata,sep="")
mypara <- paste("results_HaDDM_FIT_",filename,".RData", sep="")

recovery_folder <- "do_recovery"
output_folder <- file.path(recovery_folder,"sim_data")
#output_folder <- file.path(recovery_folder,"result_recovery")

# ==========================================================================
start_time <- Sys.time()

# Install required packages if necessary:
want = c("DEoptim", "Rcpp", "plyr", "parallel", "RcppParallel","stats4","pracma")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load them all
suppressPackageStartupMessages(lapply(want, require, character.only = TRUE))

RcppParallel::setThreadOptions(numThreads = 1) #this is critical for mclapply
#load the c++ file containing the functions to simulate the delayed DDM
# will give some errors that can be ignored
sourceCpp("do_recovery/aDDM_Rcpp_riskAve1_2money.cpp")
# ==========================================================================
### Simulations ###
load( file.path(input_folder, mypara))
chain = rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]])

#only options and subject id from data_trial
sim_data<- data.frame(val_A1 = data_trial$val_A1, val_B1 = data_trial$val_B1, 
                      val_A2 = data_trial$val_A2, val_B2 = data_trial$val_B2, 
                      subj_idx = as.numeric(ordered(data_trial$subject)))

for (s in unique(sim_data$subj_idx)){  # s is subject index (from 1~N)
  # get parameters from fitting results
  #b.p, rho.p, bias, alpha.p, theta.p, thetaGaze_A, thetaGaze_B, 
  sim_data$drift[sim_data$subj_idx==s] = mean(chain[,c( paste( c("b.p[",toString(s),"]"), collapse = ""))])
  sim_data$noise[sim_data$subj_idx==s]  = mean(chain[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
  sim_data$nDT[sim_data$subj_idx==s]  = mean(chain[,c( paste( c("theta.p[",toString(s),"]"), collapse = ""))])
  sim_data$bias[sim_data$subj_idx==s]  = mean(chain[,c( paste( c("bias[",toString(s),"]"), collapse = ""))]) # should I use the original bias? yes if boundary changed to 1
  sim_data$thetaGaze_A[sim_data$subj_idx==s] = mean(chain[,c( paste( c("thetaGaze_A[",toString(s),"]"), collapse = ""))])
  sim_data$thetaGaze_B[sim_data$subj_idx==s] = mean(chain[,c( paste( c("thetaGaze_B[",toString(s),"]"), collapse = ""))])
  
  # get single value (same for each trial)
  drift    <- sim_data$drift[sim_data$subj_idx==s] 
  noise <- sim_data$noise[sim_data$subj_idx==s]
  nDT   <- sim_data$nDT[sim_data$subj_idx==s]
  bias  <- sim_data$bias[sim_data$subj_idx==s]
  thetaGaze_A <- sim_data$thetaGaze_A[sim_data$subj_idx==s]
  thetaGaze_B <- sim_data$thetaGaze_B[sim_data$subj_idx==s]
  
  # get options
  val_A1 = sim_data$val_A1[sim_data$subj_idx==s]
  val_B1 = sim_data$val_B1[sim_data$subj_idx==s]
  val_A2 = sim_data$val_A2[sim_data$subj_idx==s]
  val_B2 = sim_data$val_B2[sim_data$subj_idx==s]


  # fake fixations, filename = data_mat 
  load(file = paste(c(output_folder, "/sim_fix/", mydata ,"_sub_", toString(unique(data_trial$subject)[s]),".RData"), collapse = "") )    
  
  for (i in 1:length(val_A1)) { # for each trial of this subject
    maxfix<- (ncol(data_mat)-3)/2 
    fixL = data_mat[i, 4: (3+maxfix)] - 1 # fixation location: Left(1) or Right(0)
    fixDur = data_mat[i, (4+maxfix):(3+2*maxfix)] # fixation duration
    
    # generate new RTs, here the rt is RT_NP in second
    ######################## here using thres =2
    rt <- ddm2_parallel(drift[i], 2, nDT[i], thetaGaze_A[i], thetaGaze_B[i], bias[i], 
                        val_A1[i], val_B1[i], val_A2[i], val_B2[i], 
                        fixL, fixDur, noise[i], 1)
    sim_data$RT_PN[sim_data$subj_idx==s & sim_data$val_A1==val_A1[i] & sim_data$val_B1==val_B1[i] & sim_data$val_A2==val_A2[i] & sim_data$val_B2==val_B2[i]] = rt

    ## get simulated fixation duration and proportion
    ToTfix_time = (abs(rt)-nDT[i])*1000 #rt and nDT here are in seconds 
    
    fixDurSum = cumsum(fixDur)  
    # cumsum(1:10)=  1  3  6 10 15 21 28 36 45 55 2V?n$@%,
    
    
    fixStop = which(fixDurSum < ToTfix_time, arr.ind = TRUE) # array
    lastfixNum = fixStop[length(fixStop)]+1
    
    fixDur_real = c(fixDur[fixStop], (fixDur[lastfixNum]-(fixDurSum[lastfixNum] -ToTfix_time)))
    fixL_real = c(fixL[fixStop], fixL[lastfixNum])
    sim_data$fixDur_A_sum[sim_data$subj_idx==s & sim_data$val_A1==val_A1[i] & sim_data$val_B1==val_B1[i] & sim_data$val_A2==val_A2[i] & sim_data$val_B2==val_B2[i]] = sum(fixDur_real[fixL_real==1],na.rm=TRUE)
    sim_data$fixDur_sum[sim_data$subj_idx==s & sim_data$val_A1==val_A1[i] & sim_data$val_B1==val_B1[i] & sim_data$val_A2==val_A2[i] & sim_data$val_B2==val_B2[i]] = ToTfix_time
  }
}

# check the range of RT range(data$rt)
sim_data$RT_PN[sim_data$RT_PN > 50 | sim_data$RT_PN < -50] <- NaN # 10
#sim_data$rt[sim_data$rt < 0.2 & sim_data$rt > -0.2]<-NaN
sim_data  <- sim_data[!(is.nan(sim_data$RT_PN)),]



write.csv(sim_data, file=paste(c(output_folder, "/sim_",filename,".csv"), collapse = ""))
end_time <- Sys.time()
end_time - start_time

