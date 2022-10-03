rm(list=ls())
start_time <- Sys.time()

# condition
Cond = 1
# ==========================================================================
# Install required packages if necessary:
want = c("DEoptim", "Rcpp", "plyr", "parallel", "RcppParallel","stats4","pracma")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# Now load them all
suppressPackageStartupMessages(lapply(want, require, character.only = TRUE))

# ==========================================================================
RcppParallel::setThreadOptions(numThreads = 1) #this is critical for mclapply
#load the c++ file containing the functions to simulate the delayed DDM
# will give some errors that can be ignored
sourceCpp("./Modeling/recovery/aDDM_2w1theta/aDDM_Rcpp.cpp")

# ==========================================================================
# load data 
para_folder  <- file.path("/Users/dbao/google_drive_sherry/multiattribute_discounting/data/Modeling/fit/aDDM_2w1theta")
mypara <-  paste(c("results_aDDM_2w1theta_", toString(Cond),".RData"), collapse = "")
load( file.path(para_folder, mypara))
chain = rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]])

# ==========================================================================
# scale value of health and taste difference
# ==========================================================================
# # # Data variables: # # #
#
# choice/leftchosen      -> =1 if left food item chosen, =0 if right food item chosen
# vt_l/tasteValueLeft    -> value taste food item on the left hand side of the screen
# vt_r/tasteValueRight   -> value taste food item on the right hand side of the screen
# vh_l/healthValueLeft   -> value health food item on the left hand side of the screen
# vh_r/healthValueRight  -> value health food item on the right hand side of the screen
# td      -> value difference in taste
# hd      -> value difference in health
# rt      -> reaction time in seconds
# fixnum/FixationNum  -> fixation number
# subject/subnum         -> subject number

# Data = Data[Data$rt>0.2,]
# ==========================================================================
# discard all the fixations, keep the first one
Data<-Data[Data$FixationNum==1,]
Data<-Data[Data$condition==Cond,]
# set condition
#idxCond = which(Data$condition==Cond)

# # RT is positive if left food item chosen, negative if right food item chosen
# idx = which(Data$leftchosen==0)
# Data$RT <- Data$rt
# Data$RT[idx] = Data$rt[idx] * -1

idxP = as.numeric(ordered(Data$subnum)) #makes a sequentially numbered subj index
ns = length(unique(idxP))

# scale value of health and taste 
Data$v_tL = scale(Data$tasteValueLeft)
Data$v_tR = scale(Data$tasteValueRight)
Data$v_hL = scale(Data$healthValueLeft)
Data$v_hR = scale(Data$healthValueRight)

# TO DO ..
DataFit <- Data
DDataSim<- data.frame(v_tL = DataFit$v_tL, v_tR = DataFit$v_tR, v_hL = DataFit$v_hL, v_hR = DataFit$v_hR, subject = as.numeric(ordered(DataFit$subnum)) )

# ==========================================================================
### Simulations ###

subj <- unique(as.numeric(ordered(DDataSim$subject)))

for (s in subj){  

  # TO DO...
  DDataSim$wh[DDataSim$subject==s] = mean(chain[,c( paste( c("b1.p[",toString(s),"]"), collapse = ""))])
  DDataSim$wt[DDataSim$subject==s] = mean(chain[,c( paste( c("b2.p[",toString(s),"]"), collapse = ""))])
  DDataSim$noise[DDataSim$subject==s]  = mean(chain[,c( paste( c("alpha.p[",toString(s),"]"), collapse = ""))])
  DDataSim$nDT[DDataSim$subject==s]  = mean(chain[,c( paste( c("theta.p[",toString(s),"]"), collapse = ""))])
  DDataSim$bias[DDataSim$subject==s]  = mean(chain[,c( paste( c("bias[",toString(s),"]"), collapse = ""))]) # should I use the original bias? yes if boundary changed to 1
  DDataSim$theta[DDataSim$subject==s] = mean(chain[,c( paste( c("thetaGaze[",toString(s),"]"), collapse = ""))])
  
  v_tL = DDataSim$v_tL[DDataSim$subject==s]
  v_tR = DDataSim$v_tR[DDataSim$subject==s]
  v_hL = DDataSim$v_hL[DDataSim$subject==s]
  v_hR = DDataSim$v_hR[DDataSim$subject==s]
  # ..........
  
  # get single value
  wh    <- DDataSim$wh[DDataSim$subject==s]
  wt    <- DDataSim$wt[DDataSim$subject==s]
  noise <- DDataSim$noise[DDataSim$subject==s]
  nDT   <- DDataSim$nDT[DDataSim$subject==s]
  bias  <- DDataSim$bias[DDataSim$subject==s]
  theta <- DDataSim$theta[DDataSim$subject==s]
  
  sub <- toString(unique(Data$subnum)[s])
  
  load(file = paste(c("/Users/dbao/google_drive_sherry/multiattribute_discounting/data/Modeling/recovery/aDDM_2w1theta/simulationInput/data_preppedAll_sub_", toString(sub),".RData"), collapse = "") )    
  
  for (i in 1:length(v_tL)) { # for each trial
    maxfix<- (ncol(data_mat)-3)/2 
    fixL = data_mat[i, 4: (3+maxfix)] - 1 # fixation location: Left(1) or Right(0)
    fixDur = data_mat[i, (4+maxfix):(3+2*maxfix)] # fixation duration
    
    rt <- ddm2_parallel(wt[i], wh[i], 1, nDT[i], theta[i], bias[i], 
                        v_tL[i], v_tR[i], v_hL[i], v_hR[i], 
                        fixL, fixDur, noise[i], 1)
    DDataSim$rt[DDataSim$subject==s & DDataSim$v_tL==v_tL[i] & DDataSim$v_tR==v_tR[i] & DDataSim$v_hL==v_hL[i] & DDataSim$v_hR==v_hR[i]] = rt

    ## get simulated fixation duration and proportion
    ToTfix_time = (abs(rt)-nDT[i])*1000 #in seconds
    fixDurSum = cumsum(fixDur)  
    fixStop = which(fixDurSum < ToTfix_time, arr.ind = TRUE) # array
    lastfixNum = fixStop[length(fixStop)]+1
    
    fixDur_real = c(fixDur[fixStop], (fixDur[lastfixNum]-(fixDurSum[lastfixNum] -ToTfix_time)))
    fixL_real = c(fixL[fixStop], fixL[lastfixNum])
    DDataSim$fixationTimeLeft[DDataSim$subject==s & DDataSim$v_tL==v_tL[i] & DDataSim$v_tR==v_tR[i] & DDataSim$v_hL==v_hL[i] & DDataSim$v_hR==v_hR[i]] = sum(fixDur_real[fixL_real==1],na.rm=TRUE)
    DDataSim$TotfixationTime[DDataSim$subject==s & DDataSim$v_tL==v_tL[i] & DDataSim$v_tR==v_tR[i] & DDataSim$v_hL==v_hL[i] & DDataSim$v_hR==v_hR[i]] = ToTfix_time
  }
}

# check the range of RT range(data$rt)
DDataSim$rt[DDataSim$rt > 50 | DDataSim$rt < -50] <- NaN # 10
#DDataSim$rt[DDataSim$rt < 0.2 & DDataSim$rt > -0.2]<-NaN
DataSim<-DDataSim[!(is.nan(DDataSim$rt)),]



# TO DO ...

save(DataSim, file=paste(c("/Users/dbao/google_drive_sherry/multiattribute_discounting/data/Modeling/recovery/aDDM_2w1theta/DataSims_",toString(Cond),".RData"), collapse = ""))
end_time <- Sys.time()
end_time - start_time

