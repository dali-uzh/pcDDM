## data from
# paper: Lombardi, G., & Hare, T. (2021). Piecewise constant averaging methods allow for fast and accurate hierarchical Bayesian estimation of drift diffusion models with time-varying evidence accumulation rates.
# website: https://github.com/galombardi/method_HtSSM_aDDM


# subject number: 30
# trial number:  68(#39), 69(#15, 17, 32, 42), 70
# Gamble amount: 10, 20, ..., 100 
# Gamble prob: 0.3, 0.4, ..., 0.9
# Sure amount: 3, 4, ..., 90
# Sure prob: 1

## preprocessing: get prob and amount for each gamble and B option


rm(list=ls())
####### for fixing "input string 1 is invalid in this locale"
Sys.setlocale('LC_ALL','C')


## load data
pathToFolder <- getwd()
mydata <- "Data_RiskySafe.Rda"
data_folder <- file.path(pathToFolder,"dataSets/Gaia_Hare_2021")
load( file.path(data_folder, mydata)) 

name_newdata = 'gaia_risk_data'

####################################################


## data-specific process
## remove data with non-ROI fixation
data <- Data


## trial (original id, may be discounti)
data$trial <- data$trialCount

## option A (risky) is left
data$A_in_left <- -1*(data$GambleRight-1)


## Choice_A
# GambleChosen -> =1 if the gamble option is chosen, =0 if the safe option is chosen
data$choice_A <- data$GambleChosen



## subject (original id, may be discounti)
data$subject <- data$SubjectNum

# if looking at option A (risky)
# FixateGamble -> 1 if the gamble is fixated, 0 if the safe option is fixated
data$roi_A <- data$FixateGamble
data$roi_B <- -1*(data$FixateGamble-1)


# RT (ms)
# raw RT is in ms by gaia
# but it is in sec for sure...
data$RT <- data$RT*1000 # make it in ms

# fixation (original id, may discounti)
data$fixation <- data$FixationNum

# fixation duration (ms)
# raw fixationTime is in seconds by gaia
# but it is ms for sure....
data$fixDur <-data$fixationTime 

# option, fix_idx, fix_total, fixDur_sum, fixDur_A_sum, fixDur_B_sum, trial_total, 
# note gaia has PercentFixGamble -> proportion of time the gamble is attended

subjs <- unique(data$subject)

data$fix_idx <- NA
data$fix_total <- NA
data$trial_total <- NA
data$fixDur_sum <- NA
data$fixDur_B_sum<- NA
data$fixDur_A_sum<- NA

data$val_A<-NA
data$val_B<-NA
data$prob_A <-NA
data$prob_B <-NA

for (s in subjs) 
  {
  data$trial_total[data$subject==s] = length(unique(data$trial[data$subject==s]))
  for (t in unique(data$trial[data$subject==s])) 
    {
    this_s_t <- which(data$subject==s & data$trial==t)
    
    data$fix_idx[this_s_t] = order(data$fixation[this_s_t])
    data$fix_total[this_s_t] = length(data$fix_idx[this_s_t])
    data$fixDur_sum[this_s_t] = sum(data$fixDur[this_s_t])
    #fix duration to the option A of subject s in trial t
    data$fixDur_A_sum[this_s_t] = sum(data$fixDur[which(data$subject==s & data$trial==t & data$roi_A==1)])
    #fix duration to the option B of subject s in trial t
    data$fixDur_B_sum[this_s_t] = sum(data$fixDur[which(data$subject==s & data$trial==t & data$roi_A==0)])
    
    if (unique(data$A_in_left[this_s_t]) == 1)
    {
    data$val_A[this_s_t] <- (data$MoneyLeft[this_s_t])/1
    data$prob_A[this_s_t] <- (data$ProbLeft[this_s_t])/1
    data$val_B[this_s_t] <- (data$MoneyRight[this_s_t])/1
    data$prob_B[this_s_t] <- (data$ProbRight[this_s_t])/1
    }
    else if(unique(data$A_in_left[this_s_t]) == 0)
    {
      data$val_A[this_s_t] <- (data$MoneyRight[this_s_t])/1
      data$prob_A[this_s_t] <- (data$ProbRight[this_s_t])/1
      data$val_B[this_s_t] <- (data$MoneyLeft[this_s_t])/1
      data$prob_B[this_s_t] <- (data$ProbLeft[this_s_t])/1
      # prob here is already 0~1 
    }
    }
  }


# remove useless columns with NA
#apply(is.na(data), 2, which)
data$MeanMiddleFixDuration<-NULL
data$MedianMiddleFixDuration<-NULL
apply(is.na(data), 2, which)

# save data
write.csv(data, file=file.path(data_folder,paste(name_newdata, ".csv", sep ="")))



##### data details 

# A_money, B_money, A_prob, B_prob: no need
sort(unique(data$val_A))
sort(unique(data$prob_A))
sort(unique(data$val_B))
sort(unique(data$prob_B))
unique(data$trial_total) #70, 69,68
trial_total_subj <- NA
for (s in subjs){trial_total_subj[s] <- unique(data$trial_total[data$subject == s]) }
trial_total_subj # 
which(trial_total_subj==68) #39
which(trial_total_subj==69) #15, 17, 32, 42
unique(data[data$subject==39, c("val_A","prob_A","val_B","prob_B")])


length(unique(data$subject)) # 30
 
## check
# unique(data$fixDur_A_sum+data$fixDur_B_sum-data$fixDur_sum)
# unique(data$fixDur_A_sum - data$FixationTimeGamble)
# unique(data$fixDur_sum- data$TotFixTime)
# unique(data$A_in_left + data$GambleRight)

