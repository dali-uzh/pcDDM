## data from
# paper: Smith, S. M., & Krajbich, I. (2018). Attention and choice across domains. Journal of Experimental Psychology: General, 147(12), 1810.
# website: https://osf.io/d7s6c/ (in Yang & Krajbich, 2022 paper, Yang's osf)
# (Smith & Krajbich, 2018 provided weired data, 
# https://supp.apa.org/psycarticles/supplemental/xge0000482/xge0000482_supp.html
# Smith's osf: https://osf.io/g7cv6/?view_only=2669d8f3983d4442952a52c5de5814f7 )

# subject number: 33
# trial number:  81~200 see below data exclusion criteria
# Risky choice amount difference range:  79~264
# Sure choice amount difference range:  0.0945~136
# Risky amount:  30~300
# Sure amount: 62~252

# data exclusion criteria
# we excluded trials where the logarithm of RT in this trial was two standard deviations above its mean or shorter than 300 ms. 
# Additionally, we excluded subjects who had less than 80 trials 
# or who averaged less than two fixations per trial.


## preprocessing: get prob and amount for each Risky(A) and Sure(B) option


rm(list=ls())
####### for fixing "input string 1 is invalid in this locale"
Sys.setlocale('LC_ALL','C')


## load data
pathToFolder <- getwd()
mydata <- "moneyrisk.csv"
data_folder <- file.path(pathToFolder,"dataSets/Smith_Krajbich_2018")
data<-read.csv( file.path(data_folder, mydata)) 

name_newdata = 'krajbich_risk_data'

####################################################
## val_diff
data$val_diff_L <- abs(data$upleftval - data$downleftval)
data$val_diff_R <- abs(data$uprightval - data$downrightval)


## option A (risky) is left, roi_A , option, Choice_A
#(1: upper left 2: upper right 3: down left 4: down right). 

for (i in 1:nrow(data)){
  if (data$val_diff_L[i] > data$val_diff_R[i])
    {
    data$A_in_left[i] <- 1
    
    # option
    data$val_A1[i] <- (data$upleftval[i])/1
    data$val_A2[i] <- (data$downleftval[i])/1
    data$val_B1[i] <- (data$uprightval[i])/1
    data$val_B2[i] <- (data$downrightval[i])/1
    
    # roi
    if (data$roi[i] == 1 | data$roi[i] == 3)
      {data$roi_A[i] <- 1}
    else
    {data$roi_A[i] <- 0}
    
    # choice
    if (data$choice[i] == 1)
    {data$choice_A[i] <- 1}
    else
    {data$choice_A[i] <- 0}
  }
  
  else
  {
    data$A_in_left[i] <- 0
    
    # option
    data$val_A1[i] <- (data$uprightval[i])/1
    data$val_A2[i] <- (data$downrightval[i])/1
    data$val_B1[i] <- (data$upleftval[i])/1
    data$val_B2[i] <- (data$downleftval[i])/1
    
    # roi
    if (data$roi[i] == 2 | data$roi[i] == 4)
    {data$roi_A[i] <- 1}
    else
    {data$roi_A[i] <- 0}
    
    # choice
    if (data$choice[i] == 0)
    {data$choice_A[i] <- 1}
    else
    {data$choice_A[i] <- 0}
    
  }
}


## trial (original id, may be discounti)


## subject (original id, may be discounti)
#data$subject <- as.numeric(substr(data$sub,2,3)) # s1 ->1
data$subject <- data$subj

# if looking at option A (risky)
#omit


# RT
data$RT <- data$rt

# fixation (original id, may discounti)
data$fixation <- data$fixnum

# fixation duration
data$fixDur <-data$fixdur

# fix_idx, fix_total, fixDur_sum, fixDur_A_sum, fixDur_B_sum, trial_total, 
subjs <- unique(data$subject)
data$fix_idx <- NA
data$fix_total <- NA
data$trial_total <- NA
data$fixDur_sum<- NA
data$fixDur_A_sum<- NA
data$fixDur_B_sum<- NA

for (s in subjs) {
  data$trial_total[data$subject==s] = length(unique(data$trial[data$subject==s]))
  for (t in unique(data$trial[data$subject==s])) {
    this_s_t <- which(data$subject==s & data$trial==t)
    
    data$fix_idx[this_s_t] = order(data$fixation[this_s_t])
    data$fix_total[this_s_t] = length(data$fix_idx[this_s_t])
    data$fixDur_sum[this_s_t] = sum(data$fixDur[this_s_t])
    #fix duration to the option A of subject s in trial t
    data$fixDur_A_sum[this_s_t] = sum(data$fixDur[which(data$subject==s & data$trial==t & data$roi_A==1)])
    #fix duration to the option B of subject s in trial t
    data$fixDur_B_sum[this_s_t] = sum(data$fixDur[which(data$subject==s & data$trial==t & data$roi_A==0)])
  }
}



# remove useless columns with NA
apply(is.na(data), 2, which)

# save data
write.csv(data, file=file.path(data_folder,paste(name_newdata, ".csv", sep ="")))



##### data details 

# A_money, B_money, A_prob, B_prob: no need
sort(unique(data$val_A1)) # 30~300
sort(unique(data$val_A2)) # 30~300
sort(unique(data$val_B1)) # 62~252
sort(unique(data$val_B2)) # 62~252
max(data$val_A1-data$val_A2) #264 = max(A2-A1)
min(abs(data$val_A1-data$val_A2)) # 79
max(data$val_A1-data$val_A2) #136
min(abs(data$val_B1-data$val_B2)) # 0.0945

unique(data[c("val_A1","val_A2","val_B1","val_B2")])

unique(data$trial_total)

trial_total_subj <- NA
for (s in subjs){trial_total_subj[s] <- unique(data$trial_total[data$subject == s]) }
trial_total_subj
min(trial_total_subj[!is.na(trial_total_subj)]) # 81
max(trial_total_subj[!is.na(trial_total_subj)]) # 200
#unique(data[data$subject==10, c("val_A1","val_A2","val_B1","val_B2")])

length(unique(data$subject)) #33




## check
unique(data$fixDur_A_sum+data$fixDur_B_sum-data$fixDur_sum)
