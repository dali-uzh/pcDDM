## data from
# paper: Stewart, N., Hermens, F., & Matthews, W. J. (2016). Eye movements in risky choice. Journal of Behavioral Decision Making, 29, 116???136. https://doi.org/10.1002/bdm.1854
# website: https://github.com/neil-stewart/eye_tracking_risky_choice

# subject number: 48
# trial number: 71 (subj27 with 70) (4 catch trials not included)
# Gamble prob: 0.3 0.4 0.5 0.6 0.7 0.8 0.9
# Gamble amount: 500 400 300
# Sure prob: 0.4 0.5 0.6 0.7 0.8 0.9 1.0
# Sure amount: 100 200 300

## preprocessing: get prob and amount for each gamble and B option


rm(list=ls())
####### for fixing "input string 1 is invalid in this locale"
Sys.setlocale('LC_ALL','C')


## load data
pathToFolder <- getwd()
mydata <- "cleaned_data.csv"
data_folder <- file.path(pathToFolder,"dataSets/Stewart_etal_2016")
data<-read.csv( file.path(data_folder, mydata)) 

name_newdata = 'stewart_risk_data'

####################################################


## data-specific process
## remove data with non-ROI fixation
data <- data[!data$region==' ',] # note this makes the row index discontinuous


## trial (original id, may be discounti)

## option 
data$val_A <- data$x
data$val_B <- data$y
data$prob_A <- data$p /100 # raw val of prob is 30~100
data$prob_B <- data$q /100 

## subject (original id, may be discounti)
data$subject <- as.numeric(substr(data$sub,2,3)) # s1 ->1

# if looking at option A (risky)
data$roi_A <-   as.numeric(data$region == 'p' | data$region == 'x')
data$roi_B <-   as.numeric(data$region == 'q' | data$region == 'y')

# Choice
data$choice_A <- as.numeric(data$choice == 'px') 
#data$choice_B <- (data$choice_A-1)*-1
data$choice_B <- as.numeric(data$choice == 'qy') 

# RT (ms)
data$RT <- data$rt

# fixation (original id, may discounti)
# fixation

# fixation duration (ms)
data$fixDur <-data$duration

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
data$prev.region.c<-NULL
data$type<-NULL
data$prev.region<- NULL 
apply(is.na(data), 2, which)

# save data
write.csv(data, file=file.path(data_folder,paste(name_newdata, ".csv", sep ="")))



##### data details 

# A_money, B_money, A_prob, B_prob: no need
sort(unique(data$val_A_scaled))
sort(unique(data$prob_A))
sort(unique(data$val_B_scaled))
sort(unique(data$prob_B))
unique(data$trial_total) # total trial: subjects have 71 trials (subj27 with 70). 
trial_total_subj <- NA
for (s in subjs){trial_total_subj[s] <- unique(data$trial_total[data$subject == s]) }
trial_total_subj
d_27 <- unique(data[data$subject==27, c("val_A_scaled","prob_A","val_B_scaled","prob_B")])
# subj 27 miss one trial with val_A = 500

length(unique(data$subject))

# subject number: 48
# trial number: 71 (subj27 with 70) (4 catch trials not included)
# Gamble prob: 0.3 0.4 0.5 0.6 0.7 0.8 0.9
# Gamble amount: 500 400 300
# B prob: 0.4 0.5 0.6 0.7 0.8 0.9 1.0
# B amount: 100 200 300



## check
#unique(data$fixDur_A_sum+data$fixDur_B_sum-data$fixDur_sum)
