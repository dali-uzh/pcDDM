rm(list=ls())
####### for fixing "input string 1 is invalid in this locale"
Sys.setlocale('LC_ALL','C')


####### package to plot data
pack = "ggplot2"
have = pack %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( pack[!have] ) }
# Now load it
suppressPackageStartupMessages(lapply(pack, require, character.only = TRUE))


#########
pathToFolder <- getwd()
mydata <- "dataSets/Stewart_etal_2016/stewart_risky_data.csv"
main_folder <- file.path(pathToFolder,"method_HtSSM_aDDM")
#load( file.path(main_folder, mydata))
data <- read.csv( file.path(main_folder, mydata))




######## total fixation duration
#totfix = sum(data$fixdur)
###fixation time to the option A
#fix_A <- sum(data$fixdur[data$roi_A==1])/ #[1] 0.5505934
####fixation time to the option B
#fix_B <- sum(data$fixdur[data$roi_A==0])/totfix #[1] 0.4494066
#choice_B= sum(data_trial$choice_A==0)/nrow(data_trial) #[1] 0.4473503


###### 
# subject numbers
subjs<- unique(data$subj)

for( s in subjs){
  for (t in unique(data$trial[data$subj==s])){
    #index for all fixations in one trial
    ind=which(data$subj==s & data$trial==t)
    #index for fixations on option A in the trial
    ind_A=which(data$subj==s & data$trial==t & data$roi_A==1)
    #index for fixations on option B in the trial
    ind_B=which(data$subj==s & data$trial==t & data$roi_A==0)
    #total fixation duration
    data$totfix[data$subj==s & data$trial==t] = sum(data$fixdur[ind])
    #fixation time to the option A
    data$fix_A[data$subj==s & data$trial==t] = sum(data$fixdur[ind_A])
    #fixation time to the option B
    data$fix_B[data$subj==s & data$trial==t] = sum(data$fixdur[ind_B])
  }
}

data$choice_B = (data$choice_A-1)*-1

###############################
# regression
# note singularities
#mylogit <- glm(choice_B ~ subj + trial + stake  + diffval_A +  fix_B + fix_B*diffval_A , data = data, family = "binomial")
#summary(mylogit)#AIC: 43359

#################################################
# correlation bt fix_B and choice_B

# discard all the fixations, keep the first one
data_trial<-data[data$fixnum==1,] 

choice_B_subj = NA
fix_B_subj = NA
fix_A_subj = NA


# calculate total fixation duration, 
#total fixation time to the option A and B
for( s in subjs){
  data_trial$total_trials[data_trial$subj == s] <- length(data_trial$trial[data_trial$subj==s]) 
  choice_B_subj[s] = sum(data_trial$choice_A==0 & data_trial$subj==s)/length(data_trial$trial[data_trial$subj==s]) 
  # some subjects has some missing trials (e.g., krajbich subj=9 without trial=170)
  fix_B_subj[s] = sum(data_trial$fix_B[data_trial$subj==s] / data_trial$totfix[data_trial$subj==s] )/length(data_trial$trial[data_trial$subj==s])
  fix_A_subj[s] = sum(data_trial$fix_A[data_trial$subj==s] / data_trial$totfix[data_trial$subj==s] )/length(data_trial$trial[data_trial$subj==s])
}

barplot(sort(choice_B_subj))
barplot(sort(fix_B_subj))

plot(choice_B_subj, fix_B_subj)

median(na.omit(fix_B_subj))
median(na.omit(choice_B_subj))

which(fix_B_subj > median(na.omit(fix_B_subj))) #10 18 20 21 24 25 27 28 29 34 36 37 41 42 43 44
which(fix_B_subj <= median(na.omit(fix_B_subj))) #9 11 12 14 15 17 19 22 23 26 30 31 32 33 38 39 40


