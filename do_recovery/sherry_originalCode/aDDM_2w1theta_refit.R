rm(list=ls())
start_time <- Sys.time()
library(runjags)

# Enable the use of command line argument inputs to the R script for cluster
args = commandArgs(TRUE)
input1 = args[1]
cond = input1
print(cond)

# ==========================================================================
# load data and model
pathToFolder <-"/data/dbao/fixation" # for cluster
mydata <- paste(c("DataSims_", toString(cond),".RData"), collapse = "")
# load data 
data_folder  <- file.path(pathToFolder,"Data/recovery")
model_folder <- file.path(pathToFolder,"Modeling/recovery")
load(file.path(data_folder, mydata))
Data = DataSim

# ==========================================================================

# RT is positive if left food item chosen, negative if right food item chosen
idx = which(Data$rt>0)
Data$choice <- rep(0, length(Data$rt))
Data$choice[idx] = 1
y = Data$rt
N = length(y)

idxP = as.numeric(ordered(Data$subject)) #makes a sequentially numbered subj index
ns = length(unique(idxP))

# scale value of health and taste # already scaled
v_tL = (Data$v_tL) 
v_tR = (Data$v_tR)
v_hL = (Data$v_hL)
v_hR = (Data$v_hR)

# proportion of fixations to the left option (nb. fixright = 1-gazeL)
gazeL = Data$fixationTimeLeft/Data$TotfixationTime

dat <- dump.format(list(N=N, y=y, ns=ns, idxP=idxP, 
                        v_tL=v_tL, v_tR=v_tR, v_hL=v_hL,v_hR=v_hR,
                        gazeL=gazeL))

# ==========================================================================
# fit the model
nc = 1

# alpha.mu1 = as.vector(matrix(1 + rnorm(nc)*0.2,nc,1))
# alpha.mu2 = as.vector(matrix(1.3 + rnorm(nc)*0.2,nc,1))
# alpha.mu3 = as.vector(matrix(1.3 + rnorm(nc)*0.2,nc,1))
# alpha.si1 = as.vector(matrix(runif(nc)*10, nc,1))
# alpha.si2 = as.vector(matrix(runif(nc)*10, nc,1))
# alpha.si3 = as.vector(matrix(runif(nc)*10, nc,1))

inits1 <- dump.format(list( alpha.mu=rep(0.5,nc), alpha.pr=rep(0.05,nc), thetaGaze.mu=0.6, thetaGaze.kappa=1,
                      ndt.mu=rep(0.1,nc), ndt.pr=rep(0.05, nc), b1.mu=rep(0.3,nc), b1.pr=rep(0.05,nc), b2.mu=rep(0.01,nc), b2.pr=rep(0.05, nc), 
                      bias.mu=rep(0.4,nc), bias.kappa=rep(1,nc), y_pred=y, .RNG.name="base::Super-Duper", .RNG.seed=99999))

inits2 <- dump.format(list( alpha.mu=rep(0.1,nc), alpha.pr=rep(0.05,nc), thetaGaze.mu=0.5, thetaGaze.kappa=1, 
                      ndt.mu=rep(0.01,nc), ndt.pr=rep(0.05, nc), b1.mu=rep(0.1,nc), b1.pr=rep(0.05,nc), b2.mu=rep(0.2,nc), b2.pr=rep(0.05, nc), 
                      bias.mu=rep(0.5,nc), bias.kappa=rep(1,nc), y_pred=y,  .RNG.name="base::Wichmann-Hill", .RNG.seed=1234))

inits3 <- dump.format(list( alpha.mu=rep(0.2,nc), alpha.pr=rep(0.05,nc), thetaGaze.mu=0.2, thetaGaze.kappa=1, 
                      ndt.mu=rep(0.15,nc), ndt.pr=rep(0.05, nc), b1.mu=rep(0.5,nc), b1.pr=rep(0.05,nc), b2.mu=rep(0.3,nc), b2.pr=rep(0.05, nc), 
                      bias.mu=rep(0.6,nc), bias.kappa=rep(1,nc), y_pred=y, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

monitor = c(
  "b1.mu", "b1.p", "b2.mu", "b2.p", 
  "ndt.mu", "ndt.p", 
  "alpha.mu", "alpha.p", 
  "bias.mu", "bias.p",
  "thetaGaze.mu", "thetaGaze.kappa", "thetaGaze", 
  "deviance" )

# run the fitting
results <- run.jags(model=file.path(model_folder,"/models/aDDM_2w1theta.txt"), monitor=monitor, data=dat, n.chains=3, inits=c(inits1,inits2,inits3), plots = TRUE, method="parallel", module="wiener", burnin=50000, sample=10000, thin=10)

# save 
suuum<-summary(results)
save_folder <- file.path(pathToFolder,"Results/recovery")
save(results, Data, file=file.path(save_folder, file=paste("refit_results_aDDM_2w1theta_", cond, ".RData", sep="")))
write.csv(suuum, file=file.path(save_folder, file=paste("refit_results_aDDM_2w1theta_", cond, ".csv", sep="")))

end_time <- Sys.time()
end_time - start_time