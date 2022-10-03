rm(list=ls())

cond = 3
cond2= 2
cond3= 3

modelname = 'stDDM_1w1d'
switch(modelname,
       'stDDM'        = (setting = c(time = 1,w = 2, theta = 0, dscale = 0)),
       'stDDM_1w1d'   = (setting = c(time = 1,w = 1, theta = 0, dscale = 1)),
       'aDDM_2w2theta'= (setting = c(time = 0,w = 2, theta = 2, dscale = 0)),
       'aDDM_2w1theta'= (setting = c(time = 0,w = 2, theta = 1, dscale = 0)),
       'aDDM_1w2theta'= (setting = c(time = 0,w = 1, theta = 2, dscale = 0)))

### PLOT GROUP LEVEL DISTRIBUTION OF PARAMETERS FITTING ######
# ==========================================================================
## package to plot data
pack = "ggplot2"
have = pack %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( pack[!have] ) }
# Now load it
suppressPackageStartupMessages(lapply(pack, require, character.only = TRUE))

# ==========================================================================
pathToFolder <- "/Users/dbao/google_drive_sherry/multiattribute_discounting/data"
folder.recovery <- file.path(pathToFolder, "Modeling/recovery", modelname)
folder.results <- file.path(pathToFolder, "Modeling/fit", modelname)
save_folder <- file.path(pathToFolder, 'Figures/recovery', modelname) # , 'with_exclusion'

# load fitting results
recoveryFileName <- paste("refit_results_",modelname, "_", cond, ".RData", sep="")
resultsFileName <- paste("results_",modelname, "_", cond, ".RData", sep="")

load(file.path(folder.recovery, recoveryFileName))
chain.recovery=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))
load(file.path(folder.results, resultsFileName))
chain.results=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))

recoveryFileName <- paste("refit_results_",modelname, "_", cond2, ".RData", sep="")
resultsFileName <- paste("results_",modelname, "_", cond2, ".RData", sep="")

load(file.path(folder.recovery, recoveryFileName))
chain.recovery2=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))
load(file.path(folder.results, resultsFileName))
chain.results2=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))

recoveryFileName <- paste("refit_results_",modelname, "_", cond3, ".RData", sep="")
resultsFileName <- paste("results_",modelname, "_", cond3, ".RData", sep="")

load(file.path(folder.recovery, recoveryFileName))
chain.recovery3=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))
load(file.path(folder.results, resultsFileName))
chain.results3=as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))


# ==========================================================================
# NDT PARAMETER GROUP LEVEL
figNDT <- ggplot() + 
  geom_histogram(data=chain.recovery,aes(ndt.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#add8e6",colour="black")+
  geom_histogram(data=chain.results, aes(ndt.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#ffc0cb",colour="black")+
  ylab(' ') +
  xlab( 'Non-decision time' ) +
  ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
  theme(text = element_text(family = 'Arial')) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figNDT)
ggsave(paste('ndt_mu_', cond, sep=""),plot = figNDT, device = 'png', path = save_folder, dpi = 700)

# ==========================================================================
# NOISE PARAMETER GROUP LEVEL
figNoise <- ggplot() + 
  geom_histogram(data=chain.recovery,aes(alpha.mu),alpha = 0.5,position = "identity",binwidth=0.005,fill="#add8e6",colour="black")+
  geom_histogram(data=chain.results, aes(alpha.mu),alpha = 0.5,position = "identity",binwidth=0.005,fill="#ffc0cb",colour="black")+
  ylab(' ') +
  xlab( 'Noise' ) +
  ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
  theme(text = element_text(family = 'Arial')) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figNoise)
ggsave(paste('alpha_mu_', cond, sep=""),plot = figNoise, device = 'png', path = save_folder, dpi = 700)

# ==========================================================================
# BIAS PARAMETER GROUP LEVEL
figBias <- ggplot() + 
  geom_histogram(data=chain.recovery,aes((bias.mu-0.5)/2),alpha = 0.5,position = "identity",binwidth=0.001,fill="#add8e6",colour="black")+
  geom_histogram(data=chain.results, aes((bias.mu-0.5)/2),alpha = 0.5,position = "identity",binwidth=0.001,fill="#ffc0cb",colour="black")+
  ylab(' ') +
  xlab( 'Bias' ) +
  ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
  theme(text = element_text(family = 'Arial')) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figBias)
ggsave(paste('bias_mu_', cond, sep=""),plot = figBias, device = 'png', path = save_folder, dpi = 700)

# ==========================================================================
# TIME PARAMETER GROUP LEVEL
# if it's positive the b1 attribute (health) comes later at time t, if it's negative the b2 attribute (taste) comes later at time -t
if (as.numeric(setting['time']) == 1){
  figTime <- ggplot() + 
    geom_histogram(data=chain.recovery, aes(time.mu),alpha = 0.5,position = "identity",binwidth=0.05,fill="#add8e6",colour="black") +
    geom_histogram(data=chain.results, aes(time.mu),alpha = 0.5,position = "identity",binwidth=0.05,fill="#ffc0cb",colour="black") +
    ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
    ylab(' ') +
    xlab( 'Time health in' ) +
    theme(text = element_text(family = 'Arial')) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  print(figTime)
  ggsave(paste('time_mu_', cond, sep=""),plot = last_plot(), device = 'png', path = save_folder, dpi = 700)
}

# ==========================================================================
# SCALE PARAMETER GROUP LEVEL
# if it's positive the b1 attribute (health) comes later at time t, if it's negative the b2 attribute (taste) comes later at time -t
if (as.numeric(setting['dscale']) == 1){
  figTime <- ggplot() + 
    geom_histogram(data=chain.recovery, aes(d.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#add8e6",colour="black") +
    geom_histogram(data=chain.results, aes(d.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#ffc0cb",colour="black") +
    ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
    ylab(' ') +
    xlab( 'scaling' ) +
    theme(text = element_text(family = 'Arial')) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  print(figTime)
  ggsave(paste('d_mu_', cond, sep=""),plot = last_plot(), device = 'png', path = save_folder, dpi = 700)

  figTime <- ggplot() + 
    geom_histogram(data=chain.recovery3-chain.recovery2, aes(d.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#add8e6",colour="black") +
    geom_histogram(data=chain.results3-chain.results2, aes(d.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#ffc0cb",colour="black") +
    ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
    ylab(' ') +
    xlab( 'scaling difference' ) +
    theme(text = element_text(family = 'Arial')) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  print(figTime)
  ggsave(paste('d_mu_diff_23', sep=""),plot = last_plot(), device = 'png', path = save_folder, dpi = 700)
  
  
}


# ==========================================================================
# WEIGHT PARAMETERS GROUP LEVEL
if (as.numeric(setting['w']) == 2){

  figWeight1 <- ggplot() + 
    geom_histogram(data=chain.recovery,aes(b1.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#add8e6",colour="black")+
    geom_histogram(data=chain.results, aes(b1.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#ffc0cb",colour="black")+
    ylab(' ') +
    xlab( 'Weights b1' ) +
    ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
    theme(text = element_text(family = 'Arial')) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  print(figWeight1)
  ggsave(paste('b1_mu_', cond, sep=""),plot = figWeight1, device = 'png', path = save_folder, dpi = 700)

figWeight2 <- ggplot() + 
  geom_histogram(data=chain.recovery,aes(b2.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#add8e6",colour="black")+
  geom_histogram(data=chain.results, aes(b2.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#ffc0cb",colour="black")+
  ylab(' ') +
  xlab( 'Weights b2' ) +
  ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
  theme(text = element_text(family = 'Arial')) +
  theme_bw() +
  theme(panel.grid = element_blank()) 
print(figWeight2)
ggsave(paste('b2_mu_', cond, sep=""),plot = figWeight2, device = 'png', path = save_folder, dpi = 700)
}

if (as.numeric(setting['w']) == 1){
  figWeight <- ggplot() + 
    geom_histogram(data=chain.recovery, aes(b.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#add8e6",colour="black")+
    geom_histogram(data=chain.results, aes(b.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#ffc0cb",colour="black")+
    # coord_cartesian(xlim = c(0.3,1.1), ylim = c(0,6500)) +
    ylab(' ') +
    xlab( 'Weight' ) +
    ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
    theme(text = element_text(family = 'Arial')) +
    # annotate(geom="text", x=c(0.97,0.7), y=c(3000,3000), label=c("Health","Taste"),
    #           color=c("pink","red") ) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  print(figWeight)
  ggsave(paste('b_mu_', cond, sep=""),plot = last_plot(), device = 'png', path = save_folder, dpi = 700)
}


# ==========================================================================
# DISCOUNT GROUP LEVEL
#dev.new(width = 5, height =5)
if (as.numeric(setting['theta']) == 1){
  figTheta <- ggplot() + 
    geom_histogram(data=chain.recovery,aes(thetaGaze.mu),alpha = 0.8,position = "identity",binwidth=0.01,fill="#add8e6",colour="black")+ #
    geom_histogram(data=chain.results, aes(thetaGaze.mu),alpha = 0.5,position = "identity",binwidth=0.01,fill="#ffc0cb",colour="black")+    
    #coord_cartesian(xlim = (c(0.4,0.6)-0.5)/2, ylim = c(0,8000)) +
    ylab(' ') +
    xlab( 'Discounting' ) +
    ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
    theme(text = element_text(family = 'Arial')) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  print(figTheta)
  ggsave(paste('thetaGaze_mu_', cond, sep=""),plot = last_plot(), device = 'png', path = save_folder, dpi = 700)
}

# ==========================================================================
# MULTIDISCOUNT GROUP LEVEL
# pink is the health attribute and blue is the taste attribute
#dev.new(width = 5, height =5)
if (as.numeric(setting['theta']) == 2){
  figTheta2 <- ggplot() + 
    geom_histogram(data=chain.recovery, aes(thetaGazeH.mu),alpha = 0.8,position = "identity",binwidth=0.01,fill= '#add8e6')+ #blue
    geom_histogram(data=chain.results, aes(thetaGazeH.mu),alpha = 0.8,position = "identity",binwidth=0.01,fill= "#ffc0cb")+ 
    #coord_cartesian(xlim = (c(0.4,0.6)-0.5)/2, ylim = c(0,8000)) +
    ylab(' ') +
    xlab( 'Discounting Health' ) +
    ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
    theme(text = element_text(family = 'Arial')) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_color_manual(name = 'attribute',
                       breaks=c('health', 'taste'),
                       values=c('health'= '#ffc0cb', 'taste'='#add8e6'))
  print(figTheta2)
  ggsave(paste('thetaGazeH_mu_', cond, sep=""),plot = last_plot(), device = 'png', path = save_folder, dpi = 700)

  figTheta2 <- ggplot() + 
    geom_histogram(data=chain.recovery, aes(thetaGazeT.mu),alpha = 0.8,position = "identity",binwidth=0.01,fill= '#add8e6')+  #blue
    geom_histogram(data=chain.results, aes(thetaGazeT.mu),alpha = 0.8,position = "identity",binwidth=0.01,fill= "#ffc0cb")+
    #coord_cartesian(xlim = (c(0.4,0.6)-0.5)/2, ylim = c(0,8000)) +
    ylab(' ') +
    xlab( 'Discounting Taste' ) +
    ggtitle(paste("Posterior distribution of the group level mean (condition ", cond, ")", sep=""))+
    theme(text = element_text(family = 'Arial')) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    scale_color_manual(name = 'attribute',
                       breaks=c('health', 'taste'),
                       values=c('health'= '#ffc0cb', 'taste'='#add8e6'))
  print(figTheta2)
  ggsave(paste('thetaGazeT_mu_', cond, sep=""),plot = last_plot(), device = 'png', path = save_folder, dpi = 700)

  
  }


