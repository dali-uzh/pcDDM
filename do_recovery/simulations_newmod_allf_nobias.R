rm(list=ls())

#load.runjagsmodule(fail=TRUE, silent=FALSE)
'%!in%' <- function(x,y)!('%in%'(x,y)) # overload my NotIn function

#--------------------------------
# Prepare the data SIMULATED
want = c("DEoptim", "doParallel", "doMC", "parallel", "foreach","stats4","pracma","ggplot2","runjags","RWiener","ggridges")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }


source('~/ErnstData/Framing/5_Analysis/functions/smrz.R')


load('~/ErnstData/Framing/6_Model/models_toRun/DataALLFixations.Rda')
load("~/ErnstData/Framing/6_Model/models_toRun/HaDDM2t/results_HaDDM_Fit_ALL_allfree_newmod_thetaFree_nobias.RData")
rm(Data_noAtt)

chain1= as.data.frame(rbind(results$mcmc[[1]], results$mcmc[[2]], results$mcmc[[3]]))

folder_plots="~/ErnstData/Framing/5_Analysis/plots/"

### prepare data
Data <- Data[!Data$SubjectNum %in% c(13,16),]
Data <- Data[Data$InitialFixation == 1,]

subs <- unique(Data$SubjectNum)



# model to simulate

model <- function(ev,mon, prob, gazeG,cond,N, thetaGs,thetaGg,noiseG,nDtG,biasG,driftG,alphaG, thetaLs,thetaLg,noiseL,nDtL,biasL,driftL,alphaL) {
  v_gamble <- rep(0,N)
  v_safe <- rep(0,N)
  li.hat <- rep(0,N)
  y_pred <- rep(0,N)
  for (i in 1:N) { # trial level
    # generate predictions
    if (cond[i]>0){
      
      v_gamble[i] <-  (mon[i]^alphaG)*prob[i]
      v_safe[i] <- (ev[i]^alphaG)
      
      li.hat[i] <- (driftG/(noiseG))*( (gazeG[i]*v_gamble[i] - (1-gazeG[i])*v_safe[i]) + thetaGg*((1-gazeG[i])*v_gamble[i]) - thetaGs*(gazeG[i]*v_safe[i]) )
      noise<-noiseG
      bias<-biasG
      nDt<-nDtG
      
    }else{
      v_gamble[i] <-  (mon[i]^alphaL)*prob[i]
      v_safe[i] <- (ev[i]^alphaL)
      
      li.hat[i] <- (driftL/(noiseL))*( (gazeG[i]*v_gamble[i] - (1-gazeG[i])*v_safe[i]) + thetaLg*((1-gazeG[i])*v_gamble[i]) - thetaLs*(gazeG[i]*v_safe[i]) )
      noise<-noiseL
      bias<-biasL
      nDt<-nDtL
    }
    y <- rwiener(1, (2/(noise)), nDt, bias, li.hat[i])
    if(y$resp =="upper"){
      y_pred[i] <- y$q
    } else {
      y_pred[i] <- -y$q
    }
  }
  return <- y_pred
}


# starting simulations


nsim=100
dataSim <- data.frame(SubjectNum = rep(Data$SubjectNum,nsim), 
                      trialCount = rep(Data$trialCount,nsim), 
                      simNum = sort(rep(1:nsim,length(Data$SubjectNum))),
                      ev = rep(Data$Ev,nsim),
                      mon = rep(Data$InitialMoney,nsim),
                      prob = rep(Data$Probabilities,nsim),
                      cond = rep(Data$TrialType,nsim),
                      rtReal = rep(Data$RT,nsim),
                      choiceReal = rep(Data$GambleChosen,nsim),
                      percentageFixGamble = rep(Data$PercentFixGamble,nsim))

countSub=1
for (s in subs){
  driftG = mean(chain1[,c( paste( c("b2G.p[",toString(countSub),"]"), collapse = ""))])
  thetaGs = mean(chain1[,c( paste( c("thetaGazeGs[",toString(countSub),"]"), collapse = ""))])
  thetaGg = mean(chain1[,c( paste( c("thetaGazeGg[",toString(countSub),"]"), collapse = ""))])
  biasG = 0.5
  noiseG = mean(chain1[,c( paste( c("alphaG.p[",toString(countSub),"]"), collapse = ""))])
  nDtG = mean(chain1[,c( paste( c("thetaG.p[",toString(countSub),"]"), collapse = ""))])
  alphaG = mean(chain1[,c( paste( c("gammaG.p[",toString(countSub),"]"), collapse = ""))])
  
  driftL = mean(chain1[,c( paste( c("b2L.p[",toString(countSub),"]"), collapse = ""))])
  thetaLs = mean(chain1[,c( paste( c("thetaGazeLs[",toString(countSub),"]"), collapse = ""))])
  thetaLg = mean(chain1[,c( paste( c("thetaGazeLg[",toString(countSub),"]"), collapse = ""))])
  biasL = 0.5
  noiseL = mean(chain1[,c( paste( c("alphaL.p[",toString(countSub),"]"), collapse = ""))])
  nDtL = mean(chain1[,c( paste( c("thetaL.p[",toString(countSub),"]"), collapse = ""))])
  alphaL = mean(chain1[,c( paste( c("gammaL.p[",toString(countSub),"]"), collapse = ""))])
  
  
  #d = mean(chain1[,c( "b2.mu")])
  # theta = mean(chain1[,c("thetaGaze.mu")])
  # bias = mean(chain1[,c( "bias.mu")])
  #thres = mean(chain1[, c("alpha.mu")])
  #nDt = mean(chain1[, c("ndt.mu")])
  
  DataSub<-dataSim[dataSim$SubjectNum==s,]
  ev <- (DataSub$ev)/100
  mon <- (DataSub$mon)/100
  prob <- (DataSub$prob)
  cond <- (DataSub$cond)
  gazeG <- DataSub$percentageFixGamble
  
  ####============================ here do simulation ==================== #############
  dataSim$rtSimulated[dataSim$SubjectNum==s] <- model(ev,mon, prob, gazeG,cond,length(DataSub$ev), thetaGs,thetaGg,noiseG,nDtG,biasG,driftG,alphaG, thetaLs,thetaLg,noiseL,nDtL,biasL,driftL,alphaL)
  
  countSub=countSub+1
}

#dataSim2<-dataSim[(dataSim$rtSimulated>=0.2 | dataSim$rtSimulated <= -0.2) & (dataSim$rtSimulated>= -30 & dataSim$rtSimulated<= 30 ),]
dataSim$rtReal[dataSim$choiceReal==0]<- -dataSim$rtReal[dataSim$choiceReal==0]
Data$RTnegpos<- Data$RT
Data$RTnegpos[Data$GambleChosen==1]<- -Data$RT[Data$GambleChosen==1]
dataSim$rtSimulated2<- -dataSim$rtSimulated
dataSim$choiceSureSim[dataSim$rtSimulated2>=0]<-1 
dataSim$choiceSureSim[dataSim$rtSimulated2<0]<-0 

Data$Type[Data$TrialType==-1]<-1
Data$Type[Data$TrialType==1]<-0
Data$Type=as.factor(Data$Type)
levels(Data$Type) <-c("Gain",  "Loss")

dataSim$Type[dataSim$cond==-1]<-1
dataSim$Type[dataSim$cond==1]<-0
dataSim$Type=as.factor(dataSim$Type)
levels(dataSim$Type) <-c("Gain",  "Loss")


# plot goodness of fit for the overall RT
#png( file.path(folder_plots, "rt_fit_Loss.png"), width = 5*300, height = 5*300,res = 300)
dev.new(width = 8, height = 4)
pp<-ggplot() + geom_histogram(data=Data, aes(y=..density..,RTnegpos,colour=Type),alpha = 0.1,position = "identity",binwidth=2.5)+
  geom_density(data=dataSim, aes(rtSimulated2),alpha = 0.5,position = "identity",linetype="dashed",size=0.8,colour="black")+
  ylab('density ') + 
  xlab( 'Gamble chosen                RT [s]                      Sure chosen' ) +
  scale_x_continuous(breaks=c(-30,-20,-10,0,10,20,30),labels=c("30","20","10","0","10","20","30"), limits= c(-35,35)) +
  #ggtitle("Only-imgs")+
  geom_hline(yintercept = 0 ,colour = 'grey',size=0.8)+
  theme(text = element_text(family = 'Arial')) +
  theme_ridges() +
  theme_bw() +
  #theme( plot.title = element_text(hjust = 0.5))+
  #theme(legend.position = "none")+
  #theme_classic()+
  theme(text = element_text(family = 'Arial'),
        axis.text.y=element_blank(),
        strip.background = element_rect(fill = "white", colour = "black", size = rel(2))) +
  facet_grid(Type~.)+
  theme(panel.grid = element_blank())
print(pp)
#dev.off()
save(results,dataSim, Data,file='~/ErnstData/Framing/6_Model/models_toRun/HaDDM2t/simulations_HaDDM_allfree_newmod_allf_nobias.Rdata')






