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
