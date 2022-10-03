rm(list=ls())
cond = 2
library(ggplot2)
# ==========================================================================
# load data and model
pathToFolder <- "/Users/dbao/google_drive_sherry/multiattribute_discounting/data"
mydata <- paste(c("DataSims_", toString(cond),".RData"), collapse = "")
# load data 
data_folder  <- file.path(pathToFolder,"Modeling/recovery/aDDM_2w1theta")
load(file.path(data_folder, mydata))
save_folder <- file.path(pathToFolder, 'Modeling/figs')

abnormal = which(((DataSim$fixationTimeLeft == 0) & (DataSim$rt > 0))|((DataSim$fixationTimeLeft == DataSim$TotfixationTime) & (DataSim$rt < 0)))
normal = which(((DataSim$fixationTimeLeft == DataSim$TotfixationTime)& (DataSim$rt > 0)) |((DataSim$fixationTimeLeft == 0) & (DataSim$rt < 0))|((DataSim$fixationTimeLeft > 0) & (DataSim$fixationTimeLeft < DataSim$TotfixationTime)))

generate_hist_data<- function(type){
  result = list()
  choice = DataSim[type,]
  
  choice_vt = choice$v_tL[choice$rt>0]
  result$choice_vt = append(choice_vt, choice$v_tR[choice$rt<0])
  nochoice_vt = choice$v_tL[choice$rt<0]
  result$nochoice_vt = append(nochoice_vt, choice$v_tR[choice$rt>0])
  
  choice_vh = choice$v_hL[choice$rt>0]
  result$choice_vh = append(choice_vh, choice$v_hR[choice$rt<0])
  nochoice_vh = choice$v_hL[choice$rt<0]
  result$nochoice_vh = append(nochoice_vh, choice$v_hR[choice$rt>0])
  
  return(result)
}

plot_check_hist<- function(data,name) {
  fig <- ggplot() + 
    geom_histogram(aes(data),alpha = 0.8,position = "identity",binwidth=0.01,fill="#cdb4db",colour="black")+
    #coord_cartesian(xlim = c(0.25,1), ylim = c(0,8000)) +
    ylab(' ') +
    xlab( name ) +
    theme(text = element_text(family = 'Arial')) +
    theme_bw() +
    theme(panel.grid = element_blank()) 
  print(fig)
  ggsave(paste( name, '_', cond, sep=""),plot = last_plot(), device = 'png', path = save_folder, dpi = 700)
}
result_abnormal = generate_hist_data(abnormal)
plot_check_hist(result_abnormal$choice_vt,'abnormal_choice_vt')
plot_check_hist(result_abnormal$nochoice_vt,'abnormal_nochoice_vt')
plot_check_hist(result_abnormal$choice_vh,'abnormal_choice_vh')
plot_check_hist(result_abnormal$nochoice_vh, 'abnormal_nochoice_vh')

result_normal = generate_hist_data(normal)
plot_check_hist(result_normal$choice_vt,'normal_choice_vt')
plot_check_hist(result_normal$nochoice_vt,'normal_nochoice_vt')
plot_check_hist(result_normal$choice_vh,'normal_choice_vh')
plot_check_hist(result_normal$nochoice_vh, 'normal_nochoice_vh')