modelname = 'stDDM_1w1d'

pathToFolder <- "/Users/dbao/google_drive_sherry/multiattribute_discounting/data"
folder.recovery <- file.path(pathToFolder, "Modeling/recovery", modelname)
folder.results <- file.path(pathToFolder, "Modeling/fit", modelname)

emp = read.csv(paste(folder.results,"/results_stDDM_1w1d_2.csv",sep=''), header = TRUE)
sim = read.csv(paste(folder.recovery,"/refit_results_stDDM_1w1d_2.csv",sep=''), header = TRUE)

edf = data.frame(
  b = emp[grep("b.p", emp$X), "Mean"],
  d = emp[grep("d.p", emp$X), "Mean"],
  ndt = emp[grep("ndt.p", emp$X), "Mean"],
  alpha = emp[grep("alpha.p", emp$X), "Mean"],
  bias = emp[grep("bias.p", emp$X), "Mean"], #omit bias.mu
  time = emp[grep("time", emp$X), "Mean"][3:97] #omit time.mu, time.pr
)

sdf = data.frame(
  b = sim[grep("b.p", sim$X), "Mean"],
  d = sim[grep("d.p", sim$X), "Mean"],
  ndt = sim[grep("ndt.p", sim$X), "Mean"],
  alpha = sim[grep("alpha.p", sim$X), "Mean"],
  bias = sim[grep("bias.p", sim$X), "Mean"], #omit bias.mu
  time = sim[grep("time", sim$X), "Mean"][3:97] #omit time.mu, time.pr
)

cor(cbind(edf, sdf))[1:6, 7:12]

plot(edf$b, sdf$b)
abline(0, 1)

plot(edf$d, sdf$d)
abline(0, 1)

plot(edf$time, sdf$time)
abline(0, 1)

plot(edf$alpha, sdf$alpha)
abline(0, 1)

# plot(edf$b1-edf$b2, sdf$b1-sdf$b2)
# abline(0, 1)
