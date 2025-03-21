#load in libraries
library(mediation)
library(tidyverse)
library(data.table)

#import data
met_df <- fread("C:/file.csv")

#MMSTot
# empty data frame object to fill with results
mediation_results = NULL
n <- length(met_df[1,]) 
MMSTot <- subset(met_df, !is.na(MMSTot))

#for mediation models of mobility, use HtCm covariate instead of educat

#mediation
for(i in 488:503){
  #define exposure, mediator, and outcome
  MMSTot$X <- MMSTot$smell
  MMSTot$M <- scale(MMSTot[,i])
  MMSTot$Y <- scale(MMSTot$MMSTot)
  mediator_lab = colnames(MMSTot)[i]
  
  model.M <- lm(M ~ X + black + age + test + sex + educat, data = MMSTot)
  model.Y <- lm(Y ~ X + M + black + age + test + sex + educat, data = MMSTot)
  
  model = mediate(model.M, model.Y, treat="X", mediator="M", covariates = c("black", "age", "test", "sex", "educat"), sims= 500, conf.level=0.95, robustSE=F, data = MMSTot)
  
  # extract and format results
  b_med    = as.vector(model$d.avg)
  b_dir    = as.vector(model$z.avg)
  b_tot    = as.vector(model$tau.coef)
  
  lci_med  = as.vector(model$d.avg.ci[1])
  uci_med  = as.vector(model$d.avg.ci[2])
  p_med    = as.vector(model$d.avg.p)
  
  lci_dir  = as.vector(model$z.avg.ci[1])
  uci_dir  = as.vector(model$z.avg.ci[2])
  p_dir    = as.vector(model$z.avg.p)
  
  lci_tot  = as.vector(model$tau.ci[1])
  uci_tot  = as.vector(model$tau.ci[2])
  p_tot    = as.vector(model$tau.p)
  
  prop_tot_med     = as.vector(model$n.avg)
  prop_tot_med_abs = sqrt(prop_tot_med * prop_tot_med)
  prop_tot_med_lse = as.vector(model$n.avg.ci[1])
  prop_tot_med_use = as.vector(model$n.avg.ci[2])
  p_prop_tot_med   = as.vector(model$n.avg.p)
  
  Nobs     = as.vector(model$nobs)
  Nsims    = as.vector(model$sims)
  
  ## combine into single row
  newRow   = data.frame(Mediator=mediator_lab, TotEff=b_tot, TotEff_lower_CI=lci_tot, TotEff_upper_CI=uci_tot, TotEff_p=p_tot, 
                        DirEff=b_dir, DirEff_lower_CI=lci_dir, DirEff_upper_CI=uci_dir, DirEff_p=p_dir,
                        MedEff=b_med, MedEff_lower_CI=lci_med, MedEff_upper_CI=uci_med, MedEff_p=p_med, 
                        PropEff=prop_tot_med, PropEff_lower_CI=prop_tot_med_lse, PropEff_upper_CI=prop_tot_med_use, PropEff_p=p_prop_tot_med, PropEffAbs=prop_tot_med_abs,
                        Nobs=Nobs, Sims=Nsims)
  
  ## merge with full data frame
  mediation_results = data.frame(rbind(mediation_results, newRow))
  #p-value = 0 = <2e-16
}
#save output
write.csv(mediation_results, "C:/file.csv", row.names=T)

#repeat for all cognitive and mobility outcomes of interest
