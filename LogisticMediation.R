#load in libraries
library(tidyverse)
library(data.table)
library(regmedint)
library(sas7bdat)

#import data
met_df <- fread("C:/file.csv")

# empty data frame object to fill with results
mediation_results = NULL
n <- length(met_df[1,]) 

#for mediation models of mobility, use HtCm covariate instead of educat

for(i in 514:529){
  #define exposure, mediator, and outcome
  df$M <- scale(df[,i])
  mediator_lab = colnames(df)[i]
  logistic_mediate <- regmedint(data = df, #data
                                yvar = "impair", #outcome variable
                                avar = "smell", #treatment variable/predictor variable
                                mvar = "M", #mediator variable
                                cvar = c("black", "age", "test", "sex", "educat", "abVisc"), #covariate names, should be column names from dataframe
                                a0 = 0, #reference level – basically the unit change between a0 and a1 will account for the effect size calculated for the models
                                a1 = 1,  #non-reference level
                                m_cde = 1, #mediator variable level
                                c_cond = c(mean(df$black),mean(df$age),mean(df$test),
                                           mean(df$sex), mean(df$educat), mean(df$abVisc)), #levels at which to evaluate covariates – must be the same length as cvar
                                mreg = "linear", #type of model between the predictor and the mediator
                                yreg = "logistic", #type of model between the predictor/mediator and the outcome
                                interaction = FALSE, #if you want to test for interaction
                                casecontrol = FALSE) #if it’s a case-control study
  
  pureNaturalDirectEffect_estimate <- coef(summary(logistic_mediate))[2,1]
  pureNaturalDirectEffect_ci_low <- coef(summary(logistic_mediate))[2,5]
  pureNaturalDirectEffect_ci_high <- coef(summary(logistic_mediate))[2,6]
  pureNaturalDirectEffect_p <- coef(summary(logistic_mediate))[2,4]
  
  totalNaturalIndirectEffect_estimate <- coef(summary(logistic_mediate))[3,1]
  totalNaturalIndirectEffect_ci_low <- coef(summary(logistic_mediate))[3,5]
  totalNaturalIndirectEffect_ci_high <- coef(summary(logistic_mediate))[3,6]
  totalNaturalIndirectEffect_p <- coef(summary(logistic_mediate))[3,4]
  
  totalEffect_estimate <- coef(summary(logistic_mediate))[6,1]
  totalEffect_ci_low <- coef(summary(logistic_mediate))[6,5]
  totalEffect_ci_high <- coef(summary(logistic_mediate))[6,6]
  totalEffect_p <- coef(summary(logistic_mediate))[6,4]
  
  proportionMediated_estimate <- coef(summary(logistic_mediate))[7,1]
  proportionMediated_ci_low <- coef(summary(logistic_mediate))[7,5]
  proportionMediated_ci_high <- coef(summary(logistic_mediate))[7,6]
  proportionMediated_p <- coef(summary(logistic_mediate))[7,4]
  
  
  ## combine into single row
  newRow   = data.frame(Mediator=mediator_lab, TotEff=totalEffect_estimate, TotEff_lower_CI=totalEffect_ci_low, TotEff_upper_CI=totalEffect_ci_high, TotEff_p=totalEffect_p, 
                        DirEff=pureNaturalDirectEffect_estimate, DirEff_lower_CI=pureNaturalDirectEffect_ci_low, DirEff_upper_CI=pureNaturalDirectEffect_ci_high, DirEff_p=pureNaturalDirectEffect_p,
                        MedEff=totalNaturalIndirectEffect_estimate, MedEff_lower_CI=totalNaturalIndirectEffect_ci_low, MedEff_upper_CI=totalNaturalIndirectEffect_ci_high, MedEff_p=totalNaturalIndirectEffect_p, 
                        PropEff=proportionMediated_estimate, PropEff_lower_CI=proportionMediated_ci_low, PropEff_upper_CI=proportionMediated_ci_high, PropEff_p=proportionMediated_p)
  
  ## merge with full data frame
  mediation_results = data.frame(rbind(mediation_results, newRow))
  #p-value = 0 = <2e-16
}

#save output
write.csv(mediation_results, "C:/file.csv", row.names=T)
