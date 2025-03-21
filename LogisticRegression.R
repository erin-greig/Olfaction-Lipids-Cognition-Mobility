#load in libraries
library(tidyverse)
library(data.table)

#import data
met_df <- fread("C:/file.csv")

#for models of mobility, use HtCm covariate instead of educat

#loop over all BV
run_lme <- function(i){
  print(colnames(met_df)[i])
  print(i)
  met_df$v1 <- (met_df[[i]])
  
  #build linear regression models
  reg <- glm(v1 ~ test + age + sex + black + smell + educat, data = met_df, na.action=na.omit, family='binomial')
  
  #obtain coefficients from each model
  coef1 <- data.frame(coef(summary(reg)))
  coef2 <- exp(cbind(Odds_Ratio = coef(reg), confint(reg)))
  
  #calculate n and # observations for each variable
  n <- length(unique(met_df$idno))
  obs <- length(met_df$idno)
  
  #defining the statistic coefficients like beta, SE, and p-value 
  #Pr(>F) = p-value for F statistic, null = 0
  results <- c(coef2[6,2], coef2[6,1], coef2[6,3], coef1[6,2],coef1[6,4], coef1[6,3],n, obs)
  
  results
  
}

#create dataframe for statistics#
n <- length(met_df[1,]) 
res1 <- data.frame((t(sapply(532:532,run_lme))))
names(res1) <- c("odds_lower","odds","odds_upper","se","p", "t", "n", "obs") 
res1$cog<-colnames(met_df)[532:532]

write.csv(res1,"C:/file.csv", row.names=T)

