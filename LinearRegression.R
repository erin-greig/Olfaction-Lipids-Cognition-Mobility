#load in libraries
library(tidyverse)
library(data.table)

#import data
met_df <- fread("C:/file.csv")

#for models of mobility, use HtCm covariate instead of educat

#loop over all BV
run_lm <- function(i){
  print(colnames(met_df)[i])
  print(i)
  met_df$v1 <- scale((met_df[[i]]))
  met_df$CVLtca <- (scale(met_df$CVLtca))
  
  #build linear regression models
  reg <- lm(CVLtca ~ age + sex + black + v1 + educat, data = met_df, na.action=na.omit)
  
  #obtain coefficients from each model
  coef1 <- data.frame(coef(summary(reg)))
  coef2 <- data.frame(confint(reg, 'v1', level=0.95))
  
  #calculate n and # observations for each variable
  n <- length(unique(met_df$idno))
  obs <- length(met_df$idno)
  
  #defining the statistic coefficients like beta, SE, and p-value 
  #Pr(>F) = p-value for F statistic, null = 0
  results <- c(coef2[1,1], coef1[5,1], coef2[1,2], coef1[5,2],coef1[5,4], coef1[5,3],n, obs)
  
  results
  
}

#create dataframe for statistics#
n <- length(met_df[1,]) 
res1 <- data.frame((t(sapply(519:534,run_lm))))
names(res1) <- c("lower","b","upper","se","p", "t", "n", "obs") 
res1$cog<-colnames(met_df)[519:534]
write.csv(res1,"C:/file.csv", row.names=T)
