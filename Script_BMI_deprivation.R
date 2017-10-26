# library(foreign) ##Can read .dta files from Stata 5-12
# library(readstata13)
library(dplyr)
library(R2WinBUGS)

setwd("D:/Dropbox/MUN projects/Saskatchewan_deprivation+obesity/")

sas_depri <- read.csv("COS_Person_HH_Trip_Nhbd_Data_2017.csv")
final_dat <- filter(sas_depri,is.na(genderID)==F & is.na(agecat)==F & is.na(transport_cat)==F& is.na(incomecat)==F & is.na(hhChildren)==F & is.na(fitnesscat)==F & is.na(totaldeplo)==F & is.na(PopnDensity)==F& is.na(road_centr)==F & is.na(BMI)==F)

sink("model.txt")
cat("
    ##Individual covariates: gender(1/2), age group(0,1,2,3), transport category (1,2,3), fitness category(0/1)
##Household covariates: household income (1,2,3,4); presence of young children(0,1)
##DA-level covariates: deprivation (1,2,3,4,5); population density; road centroids
    
    model{
    for(i in 1:NPerson)
    {
    Y[i] ~ dnorm(mean[i],prec.u1)
    # GENDER2[i] <- equals(gender[i],2)
    # AGE2[i] <- equals(age[i],1)
    # AGE3[i] <- equals(age[i],2)
    # AGE4[i] <- equals(age[i],3)
    # TRANS_CAT1[i] <- equals(trans_cat[i],1)
    # TRANS_CAT2[i] <- equals(trans_cat[i],2)
    # TRANS_CAT3[i] <- equals(trans_cat[i],3)
    # INCOME1[i] <- equals(income[i],1)
    # INCOME2[i] <- equals(income[i],2)
    # INCOME3[i] <- equals(income[i],3)
    # INCOME4[i] <- equals(income[i],4)
    # DEPRI2[i] <- equals(depri[i],2)
    # DEPRI3[i] <- equals(depri[i],3)
    # DEPRI4[i] <- equals(depri[i],4)
    # DEPRI5[i] <- equals(depri[i],5)
    
    mean[i] <- alpha + beta[1]*GENDER2[i] + beta[2]*AGE2[i] + beta[3]*AGE3[i] + beta[4]*AGE4[i] + beta[5]*TRANS_CAT2[i] + beta[6]*TRANS_CAT3[i] + beta[7]*fit_cat[i] + beta[8]*INCOME2[i] + beta[9]*INCOME3[i] + beta[10]*INCOME4[i] + beta[11]*child[i] + beta[12]*DEPRI2[i] + beta[13]*DEPRI3[i] + beta[14]*DEPRI4[i] + beta[15]*DEPRI5[i] +beta[16]*roadCen[i] + beta[17]*popDen[i] + u2[HHID[i]] + u3[DAUID[i]]
# inter[i] <- beta[18]*INTER22[i] + beta[19]*INTER23[i] + beta[20]*INTER24[i] + beta[21]*INTER32[i] + beta[22]*INTER33[i] + beta[23]*INTER34[i]
    }
    
    prec.u1 <- pow(sd.u1,-2)
    sd.u1 ~ dunif(0,100)
    
    ##Household-level random effects
    
    for(i in 1:hhNum)
    {
      u2[i] ~ dnorm(0,prec.u2)
    }
    prec.u2 <- pow(sd.u2,-2)
    sd.u2 ~ dunif(0,100)
    
    alpha ~ dflat()
    
    ##Prior of coefficients
    for(j in 1:betaNum)
    {
      beta[j] ~ dnorm(0,0.001)
    }
    
    for(i in 1:daNum)
    {
      u3[i] ~ dnorm(0,prec.u3)
    }
    prec.u3 <- pow(sd.u3,-2)
    sd.u3 ~ dunif(0,100)
    
    }
    ", fill=TRUE)
sink()

NPerson <- 4625 ## Number of samples
betaNum <- 17 ## Number of covariates
hhNum <- 2726 ## Number of households
daNum <- 330 ## Number of DAs

##Dependent and independent variables
Y <- log(final_dat$BMI)
gender <- final_dat$genderID
age <- final_dat$agecat
trans_cat <- final_dat$transport_cat
fit_cat <- final_dat$fitnesscat
income <- final_dat$incomecat
child <- final_dat$hhChildren
depri <- final_dat$totaldeplo

##DA-level independent variables
uniqueDA <- distinct(final_dat,dauid,.keep_all = T)
uniqueDA$sd_roadCen <- as.vector(t(scale(uniqueDA$road_centr,T,T)))
uniqueDA$sd_popDen <- as.vector(t(scale(uniqueDA$PopnDensity,T,T)))
uniqueDA$POLYID <- seq(1,length(uniqueDA$dauid),1)

kept_variables <- select(uniqueDA,dauid,sd_popDen,sd_roadCen,POLYID)
final_dat <- left_join(final_dat,kept_variables,"dauid")

##Add a continuous HHID to the final_dat
uniqueHH <- distinct(select(final_dat,revHHID))
uniqueHH$HHID <- seq(1,length(uniqueHH$revHHID),1)
final_dat <- left_join(final_dat,uniqueHH,"revHHID")

popDen <- final_dat$sd_popDen
roadCen <- final_dat$sd_roadCen
HHID <- final_dat$HHID
DAUID <- final_dat$POLYID

GENDER2 <- unlist(lapply(final_dat$genderID,function(x) ifelse(x==2,1,0)))
AGE2 <- unlist(lapply(final_dat$agecat,function(x) ifelse(x==1,1,0)))
AGE3<- unlist(lapply(final_dat$agecat,function(x) ifelse(x==2,1,0)))
AGE4 <- unlist(lapply(final_dat$agecat,function(x) ifelse(x==3,1,0)))
TRANS_CAT2 <- unlist(lapply(final_dat$transport_cat,function(x) ifelse(x==2,1,0)))
TRANS_CAT3 <- unlist(lapply(final_dat$transport_cat,function(x) ifelse(x==3,1,0)))
INCOME2 <- unlist(lapply(final_dat$incomecat,function(x) ifelse(x==2,1,0)))
INCOME3 <- unlist(lapply(final_dat$incomecat,function(x) ifelse(x==3,1,0)))
INCOME4 <- unlist(lapply(final_dat$incomecat,function(x) ifelse(x==4,1,0)))
DEPRI2 <- unlist(lapply(final_dat$totaldeplo,function(x) ifelse(x==2,1,0)))
DEPRI3 <- unlist(lapply(final_dat$totaldeplo,function(x) ifelse(x==3,1,0)))
DEPRI4 <- unlist(lapply(final_dat$totaldeplo,function(x) ifelse(x==4,1,0)))
DEPRI5 <- unlist(lapply(final_dat$totaldeplo,function(x) ifelse(x==5,1,0)))

# INTER22 <- TRANS_CAT2*INCOME2
# INTER23 <- TRANS_CAT2*INCOME3
# INTER24 <- TRANS_CAT2*INCOME4
# INTER32 <- TRANS_CAT3*INCOME2
# INTER33 <- TRANS_CAT3*INCOME3
# INTER34 <- TRANS_CAT3*INCOME4

data.sim <- list("Y","NPerson","betaNum","hhNum","daNum", "fit_cat","child","popDen","roadCen","HHID","DAUID","GENDER2","AGE2","AGE3","AGE4","TRANS_CAT2","TRANS_CAT3","INCOME2","INCOME3","INCOME4","DEPRI2","DEPRI3","DEPRI4","DEPRI5")
# ,"INTER22","INTER23","INTER24","INTER32","INTER33","INTER34"

parameters_monitor <- c("alpha","beta","sd.u1","sd.u2","sd.u3","u2[1:3]","u3[1:3]")

init1 <- list(alpha=3,beta=rep(0.2,betaNum),sd.u1=1,sd.u2=0.1,sd.u3=1,u2=rep(0.1,hhNum),u3=rep(0.1,daNum))
init2 <- list(alpha=-3,beta=rep(-0.2,betaNum),sd.u1=0.1,sd.u2=1,sd.u3=0.1,u2=rep(0,hhNum),u3=rep(0.2,daNum))

inits.sim <- list(init1,init2)

sim <- bugs(data = data.sim, inits = inits.sim, parameters.to.save = parameters_monitor, model.file = "model.txt", n.chains=2, n.iter=10000, n.burnin=0, n.thin=1, codaPkg = FALSE, debug=TRUE, DIC=FALSE, bugs.directory = "C:/Program Files (x86)/WinBUGS14", working.directory=getwd(), save.history=FALSE)

# filter_dat <- filter(final_dat,final_dat$incomecat==3)
# mean(filter_dat$BMI)
# sd(filter_dat$BMI)
# filter_dat1 <- filter(filter_dat,percentactive==0)
# filter_dat2 <- filter(filter_dat,percentactive<1 & percentactive>0)
# filter_dat3 <- filter(filter_dat,percentactive==1)
# dim(filter_dat1)[1]/dim(filter_dat)[1]
# dim(filter_dat2)[1]/dim(filter_dat)[1]
# dim(filter_dat3)[1]/dim(filter_dat)[1]
# dim(filter_dat)[1]
# dim(filter_dat)[1]/dim(final_dat)[1]
summary(lm(Y~AGE2+AGE3+AGE4))
summary(lm(Y~fit_cat))
summary(lm(Y~TRANS_CAT2+TRANS_CAT3))
summary(lm(Y~INCOME2+INCOME3+INCOME4))
summary(lm(Y~child))
summary(lm(Y~DEPRI2+DEPRI3+DEPRI4+DEPRI5))
summary(lm(Y~popDen))
summary(lm(Y~roadCen))

