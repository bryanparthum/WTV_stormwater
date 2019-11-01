## Written by: Bryan Parthum; bparthum@gmail.com ; May 2019

###################################################################################
##########################     WILLINESS TO VOLUNTEER    ##########################
###################################################################################

####################################################
#####################################   PACKAGE SHOP
####################################################

# install.packages('mlogit')

####################################################
########   Check for and load Packages   ###########
####################################################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = T))
  {
    install.packages(x, dep = T)
    if(!require(x, character.only = T)) stop("Package not found")
  }
}

## These lines load the required packages
packages <- c("gmnl","mlogit","memisc","data.table","stargazer","plotly","readstata13") ## you can add more packages here
lapply(packages, pkgTest)

####################################################
#####################   Load and generate dataframes    
####################################################

## Set working directory to location of this file
setwd("")

####################################################
#######################  CHICAGO VS PORTLAND - MONEY
####################################################

## Read in files from generate.R
moti<- as.data.table(read.dta13("data/pooled_money_time.dta"))

## Trim to only necessary variables
moti  <- moti[wage>=8 & wage<500 & !is.na(wage), list(choice, fld_pctdec, hd_exc, pld_swm, money, time, c_id, id, chic_dum, alt, time_survey, wage)]

## Generate alternative specific constant
moti[, asc := as.numeric(alt==3)]

## Transform flood from percent
moti[, fld_pctdec := as.numeric(fld_pctdec*100)]
moti[, money := as.numeric(money)]
moti[, time := as.numeric(time)]
moti[, wage_time := 1/3*wage*time]
moti[time_survey==0, cost := money]
moti[time_survey==1, cost := wage_time]

## Setkey and order
setkeyv(moti,c('time_survey','id','c_id','alt'))

## Create mlogit data
d <-  mlogit.data(moti,
                  id.var='id',
                  chid.var = 'c_id',
                  choice='choice',
                  shape='long',
                  alt.var='alt',
                  opposite='cost')

## Build parts to store Log-likelihood
scale <- as.numeric(NA)
LL    <- as.numeric(NA)
resid <- as.numeric(NA)
results <- data.table(scale,LL,resid)
s <- seq(0.025, 2, by=0.025)  

## Run model to get starting values
mod0 <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + cost | 0 ,
                 data=d,
                 rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',cost='ln'),
                 R=500,
                 halton=NA,
                 panel=T,
                 correlation=T)
summary(mod0)
start <- coef(mod0) 
betas <-  as.data.table(as.list(start))[,1:5]
betas$scale <- NA
se <- as.data.table(as.list(sqrt(diag(vcov(mod0)))))[,1:5]
se$scale <- NA

## Iterate model, store scalar and likelihood
for (v in s) {
  scale <- v
  mo2 <- as.data.table(moti)
  mo2[, scale := scale]
  mo2[time_survey==1, asc := asc*scale]
  mo2[time_survey==1, fld_pctdec := fld_pctdec*scale]
  mo2[time_survey==1, hd_exc := hd_exc*scale]
  mo2[time_survey==1, pld_swm := pld_swm*scale ]
  mo2[time_survey==1, cost := cost*scale]
  setkeyv(mo2,c('time_survey','id','c_id','alt'))
  
  d <-  mlogit.data(mo2, 
                    id.var='id',
                    chid.var = 'c_id',
                    choice='choice',
                    shape='long',
                    alt.var='alt',
                    opposite='cost')
  
  mod <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + cost | 0 ,
                  data=d,
                  rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',cost='ln'),
                  R=500,
                  halton=NA,
                  panel=T,
                  correlation=T,
                  start = start)
  
  ## Parts
  b <- as.data.table(as.list(coefficients(mod)))[,1:5]
  b$scale <- scale
  betas <- rbind(betas,b)
  
  s <- as.data.table(as.list(sqrt(diag(vcov(mod)))))[,1:5]
  s$scale <- scale
  se <- rbind(se,s)
  
  LL <- as.numeric(logLik(mod))
  resid <- mean(residuals(mod))
  t <- data.table(scale,LL,resid)
  results <- rbind(results,t, fill=T)
}

## FIND OPTIMAL SCALE RATIO
maxLL <- max(results$LL, na.rm=T)
scale_opt <- results[LL==maxLL,]$scale
maxLL
scale_opt

results <- na.omit(results)
betas <- na.omit(betas)
se <- na.omit(se)

saveRDS(results,file="output/results_money_time.rds")
saveRDS(betas,file="output/betas_money_time.rds")
saveRDS(se,file="output/stderrors_money_time.rds")

## END OF SCRIPT. Have a great day! 