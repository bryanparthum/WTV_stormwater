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
cmo <- as.data.table(read.dta13("data/chicago_money.dta"))
pmo <- as.data.table(read.dta13("data/portland_money.dta"))
mo  <- as.data.table(rbind(cmo,pmo))

## Trim to only necessary variables
mo  <- mo[, list(choice, fld_pctdec, hd_exc, pld_swm, money, c_id, id, chic_dum, alt)]

## Generate alternative specific constant
mo[, asc := as.numeric(alt==3)]

## Transform flood from percent
mo[, fld_pctdec := as.numeric(fld_pctdec*100)]
mo[, money := as.numeric(money)]

## Setkey and order
setkeyv(mo,c('chic_dum','id','c_id','alt'))

## Create mlogit data
d <-  mlogit.data(mo,
                   id.var='id',
                   chid.var = 'c_id',
                   choice='choice',
                   shape='long',
                   alt.var='alt',
                   opposite='money')

## Build parts to store Log-likelihood
scale <- as.numeric(NA)
LL    <- as.numeric(NA)
resid <- as.numeric(NA)
results <- data.table(scale,LL,resid)
s <- seq(0.025, 2, by=0.025)  

## Run model to get starting values
mod0 <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
                data=d,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T)
summary(mod0)
start <- coefficients(mod0) 
betas <-  as.data.table(as.list(start))[,1:5]
betas$scale <- NA
se <- as.data.table(as.list(sqrt(diag(vcov(mod0)))))[,1:5]
se$scale <- NA

## Iterate model, store scalar and likelihood
for (v in s) {
  scale <- v
  mo2 <- as.data.table(mo)
  mo2[, scale := scale]
  mo2[chic_dum==0, asc := asc*scale]
  mo2[chic_dum==0, fld_pctdec := fld_pctdec*scale]
  mo2[chic_dum==0, hd_exc := hd_exc*scale]
  mo2[chic_dum==0, pld_swm := pld_swm*scale ]
  mo2[chic_dum==0, money := money*scale]
  setkeyv(mo2,c('chic_dum','id','c_id','alt'))
  
  d <-  mlogit.data(mo2, 
                    id.var='id',
                    chid.var = 'c_id',
                    choice='choice',
                    shape='long',
                    alt.var='alt',
                    opposite='money')
  
mod <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
              data=d,
              rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
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

saveRDS(results,file="output/results_money.rds")
saveRDS(betas,file="output/betas_money.rds")
saveRDS(se,file="output/stderrors_money.rds")

####################################################
########################  CHICAGO VS PORTLAND - TIME
####################################################

## Read in files from generate.R
cti <- as.data.table(read.dta13("data/chicago_time.dta"))
pti <- as.data.table(read.dta13("data/portland_time.dta"))
ti  <- as.data.table(rbind(cti,pti))

## Trim to only necessary variables
ti  <- ti[, list(choice, fld_pctdec, hd_exc, pld_swm, time, c_id, id, chic_dum, alt)]

## Generate alternative specific constant
ti[, asc := as.numeric(alt==3)]

## Transform flood from percent
ti[, fld_pctdec := as.numeric(fld_pctdec*100)]
ti[, time := as.numeric(time)]

## Setkey and order
setkeyv(ti,c('chic_dum','id','c_id','alt'))

## Create mlogit data
d <-  mlogit.data(ti,
                  id.var='id',
                  chid.var = 'c_id',
                  choice='choice',
                  shape='long',
                  alt.var='alt',
                  opposite='time')

## Build parts to store Log-likelihood
scale <- as.numeric(NA)
LL    <- as.numeric(NA)
resid <- as.numeric(NA)
results <- data.table(scale,LL,resid)
s <- seq(0.025, 2, by=0.025)  

## Run model to get starting values
mod0 <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + time | 0 ,
                 data=d,
                 rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',time='ln'),
                 R=500,
                 halton=NA,
                 panel=T,
                 correlation=T)
summary(mod0)
start <- coefficients(mod0) 
betas <-  as.data.table(as.list(start))[,1:5]
betas$scale <- NA
se <- as.data.table(as.list(sqrt(diag(vcov(mod0)))))[,1:5]
se$scale <- NA

## Iterate model, store scalar and likelihood
for (v in s) {
  scale <- v
  ti2 <- as.data.table(ti)
  ti2[, scale := scale]
  ti2[chic_dum==0, asc := asc*scale]
  ti2[chic_dum==0, fld_pctdec := fld_pctdec*scale]
  ti2[chic_dum==0, hd_exc := hd_exc*scale]
  ti2[chic_dum==0, pld_swm := pld_swm*scale ]
  ti2[chic_dum==0, time := time*scale]
  setkeyv(ti2,c('chic_dum','id','c_id','alt'))
  
  d <-  mlogit.data(ti2, 
                    id.var='id',
                    chid.var = 'c_id',
                    choice='choice',
                    shape='long',
                    alt.var='alt',
                    opposite='time')
  
  mod <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + time | 0 ,
                  data=d,
                  rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',time='ln'),
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

results <- na.omit(results)
betas <- na.omit(betas)
se <- na.omit(se)

saveRDS(results,file="output/results_time.rds")
saveRDS(betas,file="output/betas_time.rds")
saveRDS(se,file="output/stderrors_time.rds")

## END OF SCRIPT. Have a great day! 