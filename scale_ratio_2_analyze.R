## Written by: Bryan Parthum; bparthum@gmail.com ; May 2019

###################################################################################
##########################     WILLINESS TO VOLUNTEER    ##########################
###################################################################################

####################################################
#####################################   PACKAGE SHOP
####################################################

# install.packages('mlogit')
# install.packages("viridis")
# install.packages("latex2exp")
# install.packages("tikzDevice")


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
packages <- c("gmnl","mlogit","memisc","data.table","stargazer","plotly","readstata13",'ggpubr','Cairo','viridis','latex2exp','ggplot2','lmtest','haven') ## you can add more packages here
lapply(packages, pkgTest)

####################################################
##########################################  PREAMBLE
####################################################

## Set working directory to location of this file
setwd("")

####################################################
#######################  CHICAGO VS PORTLAND - MONEY
####################################################

## READ RESULTS
results <- readRDS("output/results_money.rds")
betas   <- readRDS("output/betas_money.rds")
se      <- readRDS("output/stderrors_money.rds")

## FIND OPTIMAL SCALE RATIO
maxLL <- max(results$LL, na.rm=T)
scale <- results[LL==maxLL,]$scale
scale

## STORE FOR LATER
money.scale <- scale

## READ IN FILE
cmo <- as.data.table(read.dta13("data/chicago_money.dta"))
pmo <- as.data.table(read.dta13("data/portland_money.dta"))
mo  <- as.data.table(rbind(cmo,pmo))

## TRIM FAT
mo  <- mo[, list(choice, fld_pctdec, hd_exc, pld_swm, money, c_id, id, chic_dum, alt)]

## TRANSFORM THINGS
mo[, fld_pctdec := as.numeric(fld_pctdec*100)]
mo[, money := as.numeric(money)]

## GEN ASC
mo[, asc := as.numeric(alt==3)]
mo[, asc_chi := asc*chic_dum]
mo[, fld_pctdec_chi := fld_pctdec*chic_dum]
mo[, hd_exc_chi := hd_exc*chic_dum]
mo[, pld_swm_chi := pld_swm*chic_dum]
mo[, money_chi := money*chic_dum]

## RESCALE OTHER DATA
mo2 <- as.data.table(mo)
mo2[chic_dum==0, asc := asc*scale]
mo2[chic_dum==0, fld_pctdec := fld_pctdec*scale]
mo2[chic_dum==0, hd_exc := hd_exc*scale]
mo2[chic_dum==0, pld_swm := pld_swm*scale ]
mo2[chic_dum==0, money := money*scale]
write_dta(mo2,'output/scaled_money.dta', version=14)


## ORDER 
setkeyv(mo,c('chic_dum','id','c_id','alt'))
setkeyv(mo2,c('chic_dum','id','c_id','alt'))

## MLOGIT DATA
p <-  mlogit.data(mo,
                    id.var='id',
                    chid.var = 'c_id',
                    choice='choice',
                    shape='long',
                    alt.var='alt',
                    opposite='money')

ch <-  mlogit.data(mo[chic_dum==1,],
                  id.var='id',
                  chid.var = 'c_id',
                  choice='choice',
                  shape='long',
                  alt.var='alt',
                  opposite='money')

po <-  mlogit.data(mo[chic_dum==0,],
                    id.var='id',
                    chid.var = 'c_id',
                    choice='choice',
                    shape='long',
                    alt.var='alt',
                    opposite='money')

## MLOGIT DATA
scaled <-  mlogit.data(mo2,
                    id.var='id',
                    chid.var = 'c_id',
                    choice='choice',
                    shape='long',
                    alt.var='alt',
                    opposite='money')

## MODELS
chi <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
                 data=ch,
                 rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
                 R=500,
                 halton=NA,
                 panel=T,
                 correlation=T)

chi <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
                data=ch,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T,
                start=coef(chi))

por <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
                 data=po,
                 rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
                 R=500,
                 halton=NA,
                 panel=T,
                 correlation=T)

por <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
                data=po,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T,
                start=coef(por))

start <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
                data=p,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T)

poo <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
                   data=p,
                   rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
                   R=500,
                   halton=NA,
                   panel=T,
                   correlation=T,
                   start = coef(start))

sca <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + money | 0 ,
                data=scaled,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',money='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T,
                start = coef(poo))

saveRDS(chi,file="output/chicago_money_mixl.rds")
saveRDS(por,file="output/portland_money_mixl.rds")
saveRDS(poo,file="output/pooled_money_mixl.rds")
saveRDS(sca,file="output/scaled_money_mixl.rds")

####################################################
########################  CHICAGO VS PORTLAND - TIME
####################################################

## READ RESULTS
results <- readRDS("output/results_time.rds")
betas   <- readRDS("output/betas_time.rds")
se      <- readRDS("output/stderrors_time.rds")

## FIND OPTIMAL SCALE RATIO
maxLL <- max(results$LL, na.rm=T)
scale <- results[LL==maxLL,]$scale
scale 

## STORE FOR LATER
time.scale <- scale
time.scale

## READ IN FILE
cti <- as.data.table(read.dta13("data/chicago_time.dta"))
pti <- as.data.table(read.dta13("data/portland_time.dta"))
ti  <- as.data.table(rbind(cti,pti))

## TRIM FAT
ti  <- ti[, list(choice, fld_pctdec, hd_exc, pld_swm, time, c_id, id, chic_dum, alt)]

## GEN ASC
ti[, asc := as.numeric(alt==3)]

## TRANSFORM THINGS
ti[, fld_pctdec := as.numeric(fld_pctdec*100)]
ti[, time := as.numeric(time)]

## RESCALE OTHER DATA
ti2 <- as.data.table(ti)
ti2[chic_dum==0, asc := asc*scale]
ti2[chic_dum==0, fld_pctdec := fld_pctdec*scale]
ti2[chic_dum==0, hd_exc := hd_exc*scale]
ti2[chic_dum==0, pld_swm := pld_swm*scale ]
ti2[chic_dum==0, time := time*scale]
write_dta(ti2,'output/scaled_time.dta', version=14)

## ORDER 
setkeyv(ti,c('chic_dum','id','c_id','alt'))
setkeyv(ti2,c('chic_dum','id','c_id','alt'))

## MLOGIT DATA
p <-  mlogit.data(ti,
                   id.var='id',
                   chid.var = 'c_id',
                   choice='choice',
                   shape='long',
                   alt.var='alt',
                   opposite='time')

ch <-  mlogit.data(ti[chic_dum==1,],
                    id.var='id',
                    chid.var = 'c_id',
                    choice='choice',
                    shape='long',
                    alt.var='alt',
                    opposite='time')

po <-  mlogit.data(ti[chic_dum==0,],
                    id.var='id',
                    chid.var = 'c_id',
                    choice='choice',
                    shape='long',
                    alt.var='alt',
                    opposite='time')

## MLOGIT DATA
scaled <-  mlogit.data(ti2,
                       id.var='id',
                       chid.var = 'c_id',
                       choice='choice',
                       shape='long',
                       alt.var='alt',
                       opposite='time')

## MODELS
chi <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + time | 0 ,
                data=ch,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',time='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T,
                start=coef(chi))

por <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + time | 0 ,
                data=po,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',time='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T,
                start=coef(por))

start <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + time | 0 ,
                data=p,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',time='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T)

poo <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + time | 0 ,
                data=p,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',time='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T, 
                start = coef(start))

sca <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + time | 0 ,
                data=scaled,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',time='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T,
                start = coef(poo))

saveRDS(chi,file="output/chicago_time_mixl.rds")
saveRDS(por,file="output/portland_time_mixl.rds")
saveRDS(poo,file="output/pooled_time_mixl.rds")
saveRDS(sca,file="output/scaled_time_mixl.rds")

####################################################
################################### HYPOTHESIS TESTS
####################################################

############################
###############   READ FILES
############################

## READ RESULTS
mo.results <- readRDS("output/results_money.rds")
mo.betas   <- readRDS("output/betas_money.rds")
mo.se      <- readRDS("output/stderrors_money.rds")

ti.results <- readRDS("output/results_time.rds")
ti.betas   <- readRDS("output/betas_time.rds")
ti.se      <- readRDS("output/stderrors_time.rds")

chi.mo <- readRDS("output/chicago_money_mixl.rds")
por.mo <- readRDS("output/portland_money_mixl.rds")
pooled.mo <- readRDS("output/pooled_money_mixl.rds")
scaled.mo <- readRDS("output/scaled_money_mixl.rds")

chi.ti <- readRDS("output/chicago_time_mixl.rds")
por.ti <- readRDS("output/portland_time_mixl.rds")
pooled.ti <- readRDS("output/pooled_time_mixl.rds")
scaled.ti <- readRDS("output/scaled_time_mixl.rds")

############################
###############  MONEY TESTS
############################

## TESTS
chi.ll.money <- as.numeric(logLik(chi.mo))
por.ll.money <- as.numeric(logLik(por.mo))
poo.ll.money <- as.numeric(logLik(pooled.mo))
sca.ll.money <- as.numeric(logLik(scaled.mo))

## H_0: EQUAL PARAMETERS
test <- 2*((chi.ll.money + por.ll.money) - sca.ll.money)
crit <- qchisq(p = 0.95, df = 46)
money.equalB  <- test > crit
money.pvalueB <- pchisq(test, 46,lower.tail=F)
money.testB   <- test 

## H_0: EQUAL SCALE 
test <- 2*(sca.ll.money - poo.ll.money)
crit <- qchisq(p = 0.95, df = 1)
money.equalM  <- test > crit
money.pvalueM <- pchisq(test, 1,lower.tail=F)
money.testM   <- test 

############################
###############   TIME TESTS
############################

## TESTS
chi.ll.time <- as.numeric(logLik(chi.ti))
por.ll.time <- as.numeric(logLik(por.ti))
poo.ll.time <- as.numeric(logLik(pooled.ti))
sca.ll.time <- as.numeric(logLik(scaled.ti))

## H_0: EQUAL PARAMETERS
test <- 2*((chi.ll.time + por.ll.time) - sca.ll.time)
crit <- qchisq(p = 0.95, df = 46)
time.equalB <- test > crit
time.pvalueB <- pchisq(test, 46,lower.tail=F)
time.testB <- test 
critB <- crit

## H_0: EQUAL SCALE 
test <- 2*(sca.ll.time - poo.ll.time)
crit <- qchisq(p = 0.95, df = 1)
time.equalM <- test > crit
time.pvalueM <- pchisq(test, 1,lower.tail=F)
critM <- crit
time.testM <- test

############################
###############  PRINT TESTS
############################

## LL - MONEY
chi.ll.money
por.ll.money
poo.ll.money
sca.ll.money

## LL - TIME
chi.ll.time
por.ll.time
poo.ll.time
sca.ll.time

## RELATIVE SCALE 
money.scale 
time.scale

## TEST STATISTICS
critB
critM
money.testB
money.testM
money.pvalueB
money.pvalueM
time.testB
time.testM
time.pvalueB
time.pvalueM

## HYPOTHESES
money.equalB
money.equalM
time.equalB
time.equalM

##########################################
###########################   PLOT - MONEY
##########################################

## READ IN RESULTS
results <- as.data.table(readRDS("output/results_money.rds"))
results[, color := 'Ha',]

## CREATE SCALAR
maxLL <- max(results$LL, na.rm=T)
scalar <- results[LL==maxLL,]$scale
results[, scalar := scalar,]

## HORIZONTAL LINES
results[, testline := (chi.ll.money + por.ll.money),]
results[, testline_color := 'Chicago + Portland',]
results[, pooled := poo.ll.money - money.testM/2,]
results[, pooled_color := 'Pooled, Scale = 1',]
results[, scaled := sca.ll.money,]
results$testline
## TEST VALUES
crit1 <- qchisq(p = 0.95, df = 46)/2
crit1

## PLOT 
money <- ggplot(results) + 
  geom_line(aes(x=scale, y=LL, linetype = "solid"), size=0.75) + 
  geom_vline(aes(xintercept=scalar),linetype = "solid", size=0.75) + 
  geom_vline(aes(xintercept=1),linetype = "dashed", size=0.75) +
  geom_line(aes(x=scale, y=testline, linetype = "twodash"), size=0.75) + 
  geom_ribbon(aes(x=scale, ymin = LL - crit1, ymax = LL + crit1), fill = "grey70", color="grey70",alpha = 0.3,linetype = "dashed") +
  scale_y_continuous(breaks = seq(-2000,-1500,100),
                     limits=c(-2000,-1550),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0,2.1,by=0.2),
                     expand = c(0,0)) + 
  scale_linetype_discrete(labels = unname(TeX(c("$LL_\\sigma$","$(LL_R + LL_U)$")))) +
  annotate("text",x = .33, y = -1608,
           label=TeX("$(LL_C + LL_P)= \ -1619.28$", output = "character"),
           hjust=0, parse=T) +
  annotate("text",x = 1.1, y = -1975,
           label=TeX("$\\sigma =1.05$", output = "character"),
           hjust=0, parse=T) +
labs(x="Relative Scale Factor",
     y="Log-likelihood",
     linetype="") +
  theme_bw() +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        legend.position = 'bottom',
        legend.text=element_text(size=16))
money
ggsave(money,filename="plots/scale_money.png",h=5,w=8,type="cairo-png")

##########################################
###########################   PLOT - MONEY
##########################################

## READ IN RESULTS
results <- as.data.table(readRDS("output/results_time.rds"))
results[, color := 'Ha',]

## CREATE SCALAR
maxLL <- max(results$LL, na.rm=T)
scalar <- results[LL==maxLL,]$scale
results[, scalar := scalar,]

## HORIZONTAL LINES
results[, testline := (chi.ll.time + por.ll.time),]
results[, testline_color := 'Chicago + Portland',]
results[, pooled := poo.ll.time - time.testM/2,]
results[, pooled_color := 'Pooled, Scale = 1',]
results[, scaled := sca.ll.time,]
results$testline
## TEST VALUES
crit1 <- qchisq(p = 0.95, df = 46)/2
crit1
crit2 <- qchisq(p = 0.95, df = 1)/2
crit2

## PLOT 
time <- ggplot(results) + 
  geom_line(aes(x=scale, y=LL, linetype = "solid"), size=0.75) + 
  geom_vline(aes(xintercept=scalar),linetype = "solid", size=0.75) + 
  geom_vline(aes(xintercept=1),linetype = "dashed", size=0.75) +
  geom_line(aes(x=scale, y=testline, linetype = "twodash"), size=0.75) + 
  geom_ribbon(aes(x=scale, ymin = LL - crit1, ymax = LL + crit1), fill = "grey70", color="grey70",alpha = 0.3,linetype = "dashed") +
  scale_linetype_discrete(labels = unname(TeX(c("$LL_\\sigma$","$(LL_R + LL_U)$")))) +
  scale_y_continuous(breaks = seq(-2000,-1500,100),
                     limits=c(-2000,-1550),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0,2.1,by=0.2),
                     expand = c(0,0)) + 
  scale_color_viridis(discrete=T,alpha=0.6) +
  annotate("text",x = .33, y = -1605,
           label=TeX("$(LL_C + LL_P)= \ -1615.70$", output = "character"),
           hjust=0, parse=T) +
  annotate("text",x = 1.35, y = -1975,
           label=TeX("$\\sigma =1.30$", output = "character"),
           hjust=0, parse=T) +
  labs(x="Relative Scale Factor",
       y="Log-likelihood",
       linetype="") +
  theme_bw() +
  theme(panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        legend.position = 'bottom',
        legend.text=element_text(size=16))
time
ggsave(time,filename="plots/scale_time.png",h=5,w=8,type="cairo-png")

## END OF SCRIPT. Have a nice day! 