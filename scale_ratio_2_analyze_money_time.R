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

## Set working directory to the location of this file
setwd("")

####################################################
#######################  CHICAGO VS PORTLAND - MONEY
####################################################

## READ RESULTS
results <- readRDS("output/results_money_time.rds")
betas   <- readRDS("output/betas_money_time.rds")
se      <- readRDS("output/stderrors_money_time.rds")

## FIND OPTIMAL SCALE RATIO
maxLL <- max(results$LL, na.rm=T)
scale.ratio <- results[LL==maxLL,]$scale
maxLL
scale.ratio

## READ IN FILE
moti<- as.data.table(read.dta13("data/pooled_money_time.dta"))

## TRIM FAT
moti  <- moti[wage>=8 & wage<500 & !is.na(wage), list(choice, fld_pctdec, hd_exc, pld_swm, money, time, c_id, id, chic_dum, alt, time_survey, wage)]

## Generate alternative specific constant
moti[, asc := as.numeric(alt==3)]

## Transform flood from percent
moti[, fld_pctdec := as.numeric(fld_pctdec*100)]
moti[, money := as.numeric(money)]
moti[, time := as.numeric(time)]
moti[, wage_time := 1/3*wage*time]
moti[time_survey==0, cost := money]
moti[time_survey==1, cost := time]

mo2 <- as.data.table(moti)
mo2[, scale := scale.ratio]
mo2[time_survey==1, asc := asc*scale]
mo2[time_survey==1, fld_pctdec := fld_pctdec*scale]
mo2[time_survey==1, hd_exc := hd_exc*scale]
mo2[time_survey==1, pld_swm := pld_swm*scale ]
mo2[time_survey==1, cost := cost*scale]
setkeyv(mo2,c('time_survey','id','c_id','alt'))

## Setkey and order
setkeyv(moti,c('time_survey','id','c_id','alt'))
write_dta(mo2,'output/scaled_money_time.dta', version=14)

## MLOGIT DATA
p <-  mlogit.data(moti,
                  id.var='id',
                  chid.var = 'c_id',
                  choice='choice',
                  shape='long',
                  alt.var='alt',
                  opposite='cost')

ti <-  mlogit.data(moti[time_survey==1,],
                   id.var='id',
                   chid.var = 'c_id',
                   choice='choice',
                   shape='long',
                   alt.var='alt',
                   opposite='cost')

mo <-  mlogit.data(moti[time_survey==0,],
                   id.var='id',
                   chid.var = 'c_id',
                   choice='choice',
                   shape='long',
                   alt.var='alt',
                   opposite='cost')

## MLOGIT DATA
scaled <-  mlogit.data(mo2,
                       id.var='id',
                       chid.var = 'c_id',
                       choice='choice',
                       shape='long',
                       alt.var='alt',
                       opposite='cost')

## MODELS
mon <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + cost | 0 ,
                data=mo,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',cost='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T)

tim <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + cost | 0 ,
                data=ti,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',cost='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T)

start <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + cost | 0 ,
                  data=p,
                  rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',cost='ln'),
                  R=500,
                  halton=NA,
                  panel=T,
                  correlation=T)

poo <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + cost | 0 ,
                data=p,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',cost='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T,
                start = coef(start))

sca <-   mlogit(choice ~ asc + fld_pctdec + hd_exc + pld_swm + cost | 0 ,
                data=scaled,
                rpar=c(asc='n',fld_pctdec='n',hd_exc='n',pld_swm='n',cost='ln'),
                R=500,
                halton=NA,
                panel=T,
                correlation=T,
                start = coef(start))

saveRDS(mon,file="output/money_money_time_mixl.rds")
saveRDS(tim,file="output/time_money_time_mixl.rds")
saveRDS(poo,file="output/pooled_money_time_mixl.rds")
saveRDS(sca,file="output/scaled_money_time_mixl.rds")

####################################################
################################### HYPOTHESIS TESTS
####################################################

############################
###############   READ FILES
############################

## READ RESULTS
moti.results <- readRDS("output/results_money_time.rds")
moti.betas   <- readRDS("output/betas_money_time.rds")
moti.se      <- readRDS("output/stderrors_money_time.rds")

mo <- readRDS("output/money_money_time_mixl.rds")
ti <- readRDS("output/time_money_time_mixl.rds")
pooled <- readRDS("output/pooled_money_time_mixl.rds")
scaled <- readRDS("output/scaled_money_time_mixl.rds")

############################
###############  MONEY TESTS
############################

## TESTS
ll.money <- as.numeric(logLik(mo))
ll.time <- as.numeric(logLik(ti))
ll.pooled <- as.numeric(moti.results[scale==1,]$LL)
ll.scaled <- as.numeric(moti.results[scale==1.275,]$LL)
ll.scaled
ll.pooled

## H_0: EQUAL PARAMETERS
test <- 2*((ll.money + ll.time) - ll.scaled)
critB <- qchisq(p = 0.95, df = 46)
equalB  <- test > critB
pvalueB <- pchisq(test, 46,lower.tail=F)
testB   <- test 

## H_0: EQUAL SCALE 
test <- 2*(ll.scaled - ll.pooled)
critM <- qchisq(p = 0.95, df = 1)
equalM  <- test > critM
pvalueM <- pchisq(test, 1,lower.tail=F)
testM   <- test 

############################
###############  PRINT TESTS
############################

## LL - MONEY
ll.money
ll.time
ll.pooled
ll.scaled

## RELATIVE SCALE 
scale.ratio

## TEST STATISTICS
critB
critM
testB
testM
pvalueB
pvalueM

## HYPOTHESES
equalB
equalM

##########################################
###########################   PLOT - MONEY
##########################################

## READ IN RESULTS
moti.results[, color := 'Ha',]

## CREATE SCALAR
maxLL <- max(moti.results$LL, na.rm=T)
scalar <- moti.results[LL==maxLL,]$scale
results[, scalar := scalar,]
scalar
maxLL

## HORIZONTAL LINES
moti.results[, testline := (ll.money + ll.time),]
moti.results[, testline_color := 'Money + Time',]
moti.results[, pooled := ll.pooled - testM/2,]
moti.results[, pooled_color := 'Pooled, Scale = 1',]
moti.results[, scaled := ll.scaled,]
results <- moti.results

## TEST VALUES
crit1 <- qchisq(p = 0.95, df = 46)/2
crit1
crit2 <- qchisq(p = 0.95, df = 1)/2
crit2

## PLOT 
fig <- ggplot(results) + 
  geom_line(aes(x=scale, y=LL, linetype = "solid"), size=0.75) + 
  geom_vline(aes(xintercept=scalar),linetype = "solid", size=0.75) + 
  geom_vline(aes(xintercept=1),linetype = "dashed", size=0.75) +
  geom_line(aes(x=scale, y=testline, linetype = "twodash"), size=0.75) + 
  geom_ribbon(aes(x=scale, ymin = LL - crit1, ymax = LL + crit1), fill = "grey70", color="grey70",alpha = 0.3,linetype = "dashed") +
  scale_y_continuous(breaks = seq(-2500,-2000,100),
                     limits=c(-2500,-2000),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0,2.1,by=0.2),
                     expand = c(0,0)) + 
  scale_linetype_discrete(labels = unname(TeX(c("$LL_\\sigma$","$(LL_R + LL_U)$")))) +
  annotate("text",x = .30, y = -2070,
           label=TeX("$(LL_M + LL_T)= \ -2079.82$", output = "character"),
           hjust=0, parse=T) +
  annotate("text",x = 1.3, y = -2475,
           label=TeX("$\\sigma =1.275$", output = "character"),
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
fig
ggsave(fig,filename="plots/scale_money_time.png",h=5,w=8,type="cairo-png")

## END OF SCRIPT. Have a great day! 