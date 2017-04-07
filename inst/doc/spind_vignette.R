## ----Library call, echo=FALSE--------------------------------------------
library(spind)

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----GEE Data Infiling, eval=FALSE---------------------------------------
#  data(musdata)
#  data(carlinadata)
#  
#  # Examine the structure to familiarize yourself with the data
#  ?musdata
#  head(musdata)
#  
#  ?carlinadata
#  head(carlinadata)
#  

## ----GEE Example, fig.width=5,fig.height=5-------------------------------
# Next, fit a simple GEE and view the output
coords<-musdata[ ,4:5]

mgee<-GEE(musculus ~ pollution + exposure, family="poisson", data=musdata,
      coord=coords, corstr="fixed", plot=TRUE, scale.fix=FALSE)

summary(mgee, printAutoCorPars=TRUE)

predictions<-predict(mgee, newdata=musdata)

## ----WRM Example, fig.width=5, fig.height=5------------------------------


mwrm<-WRM(musculus ~ pollution + exposure, "poisson", musdata,
coord=coords, level=1, plot=TRUE)

summary(mwrm)

predictions<-predict(mwrm, newdata=musdata)


## ----Padded WRM Example, fig.width=5, fig.height=5-----------------------

# Padding with mean values

padded.mwrm<-WRM(musculus ~ pollution + exposure, "poisson", musdata,
                 coord=coords, level=1, pad=list(padform=1), plot=TRUE)

summary(padded.mwrm)

padded.predictions<-predict(padded.mwrm, newdata=musdata)


## ----Covar.plot Example, fig.width=5, fig.height=5-----------------------

coords<-carlinadata[ ,4:5]

covar.plot(carlina.horrida ~ aridity + land.use - 1,
           data=carlinadata, coord=coords, wavelet="d4",
           wtrafo='modwt', plot='covar')

covar.plot(carlina.horrida ~ aridity + land.use - 1,
           carlinadata, coord=coords, wavelet="d4",
           wtrafo='modwt', plot='var')


## ----Upscale Example, fig.width=7, fig.height=7--------------------------

upscale(carlinadata$land.use, carlinadata$x, carlinadata$y)



## ----Step.spind Example--------------------------------------------------

# For demonstration only. We are artificially imposing a grid structure
# on data that is not actually spatial data

library(MASS)
data(birthwt)


x<-rep(1:14,14)
y<-as.integer(gl(14,14))
coords<-cbind(x[-(190:196)],y[-(190:196)])

formula<-formula(low ~ age + lwt + race + smoke + ftv +  bwt + I(race^2))

mgee<-GEE(formula, family="gaussian", data=birthwt,
          coord=coords, corstr="fixed",scale.fix=TRUE)

mwrm<-WRM(formula, family="gaussian", data=birthwt,
          coord=coords, level=1)

ssgee<-step.spind(mgee, birthwt)
sswrm<-step.spind(mwrm, birthwt, AICc=TRUE)

best.mgee<-GEE(ssgee$model, family = "gaussian", data=birthwt,
           coord=coords, corstr="fixed",scale.fix=TRUE)

best.wrm<-WRM(sswrm$model, family="gaussian", data=birthwt,
              coord=coords, level = 1)

summary(best.mgee, printAutoCorPars=FALSE)
summary(best.wrm)


## ----mmi... example------------------------------------------------------

# Example for WRMs
data(carlinadata)
coords<- carlinadata[,4:5]

wrm<- WRM(carlina.horrida ~ aridity + land.use, "poisson",
          carlinadata, coords, level=1, wavelet="d4")

ms1<-scaleWMRR(carlina.horrida ~ aridity + land.use,"poisson",
                carlinadata,coords,scale=1,wavelet='d4',plot=F)

mmi<- mmiWMRR(wrm, data=carlinadata, scale=1, detail=TRUE)

# Example for GEEs
library(MASS)
data(birthwt)

# impose an artificial (not fully appropriate) grid structure
x<-rep(1:14,14)
y<-as.integer(gl(14,14))
coords<-cbind(x[-(190:196)],y[-(190:196)])

formula<-formula(low ~ race + smoke +  bwt)

mgee<-GEE(formula, family = "gaussian", data = birthwt,
         coord=coords, corstr="fixed", scale.fix=TRUE)

mmi<-mmiGEE(mgee, birthwt)



## ----RVI.plot Example, fig.width=5, fig.height=5-------------------------
data(carlinadata)
coords<- carlinadata[,4:5]

rvi.plot(carlina.horrida ~ aridity + land.use,"poisson",
         data=carlinadata,coord=coords,maxlevel=4,detail=TRUE,wavelet="d4")


## ----GOF data, eval = FALSE----------------------------------------------
#  data(hook)
#  
#  # Familiarize yourself with the data
#  ?hook
#  head(hook)
#  

## ----Spatial Indices Example, fig.width=5,fig.height=5-------------------

df<-hook[,1:2]
coords<-hook[,3:4]

# Threshold dependent metrics
th.dep.indices<-th.dep(data=df, coord=coords, spatial=TRUE)

# Confusion Matrix
th.dep.indices$cm

# Kappa statistic
th.dep.indices$kappa

# Threshold independent metrics
th.indep.indices<-th.indep(data=df, coord=coords, spatial=TRUE, plot.ROC=TRUE)

# AUC
th.indep.indices$AUC

# TSS
th.indep.indices$TSS




## ----ACFFT example-------------------------------------------------------

coords<- musdata[,4:5]
mglm <- glm(musculus ~ pollution + exposure, "poisson", musdata)

ac<-acfft(coords[ ,1], coords[ ,2], resid(mglm, type="pearson"),
          lim1=0, lim2=1, dmax=10)
ac


