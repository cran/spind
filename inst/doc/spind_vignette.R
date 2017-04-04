## ---- echo=FALSE---------------------------------------------------------
library(spind)

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

summary(mgee,printAutoCorPars=TRUE)

predictions<-predict(mgee,newdata=musdata)

## ---- fig.width=5, fig.height=5------------------------------------------


mwrm<-WRM(musculus ~ pollution + exposure, "poisson", musdata,
coord=coords, level=1, plot=TRUE)

summary(mwrm)

predictions<-predict(mwrm, newdata=musdata)


## ---- fig.width=5, fig.height=5------------------------------------------

# Padding with mean values

padded.mwrm<-WRM(musculus ~ pollution + exposure, "poisson", musdata,
                 coord=coords, level=1, pad=list(padform=1), plot=TRUE)

summary(padded.mwrm)

padded.predictions<-predict(padded.mwrm, newdata=musdata)


## ---- fig.width=5, fig.height=5------------------------------------------

coords<-carlinadata[ ,4:5]

covar.plot(carlina.horrida ~ aridity + land.use,
           data=carlinadata,coord=coords,wavelet="d4",
           wtrafo='modwt',plot='covar')

covar.plot(carlina.horrida ~ aridity + land.use,
           carlinadata,coord=coords,wavelet="d4",
           wtrafo='modwt',plot='var')


## ------------------------------------------------------------------------

library(MASS)
data(birthwt)


x<-rep(1:14,14)
y<-as.integer(gl(14,14))
coords<-cbind(x[-(190:196)],y[-(190:196)])

formula<-formula(low ~ age + lwt + race + smoke + ftv +  bwt)

mgee<-GEE(formula, family = "gaussian", data = birthwt,
          coord=coords, corstr="fixed",scale.fix=TRUE)

ss<-step.spind(mgee,birthwt)

best.mgee<-GEE(ss$model, family = "gaussian", data = birthwt,
           coord=coords, corstr="fixed",scale.fix=TRUE)

summary(best.mgee,printAutoCorPars=FALSE)


## ------------------------------------------------------------------------

data(carlinadata)
coords<- carlinadata[,4:5]

mmi<- mmiWMRR(carlina.horrida ~ aridity + land.use,"poisson",
              carlinadata,coords,scale=3,detail=TRUE,wavelet="d4")


## ---- fig.width=5, fig.height=5------------------------------------------

rvi.plot(carlina.horrida ~ aridity + land.use,"poisson",
         carlinadata,coords,maxlevel=4,detail=TRUE,wavelet="d4")


## ---- eval = FALSE-------------------------------------------------------
#  data(hook)
#  
#  # Familiarize yourself with the data
#  ?hook
#  head(hook)
#  

## ----Spatial Indices Example, fig.width=5,fig.height=5,collapes=TRUE,comment="#>"----

df<-hook[,1:2]
coords<-hook[,3:4]

# Threshold dependent metrics
th.dep.indices<-th.dep(data=df,coord=coords,spatial=TRUE)

# Confusion Matrix
th.dep.indices$cm

# Kappa statistic
th.dep.indices$kappa

# Threshold independent metrics
th.indep.indices<-th.indep(data=df,coord=coords,spatial=TRUE,plot.ROC=TRUE)

# AUC
th.indep.indices$AUC

# TSS
th.indep.indices$TSS




