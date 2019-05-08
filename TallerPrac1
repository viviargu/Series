
########################### Punto 1 #################################

######Base de Pasajeros###
data("AirPassengers")
plot(AirPassengers)
#####TransformaciÃ³n Box-Cox
library(lattice);library(leaps);library(ltsa);library(bestglm)
library(FitAR)
library(forecast)

FitAR::BoxCox(AirPassengers) 
lAirPass=log(AirPassengers)
par(mfrow=c(2,1))
plot(AirPassengers)
plot(lAirPass)

######DescomposiciÃ³n usando promedios MÃ³viles
deslAirPass=decompose(lAirPass)
plot(deslAirPass)
deslAirPass
residPM=deslAirPass$random
plot(residPM)
acf(as.ts(as.vector(residPM)), lag.max = 50, plot=TRUE, na.action = na.pass) #Es estacionario
####DescompoisiciÃ³n usando suavizamiento exponencial
HWAP=HoltWinters(lAirPass,seasonal="additive")
plot(HWAP)
ajustados=fitted(HWAP)
plot(ajustados)
ajustados
plot(AirPassengers)
residHW=lAirPass-ajustados[,2]-ajustados[,3]-ajustados[,4]
plot(resid) 
acf(as.ts(as.vector(residHW)), lag.max = 50, type = "correlation", plot=TRUE) #Sí es estacionario

########################### Punto 2 #################################
library(readxl)
accidentes <- read_excel("C:/Users/Viviana/Desktop/Materias/Series de tiempo/Practica1/Base_Accidentes.xlsx")
View(accidentes)

## BOX-COX
ise=ts(accidentes$ISE,start=c(2005,1),frequency=12);View(ISE)
plot(ise)
FitAR::BoxCox(ise) #lambda=-0.322
lise=log(ise)

acc=ts(accidentes$ACC,start=c(2005,1),frequency=12);View(ISE)
plot(acc)
FitAR::BoxCox(acc);
lambda=BoxCox.lambda(acc, "guerrero", upper=5)
lacc=(acc^lambda-1)/lambda
# Filtros: PM
deslise=decompose(lise)
plot(deslise)
deslise
PMresid_ise=deslise$random
acf(as.ts(as.vector(PMresid_ise)), lag.max = 50, plot=TRUE, na.action = na.pass)

desacc=decompose(lacc)
plot(desacc)
desacc
PMresid_acc=desacc$random
acf(as.ts(as.vector(PMresid_acc)), lag.max = 50, plot=TRUE, na.action = na.pass)

# Suavizamiento Exponencial
iseHW=HoltWinters(lise,seasonal="additive")
plot(iseHW)
ajustados_ise=fitted(iseHW)
plot(ajustados_ise)
HWresid_ise=lise-ajustados_ise[,2]-ajustados_ise[,3]-ajustados_ise[,4]
plot(HWresid_ise)
acf(as.ts(as.vector(HWresid_ise)), lag.max = 50, plot=TRUE)

laccHW=HoltWinters(lacc,seasonal="additive")
plot(laccHW)
ajustados_acc=fitted(laccHW)
plot(ajustados_acc)
HWresid_acc=lacc-ajustados_acc[,2]-ajustados_acc[,3]-ajustados_acc[,4]
plot(HWresid_acc)
acf(as.ts(as.vector(HWresid_acc)), lag.max = 50, plot=TRUE)

########################### Punto 3 #######################################
n=200 #TamaÃ±o de la serie
l=50 #condiciones iniciales
theta=-0.5 
phi=0.5
sigma=2
### IID ####
serieIID=ts(rnorm(n,0,2), frequency = 4)
plot(serieIID,main='IID')
acf(as.ts(as.vector(serieIID)), na.action=na.pass)
FitAR::BoxCox(serieIID); BoxCox.lambda(serieIID) #lambda=0.89
desIID=decompose(serieIID)
plot(desIID)
plot(desIID$random)
par(mfrow=c(1,1))
acf(as.ts(as.vector(desIID$random)), na.action=na.pass)

HW_IID=HoltWinters(serieIID,seasonal="additive")
plot(HW_IID)
ajustadosIID=fitted(HW_IID)
residHW_IID=ajustadosIID[,2]-ajustadosIID[,3]-ajustadosIID[,4]
plot(residHW_IID)
acf(as.ts(as.vector(residHW_IID)))

##### MA(1) #####
l=50
ruido=rnorm(n+l,0,sigma)
MA1aux=rep(0,n+l)
MA1aux
for(j in 2:n+l){
  MA1aux[j]=theta*ruido[j-1]+ruido[j]
}
MA1=as.ts(MA1aux[l+1:n])
plot(MA1)
acf(MA1)

serieMA=ts(MA1,frequency=9)
acf(as.ts(as.vector(serieMA)))
FitAR::BoxCox(serieMA) #lambda=0.96
BoxCox.lambda(serieMA, method = "guerrero") #lambda=1.09
## PM
desMA=decompose(serieMA)
acf(as.ts(as.vector(desMA$random)), na.action=na.pass)

### SE
HW_MA=HoltWinters(serieMA,seasonal="additive")
ajustadosMA=fitted(HW_MA)
residHW_MA=serieMA-ajustadosMA[,2]-ajustadosMA[,3]-ajustadosMA[,4] #Faltó escribir la serie original, por eso los residuales daban raros
acf(as.ts(as.vector(residHW_MA)), type="correlation")
##### AR(1) #####
AR<-function(muestra, phi){
  x=NULL
  x[1]=0
  for(i in 2:length(muestra)){ 
    x[i]=phi*x[i-1]+muestra[i]
  }
  as.ts(x)
  #t=seq(1, length(x), by=1)
  plot(x, type="l",xlab="t", ylab=expression(X_t), main="Proceso AR(1)")
  return(x)
}
#set.seed(3)
muestra=rnorm(200,0,2)
serieAR=ts(AR(muestra, 0.5), frequency=4)
FitAR::BoxCox(serieAR) #lambda=0.98
BoxCox.lambda(serieAR) #lambda=1.02
## PM
desAR=decompose(serieAR)
plot(desAR)
#plot(serieAR)
plot(desAR$random)
#acf(as.ts(as.vector(serieAR)), na.action=na.pass)
acf(as.ts(as.vector(desAR$random)), na.action=na.pass)
## SE     
HW_AR=HoltWinters(serieAR, seasonal = "additive")
ajustadosHW_AR=fitted(HW_AR)
residAR=serieAR-ajustadosHW_AR[,2]-ajustadosHW_AR[,3]-ajustadosHW_AR[,4]
plot(residAR)
#acf(as.ts(as.vector(serieAR)))
acf(as.ts(as.vector(residAR)))

############################## Punto 4 #############################################
mt.st.AR<-function(a,b,muestra, phi){
  trend=NULL
  seasonal=NULL
  t=seq(from=1, to=length(muestra))
  for (i in 1:length(muestra)) {
    trend[i]=a*t[i]+b
    seasonal[i]=ts(sin((pi/6)*t[i]))
    
    x=NULL
    x[1]=0
    for(i in 2:length(muestra)){ 
      x[i]=phi*x[i-1]+muestra[i]
    }
  }
  par(mfrow=c(c(4,1)))
  plot(ts(x), ylab="AR(1)")
  plot(ts(trend), ylab="trend")
  plot(ts(seasonal, frequency=12), ylab="seasonal")
  plot(ts(trend+ts(seasonal, frequency=12)+x), ylab="tm+ts+AR(1)")
  componentes=matrix(c(trend,seasonal,x), nr=length(muestra), nc=3, byrow=FALSE) 
  return(componentes)
}
  set.seed(3)
  muestra=rnorm(200,0,2)
  componentes=mt.st.AR(0.3,0.6,muestra,0.5)
  xt=ts(componentes[,1]+componentes[,2]+componentes[,3], frequency=12)
  plot(xt)
  ##PM
  par(mfrow=c(1,1))
  FitAR::BoxCox(xt)
  BoxCox.lambda(xt)
  desxt=decompose(ts(xt, frequency=12))
  plot(desxt)
  par(mfrow=c(1,2))
  plot(ts(componentes[,1]))
  plot(desxt$trend)
  par(mfrow=c(2,1))
  plot(ts(componentes[,2]))
  plot(desxt$seasonal)
  
  ##SE
  HWxt=HoltWinters(xt, seasonal ="additive")
  ajustados4=fitted(HWxt)
  plot(ajustados4)
  par(mfrow=c(1,2))
  plot(ts(componentes[,1]))
  plot(ajustados4[,2]+ajustados4[,3])
  par(mfrow=c(2,1))
  plot(ts(componentes[,2]))
  plot(ajustados4[,4])
