# -*- coding: utf-8 -*-
"""
Created on Tue May  7 15:51:49 2019

@author: Viviana
"""

import pandas as pd
from pandas import Series
import numpy as np
import scipy as sp
import matplotlib.pylab as plt
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 15, 6
#from pydataset import data
from statsmodels.tsa.arima_process import arma_acf
from statsmodels.tsa.arima_process import ArmaProcess
import statsmodels as sm
import statsmodels.api as smapi
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.tsa.stattools import acf
from statsmodels.graphics.tsaplots import plot_pacf
from statsmodels.tsa.stattools import pacf
from statsmodels.tsa.stattools import acovf
from statsmodels.tsa.arima_model import ARIMA
#data('AirPassengers')
#Usamos Pandas para manejar las bases de Datos

data = pd.read_csv('AirPassengers.csv')
print(data)
print('\n Data Types:')
print(data.dtypes)

######Convertir el conjunto de datos en una serie de Tiempo#####
con=data['Month']
data['Month']=pd.to_datetime(data['Month']) # Extraer los las fechas de los datos
##data.set_index('Month', inplace=True)
pasajeros=data.set_index('Month')  #INDEXAMOS los datos
#check datatype of index

#convert to time series:
ts = pasajeros['NPassengers']
ts.head(10)

####Graficar la Serie#####
plt.plot(ts)
plt.title('AirPassengers') 

#####Transformación Box-Cox
import scipy.stats ####En ocasiones puede funcionar la línea 46 sin ésta línea.
#scipy permite hacer BoxCox, pero la transformación de la serie queda en una matriz
sp.stats.boxcox(ts,alpha=0.05)     #Esta linea es igual a la siguiente
sp.stats.boxcox(data['NPassengers'],alpha=0.05) 
logAirp=sp.stats.boxcox(data['NPassengers'],lmbda=0)
data = data.assign(logAirp=logAirp)   
logpasajeros=data.set_index('Month')   
logAirPass=  logpasajeros['logAirp'] 
plt.plot(logAirPass)

####Descomposición usando filtros
from statsmodels.tsa.seasonal import seasonal_decompose
from matplotlib import pyplot

result = seasonal_decompose(logAirPass, model='additive')
print(result.trend)
print(result.seasonal)
print(result.resid)
print(result.observed)
result.plot()
#pyplot.show()
residuos=pd.Series(result.resid[6:138])
plt.plot(residuos)
#print(residuos.dtypes)
acf(residuos, nlags=30, unbiased=False, missing='drop', alpha=0.05)
plot_acf(residuos, lags=30, unbiased=False)

####Holt-Winters
from statsmodels.tsa.api import ExponentialSmoothing, SimpleExpSmoothing, Holt
print(ts)
print(pd.infer_freq(data['Month'], warn=True))
fit1 = ExponentialSmoothing(ts, seasonal_periods=12, trend='add', seasonal='add').fit(use_boxcox=True)
####ejecutar desde la línea 74 hasta la 88
plt.subplot(5, 1, 1)
plt.plot(ts)
plt.ylabel('AirPassengers') 
plt.subplot(5, 1, 2)
plt.plot(fit1.level)
plt.ylabel('level') 
plt.subplot(5, 1, 3)
plt.plot(fit1.slope)
plt.ylabel('slope') 
plt.subplot(5, 1, 4)
plt.plot(fit1.season)
plt.ylabel('sesaon') 
plt.subplot(5, 1, 5)
plt.plot(fit1.resid)
plt.ylabel('resid') 
resid=fit1.resid
acf(resid, nlags=30, unbiased=False, missing='drop', alpha=0.05)
plot_acf(resid, lags=30, unbiased=False)

#DIF
diff1logAirPass=logAirPass.diff(periods=1)####Diferencia Ordinaria (remueve la componente de tendencia)
plt.plot(diff1logAirPass)
diffs1diff1logAirPass=diff1logAirPass.diff(periods=12) ###Diferencia Estacional (remueve la componente estacional)
print(diffs1diff1logAirPass)
plt.plot(diffs1diff1logAirPass)

############################################ Punto 2 ############################################

data2 = pd.ExcelFile('Base_Accidentes.xlsx')
print(data2.sheet_names)
#Data Frame de la hoja "Datos"
data2=data2.parse('DATA2005')
print(data2)
#ISE
#Indexamos y determinamos la frecuencia (mensual)
ind = pd.date_range(start='1/1/2005', end='8/1/2018', freq='M')
accidentes2 = data2.set_index(ind)
ise = pd.Series(accidentes2['ISE'],index=ind)
print(ise.dtypes)
#BC
sp.stats.boxcox(ise,alpha=0.05)     
logise=sp.stats.boxcox(ise,lmbda=0) 
serielogise=pd.Series(logise, index=ind)  
plt.plot(serielogise) 

#PM
result2 = seasonal_decompose(serielogise, model='additive')
resid2=pd.Series(result2.resid[6:157])
acf(resid2, nlags=30, unbiased=False)
plot_acf(resid2, lags=30, unbiased=False)

#SE
fit2 = ExponentialSmoothing(ise, seasonal_periods=12, trend='add', seasonal='add').fit(use_boxcox=True)
acf(fit2.resid, nlags=30, unbiased=False, alpha=0.05)
plot_acf(fit2.resid, lags=30, unbiased=False)

#ACC
#Indexamos y determinamos la frecuencia (mensual)
ind = pd.date_range(start='1/1/2005', end='8/1/2018', freq='M')
accidentes2 = data2.set_index(ind)
acc = pd.Series(accidentes2['ACC'],index=ind)
print(acc.dtypes)
#BC
sp.stats.boxcox(acc,alpha=0.05)     
tracc=sp.stats.boxcox(acc,lmbda=3.3578573249357277) 
#tra=(acc**(3.3578573249357277)-1)/3.3578573249357277 #Fórmula 
serietracc=pd.Series(tracc, index=ind)  
plt.plot(serietracc) 

#PM
result3 = seasonal_decompose(serietracc, model='additive')
resid3=pd.Series(result3.resid[6:157])
acf(resid3, nlags=30, unbiased=False)
plot_acf(resid3, lags=30, unbiased=False)

#SE
fit3 = ExponentialSmoothing(acc, seasonal_periods=12, trend='add', seasonal='add').fit(use_boxcox=True)
acf(fit3.resid, nlags=30, unbiased=False, alpha=0.05)
plot_acf(fit3.resid, lags=30, unbiased=False)


# IID: gamma(h)= sigma^2 si h=0 y 0 e.o.c
#Cam.Ale: E(st)=0; gamma(0)=var(st)=t*sigma^2
#Estacionario ent |ga(h)|<=ga(0) |E((x_t+h-mu)(xt-mu))|<= sqrt(E((xt+h-mu)^2)sqrt()=var(xt)=ga(0)
#MA(1) depende de ruidos pasados y presentes: xt=sum j=0 a +inf sij*Zt-j
#MA(1), ga(H)=sig^2*(1-theta^2) si h=0, sig^2*theta si h=+-1, 0 e.o.c
#sum yj*zt-j converge con prob 1 si sum E(|yj*zt-j|) converge (T.conveg mon)
    #jensen: E^2(|x|)<=E(|x|^2)=sig^2 ent E(|x|)<=sig.
#Si sum si^2_j <inf ent. sum si_j*Zt-j conv el M.C al mismo lim
#AR(1): ga(h)= phi^h *ga(0) ; ga(0)=sig^2/(1-\phi^2); rho(h)=phi^|h|
    #Soluciones sum j= 0 a k phi^j*zt-j ; -sum 1 a inf phi^-j*zt+j depende de ruidos futuros
    #Bien definido: que la sum de coef es abs sumable y que la sum orig converge en prob 1
    #Encontrar solu: phi(B)^-1*phi(B)=1=1B^0+0^Bj y phi(B)^-1=pi(B)=sum 0 a inf de pij*(B)^j ; phi(B)=1-phi*B
    #Llegar a que pi(B)=sum 0 a inf phi^j*B^j y si |phi|>1 pi(B)=sum 1 a inf phi^-j*B^j
#ARMA(1,1)=xt=phi*xt-1+theta*Zt-1+Zt
    #Sol= Zt+(phi+theta)*sum j= 1 a inf phi^j-1*Zt-j (presentes, pasados)
    #Si |phi|>1 sol= -theta*phi^-1*zt-(tehta+phi)*sum 1 a inf phi^-j*Zt+j
    #ga(h)=sum o a inf sij*sij+h*sig^2 con sij= 1 si j=0 y (phi+theta)*phi^j-1 si j>=1
