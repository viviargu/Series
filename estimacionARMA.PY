#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 24 05:45:48 2018

@author: sergiocalderonv
"""

import pandas as pd
import numpy as np
import matplotlib.pylab as plt
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()
from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.tsa.stattools import acf
from statsmodels.tsa.stattools import pacf
from statsmodels.graphics.tsaplots import plot_pacf
from statsmodels.tsa.arima_model import ARIMA
#from sm.tsa.statespace import SARIMAX
from matplotlib.pylab import rcParams
rcParams['figure.figsize'] = 15, 6

#Usamos Pandas para manejar las bases de Datos

data = pd.read_csv('AirPassengers.csv')
print(data)
print('\n Data Types:')
print(data.dtypes)

#shift+ctrl+v abre el explorador de variables
######Convertir el conjunto de datos en una serie de Timepo#####


con=data['Month']
data['Month']=pd.to_datetime(data['Month'])
##data.set_index('Month', inplace=True)
pasajeros=data.set_index('Month')
#check datatype of index

#convert to time series:
tsAirP = pasajeros['NPassengers']
tsAirP.head(10)
tsAirP.asfreq(freq='MS')
####Graficar la Serie y transformaciones#####
plt.plot(tsAirP)
plt.show()
import statsmodels.api as smapi
smapi.graphics.tsa.month_plot(tsAirP)
acf(tsAirP,nlags=50,unbiased=False)
plot_acf(tsAirP,lags=50,unbiased=False)

ltsAirP=np.log(tsAirP)
plt.plot(ltsAirP)
d1ltsAirP=ltsAirP.diff(periods=1)[1:]
plt.plot(d1ltsAirP)
smapi.graphics.tsa.month_plot(d1ltsAirP)
ds1d1ltsAirP=d1ltsAirP.diff(periods=12)[12:]
plt.plot(ds1d1ltsAirP)
smapi.graphics.tsa.month_plot(ds1d1ltsAirP)
#######Identificación#######
acf(ds1d1ltsAirP,nlags=50,unbiased=False)
plot_acf(ds1d1ltsAirP,lags=50,unbiased=False)
pacf(ds1d1ltsAirP,nlags=50)
plot_pacf(ds1d1ltsAirP,lags=50)

#####Estimación#########
modeloAR=ARIMA(ds1d1ltsAirP,order=(12,0,0)) ####Ajuste de un AR(12)
ajusteAR=modeloAR.fit(trend='c')
print(ajusteAR.summary())

modeloMA=ARIMA(ds1d1ltsAirP,order=(0,0,12)) ####Ajuste de un MA(12)
ajusteMA=modeloMA.fit(trend='c')
print(ajusteMA.summary())

####Especificando las entradas de los parámetros.#####
#ma_orden=[1,0,0,0,0,0,0,0,0,0,0,1]
#modeloMAfinal=ARIMA(ds1d1ltsAirP,order=(0,0,ma_orden)) ####Ajuste de un MA(12)
#ajusteMAfinal=modeloMAfinal.fit(trend='c')
#print(ajusteMAfinal.summary())


modeloMASPmodel = smapi.tsa.statespace.SARIMAX(ds1d1ltsAirP, trend='n', order=(0,0,12))  
####Si tiene parte estacional agregar seasonal_order=(0,0,0,12)###
results = modeloMASPmodel.fit()
print(results.summary())


####Especificando las entradas de los parámetros.#####
ma_orden=[1,0,0,0,0,0,0,0,0,0,0,1]

modeloMASPmodelfinal = smapi.tsa.statespace.SARIMAX(ds1d1ltsAirP, trend='n', order=(0,0,ma_orden))  
####Si tiene parte estacional agregar seasonal_order=(0,0,0,12)###
resultsMAfinal = modeloMASPmodelfinal.fit()

print(resultsMAfinal.summary())


####Etapa de pronosticos######
pronosticosMA=resultsMAfinal.forecast(steps=12)
modeloMASPmodelfinal
salforecastMA=resultsMAfinal.get_prediction(start=131, end=142,full_results=True,alpha=0.05,dynamic=False)

salforecastMA.conf_int(alpha=0.05)

salforecastMAotro=resultsMAfinal.get_forecast(steps=12)

ICforecastMA95=salforecastMAotro.conf_int(alpha=0.05)
pronosticosMA=salforecastMAotro.predicted_mean

#######Gráfica de los pronósticos#####
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(1,1,1)
#Actual data
ax.plot(ds1d1ltsAirP.astype('float64'), '--', color="blue", label='data')
# Means
ax.plot(pronosticosMA, lw=1, color="black", alpha=0.5, label='SARIMAX')
ax.fill_between(pronosticosMA.index, ICforecastMA95.iloc[:, 0], ICforecastMA95.iloc[:, 1], alpha=0.05)
ax.legend(loc='upper right')
plt.draw()

#####Análisis de Residuales######
residuales=resultsMAfinal.resid
resultsMAfinal.plot_diagnostics()
plt.show()

resultsMAfinal.test_heteroskedasticity(method='breakvar')

resultsMAfinal.test_normality(method='jarquebera')

acf(residuales,nlags=50,unbiased=False)
plot_acf(residuales,lags=50,unbiased=False)
pacf(residuales,nlags=50)
plot_pacf(residuales,lags=50)

resultsMAfinal.test_serial_correlation(method='ljungbox')


#####Simulación####
import numpy as np

import statsmodels as sm
import matplotlib.pylab as plt
import pandas as pd
import statsmodels.api as smapi

from statsmodels.graphics.tsaplots import plot_acf
from statsmodels.tsa.stattools import acf
from statsmodels.tsa.stattools import pacf
from statsmodels.graphics.tsaplots import plot_pacf
from statsmodels.tsa.arima_model import ARIMA
import pmdarima as pm
Tlength=250
ar = np.array([1, -0.7]) 
ma = np.array([1,0.6]) # add zero-lag y maparams tiene los otros parámetros
y = sm.tsa.arima_process.arma_generate_sample(ar, ma,Tlength) 
plt.plot(y)
ajuste=pm.auto_arima(y,start_p=0,start_q=0, max_p=3, max_q=3, d=0, D=0, seasonal=False)

ajuste.summary()
modeloest = smapi.tsa.statespace.SARIMAX(y, trend='n', order=(1,0,1))  
####Si tiene parte estacional agregar seasonal_order=(0,0,0,12)###
resultsmodeloest = modeloest.fit()

print(resultsmodeloest.summary())
resultsmodeloest.aic
###an´lisis de residuales
residuales=resultsmodeloest.resid
resultsmodeloest.plot_diagnostics()
plt.show()

resultsmodeloest.test_heteroskedasticity(method='breakvar')

resultsmodeloest.test_normality(method='jarquebera')

acf(residuales,nlags=50,unbiased=False)
plot_acf(residuales,lags=50,unbiased=False)
pacf(residuales,nlags=50)
plot_pacf(residuales,lags=50)

resultsmodeloest.test_serial_correlation(method='ljungbox')





modeloprueba = smapi.tsa.statespace.SARIMAX(np.log(tsAirP), trend='n', order=(0,1,1),seasonal_order=(0,1,1,12))  
resultsmodeloprueba = modeloprueba.fit()

print(resultsmodeloprueba.summary())

residuales=resultsmodeloprueba.resid
