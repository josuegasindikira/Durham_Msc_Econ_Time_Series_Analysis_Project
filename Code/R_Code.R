# Housekeeping
rm(list=ls()) # clear workspace
graphics.off() # close all graphs
library(vars) # VAR tools
library(astsa) # time series tools
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory to script folder
couleur=1 # plot color
PRICEFR=read.csv("FRACPICORQINMEI.csv",col.names=c("t","PRICEFR")) # France price index
PRICEUS=read.csv("USACPICORQINMEI.csv",col.names=c("t","PRICEUS")) # US price index
IR10FR=read.csv("IRLTLT01FRQ156N.csv",col.names=c("t","IR10FR")) # France 10-year interest rate
head(PRICEFR) # preview France price index
head(PRICEUS) # preview US price index
head(IR10FR) # preview France interest rate
PRICEFR=ts(PRICEFR[,2],start=c(1970,1),frequency=4) # quarterly France price index
PRICEUS=ts(PRICEUS[,2],start=c(1957,1),frequency=4) # quarterly US price index
IR10FR=ts(IR10FR[,2],start=c(1960,1),frequency=4) # quarterly France interest rate
INFFR=400*diff(log(PRICEFR)) # annualised quarterly inflation for France
INFUS=400*diff(log(PRICEUS)) # annualised quarterly inflation for US
Start=1970 # sample starts in 1971Q1
INFFR=window(INFFR,start=c(Start,2),end=c(2024,4)) # restrict France inflation sample
INFUS=window(INFUS,start=c(Start,2),end=c(2024,4)) # restrict US inflation sample
IR10FR=window(IR10FR,start=c(Start,2),end=c(2024,4)) # restrict France interest rate sample

#################### QUESTION 2 LEVEL GRAPHS
x=ts.intersect(INFFR,INFUS,IR10FR) # align the three series on common dates
colnames(x)=c("INFFR","INFUS","IR10FR") # assign variable names
par(mfrow=c(3,1),mar=c(3,4,2,2),oma=c(0,0,2,0)) # 3 stacked plots
tsplot(INFFR,col=couleur,ylab="Inflation",xlab="Time",type="o",main="Inflation for France") # France inflation series
tsplot(INFUS,col=couleur,ylab="Inflation",xlab="Time",type="o",main="Inflation for United States") # US inflation series
tsplot(IR10FR,col=couleur,ylab="Interest Rates",xlab="Time",type="o",main="Interest Rates for France") # France interest rate series

###### RAW DATA PERIODOGRAMS
mvspec(x[,"INFFR"],main="Raw Periodogram on raw data for inflation, France") # raw periodogram for France inflation
mvspec(x[,"INFUS"],main="Raw Periodogram on raw data for inflation, US") # raw periodogram for US inflation
mvspec(x[,"IR10FR"],main="Raw Periodogram on raw data for Interest rate, France") # raw periodogram for France interest rate

################### FIRST DIFFERENCES
dx=diff(x) # first differences of all variables
plot.ts(dx[,"INFFR"],col=couleur,ylab="FD Inflation",xlab="Time",main="First Difference Inflation for France") # first difference of France inflation
plot.ts(dx[,"INFUS"],col=couleur,ylab="FD Inflation",xlab="Time",main="First Difference Inflation for United States") # first difference of US inflation
plot.ts(dx[,"IR10FR"],col=couleur,ylab="FD Interest Rates",xlab="Time",main="First Difference Interest Rates for France") # first difference of France interest rate

##### SMOOTHED PERIODOGRAMS
graphics.off() # reset plots
par(mfrow=c(3,1)) # prepare stacked plots
mvspec(dx[,"INFFR"],main="Smoothed Periodogram on FD for inflation, France",kernel=kernel("daniell",7)) # smoothed FD France inflation
mvspec(dx[,"INFUS"],main="Smoothed Periodogram on FD for inflation, US",kernel=kernel("daniell",7)) # smoothed FD US inflation
mvspec(dx[,"IR10FR"],main="Smoothed Periodogram on FD for Interest rate, France",kernel=kernel("daniell",4)) # smoothed FD France interest rate

##################### SEASONAL DIFFERENCES
graphics.off()
par(mfrow=c(3,1))
dINFFRS=diff(dx[,"INFFR"],4) # seasonal difference of differenced France inflation
dINFUSS=diff(dx[,"INFUS"],4) # seasonal difference of differenced US inflation
par(mfrow = c(2,1), mar = c(3,3,2,1))
plot.ts(dINFFRS,col=couleur,ylab="FD Inflation",xlab="Time",main="Seasonal Difference of France Inflation") # seasonal differenced France inflation
plot.ts(dINFUSS,col=couleur,ylab="FD Inflation",xlab="Time",main="Seasonal Difference of US Inflation") # seasonal differenced US inflation

##### FIRST DIFFERENCE PERIODOGRAMS
graphics.off() # reset plots
par(mfrow=c(3,1)) # prepare stacked plots
mvspec(dINFFRS,main="Raw Periodogram for inflation (seasonal and no seasonal FD), France") # seasonal FD France inflation
mvspec(dINFUSS,main="Raw Periodogram for inflation (seasonal and no seasonal FD), US") # seasonal FD US inflation
mvspec(dx[,"IR10FR"],main="Raw Periodogram for Interest rate FD, France") # FD France interest rate

######### CONDITIONAL HETEROSCEDASTICITY : SQUARED DIFFERENCES
par(mfrow = c(1,2), mar = c(3,3,2,1))
plot.ts(dINFFRS^2,col=couleur,ylab="FD Inflation",xlab="Time",main="Squared seasonal FD Inflation for France") # squared seasonal FD France inflation
plot.ts(dINFUSS^2,col=couleur,ylab="FD Inflation",xlab="Time",main="Squared seasonal FD Inflation for United States") # squared seasonal FD US inflation
plot.ts(dx[,"IR10FR"]^2,col=couleur,ylab="FD Interest rate",xlab="Time",main="Squared FD Interest rate for France") # repeated squared FD France interest rate

########### ACF AND PACF
graphics.off()#reset plots
par(mfrow = c(1,1), mar = c(4,4,2,1))
par(mfrow=c(1,2))
acf(x[,"INFFR"],main="ACF Inflation France")#ACF of France inflation
acf(dx[,"INFFR"],main="ACF FD Inflation France")#ACF of differenced France inflation
acf(dINFFRS,main="ACF Seasonal FD Inflation France")#ACF of seasonal differenced France inflation
acf(x[,"INFUS"],main="ACF Inflation US")#ACF of US inflation
acf(dx[,"INFUS"],main="ACF FD Inflation US")#ACF of differenced US inflation
acf(dINFUSS,main="ACF Seasonal FD Inflation US")#ACF of seasonal differenced US inflation
par(mfrow = c(1,1), mar = c(3,3,2,1))
acf(x[,"IR10FR"],main="ACF Interest rate France")#ACF of France interest rate
acf(dx[,"IR10FR"],main="ACF FD Interest rate France")#ACF of differenced France interest rate
par(mfrow = c(1,1),mar = c(3,3,2,1))
pacf(x[,"INFFR"],main="PACF Inflation France")#PACF of France inflation
pacf(x[,"INFUS"],main="PACF Inflation US")#PACF of US inflation
pacf(x[,"IR10FR"],main="PACF Interest rate France")#PACF of France interest rate

######################### QUESTION 3
######################### STATIONARITY
library(urca) # unit root tests
library(strucchange) # Chow break tests for univariate series
dx=diff(x) # first differences again for safety
dINFFR=dx[,"INFFR"] # differenced France inflation
dINFUS=dx[,"INFUS"] # differenced US inflation
dIR10FR=dx[,"IR10FR"] # differenced France interest rate

########## UNIT ROOT TESTS : FRANCE INFLATION
summary(ur.df(INFFR,lags=8,type="trend",selectlags="AIC")) # level with trend
summary(ur.df(INFFR,lags=8,type="drift",selectlags="AIC")) # level with drift
summary(ur.df(INFFR,lags=8,type="none",selectlags="AIC")) # level without deterministic term
summary(ur.df(dINFFR,lags=8,type="trend",selectlags="AIC")) # difference with trend
summary(ur.df(dINFFR,lags=8,type="none",selectlags="AIC")) # difference without deterministic term

########## UNIT ROOT TESTS : US INFLATION
summary(ur.df(INFUS,lags=8,type="trend",selectlags="AIC")) # level with trend
summary(ur.df(INFUS,lags=8,type="drift",selectlags="AIC")) # level with drift
summary(ur.df(INFUS,lags=8,type="none",selectlags="AIC")) # level without deterministic term
summary(ur.df(dINFUS,lags=8,type="trend",selectlags="AIC")) # difference with trend
summary(ur.df(dINFUS,lags=8,type="none",selectlags="AIC")) # difference without deterministic term

########## UNIT ROOT TESTS : FRANCE INTEREST RATE
summary(ur.df(IR10FR,lags=8,type="trend",selectlags="AIC")) # level with trend
summary(ur.df(IR10FR,lags=8,type="drift",selectlags="AIC")) # level with drift
summary(ur.df(IR10FR,lags=8,type="none",selectlags="AIC")) # level without deterministic term
summary(ur.df(dIR10FR,lags=8,type="trend",selectlags="AIC")) # difference with trend
summary(ur.df(dIR10FR,lags=8,type="none",selectlags="AIC")) # difference without deterministic term

############ BREAK TESTS
############ CHOW TEST : FRANCE
t=1:length(INFFR) # time trend for France inflation
sctest(INFFR~t,type="Chow",point=c(1985,1)) # Chow test in level with trend
sctest(dINFFR~1,type="Chow",point=c(1985,1)) # Chow test in first difference

############ CHOW TEST : US
t=1:length(INFUS) # time trend for US inflation
sctest(INFUS~t,type="Chow",point=c(1983,1)) # Chow test in level with trend
sctest(dINFUS~1,type="Chow",point=c(1983,1)) # Chow test in first difference

################ SEASONALITY CHECK
tapply(INFFR,cycle(INFFR),mean,na.rm=TRUE) # average inflation by quarter for France
tapply(INFUS,cycle(INFUS),mean,na.rm=TRUE) # average inflation by quarter for US
tapply(IR10FR,cycle(IR10FR),mean,na.rm=TRUE) # average interest rate by quarter for France

tapply(dINFFR,cycle(dINFFR),mean,na.rm=TRUE) # average inflation by quarter for France
tapply(dINFUS,cycle(dINFUS),mean,na.rm=TRUE) # average inflation by quarter for US
tapply(dIR10FR,cycle(dIR10FR),mean,na.rm=TRUE) # average interest rate by quarter for France

tapply(dINFFRS,cycle(dINFFRS),mean,na.rm=TRUE) # average inflation by quarter for France
tapply(dINFUSS,cycle(dINFUSS),mean,na.rm=TRUE) # average inflation by quarter for US


################################## QUESTION 4
library(astsa) # time series estimation tools
########### ACF AND PACF : HOME INFLATION (FRANCE)
graphics.off() # close all open graphs
par(mfrow = c(1,1), mar = c(3,3,2,1))
acf2(dINFFR,main="Autocorrelation Inflation France") # ACF and PACF of differenced France inflation
acf2(dINFFRS,main="Autocorrelation Inflation France") # ACF and PACF of seasonal differenced France inflation
sarima(INFFR,1,1,1, 3,1,1, 4) # candidate SARIMA model for France
aic_stats=numeric(7) # vector to store AIC values
bic_stats=numeric(7) # vector to store BIC values
for(p in 0:6) { # loop over non-seasonal AR order
  m=sarima(INFFR,p,1,1,3,1,1,4) # estimate SARIMA
  aic_stats[p+1]=m$ICs[1] # save AIC
  bic_stats[p+1]=m$ICs[3] # save BIC
}
which.min(aic_stats)-1 # best p by AIC
which.min(bic_stats)-1 # best p by BIC
sarima(INFFR,7,1,1,3,1,1,4) # selected France model
fcINFFR=sarima.for(INFFR,4, 7,1,1, 3,1,1,4,main="Forecast for Inflation France in 2025",ylab="Inflation",xlab="Time") # 4-quarter forecast for France inflation

########## ACF AND PACF : FOREIGN INFLATION (US)
acf2(dINFUSS,main="Autocorrelation Inflation US") # ACF and PACF of seasonal differenced US inflation
sarima(INFUS,2,1,0, 0,1,1,4) # candidate SARIMA model for US aic_stats=numeric(13) # vector to store AIC values
fcINFUS=sarima.for(INFUS,4,2,1,0,0,1,1,4,main="Forecast for Inflation US in 2025",ylab="Inflation",xlab="Time") # 4-quarter forecast for US inflation

#################### QUESTION 5
library(FinTS) # ARCH tests
library(fGarch) # GARCH estimation

################### CONDITIONAL HETEROSCEDASTICITY : HOME COUNTRY FRANCE
m=sarima(INFFR,7,1,1,3,1,1,4) # estimate SARIMA model for France
ResFR=m$fit$residuals # extract residuals
par(mfrow=c(1,2)) # two plots side by side
plot.ts(ResFR,ylab="Residuals",main="Residuals for SARIMA France") # residuals
plot.ts(ResFR^2,ylab="Residuals",main="Squared residuals for SARIMA France") # squared residuals
Box.test(ResFR,lag=8) # residual autocorrelation test
ResFR2=ResFR^2 # squared residuals
ArchTest(ResFR,8) # Engle ARCH test
Box.test(ResFR2,lag=8) # autocorrelation test on squared residuals

################ SELECT THE BEST ARCH/GARCH MODEL
aic_stats=numeric(3) # store AIC for ARCH orders
bic_stats=numeric(3) # store BIC for ARCH orders
for(p in 1:3) { # loop over ARCH orders
  fit=garchFit(as.formula(paste("~ garch(",p,",0)",sep="")),data=ResFR,include.mean=FALSE,trace=FALSE) # estimate 
  aic_stats[p]=fit@fit$ics["AIC"] # save AIC
  bic_stats[p]=fit@fit$ics["BIC"] # save BIC
}
aic_stats # display AIC values
bic_stats # display BIC values
which.min(aic_stats) # best ARCH order by AIC 3
which.min(bic_stats) # best ARCH order by BIC 2

aic_stats=numeric(3) # reset AIC values
bic_stats=numeric(3) # reset BIC values
for(q in 0:2) { # loop over q with p fixed at 2
  fit=garchFit(as.formula(paste("~ garch(2,",q,")",sep="")),data=ResFR,include.mean=FALSE,trace=FALSE) # estimate 
  aic_stats[q+1]=fit@fit$ics["AIC"] # save AIC
  bic_stats[q+1]=fit@fit$ics["BIC"] # save BIC
}
aic_stats # display AIC values
bic_stats # display BIC values
which.min(aic_stats)-1 # best q by AIC 1
which.min(bic_stats)-1 # best q by BIC 1

################ ESTIMATION
fitFR=garchFit(ResFR~garch(2,1),data=ResFR,include.mean=FALSE,trace=FALSE) # estimate ARCH(2) for France residuals
options(scipen = 3)
summary(fitFR) # ARCH model summary
plot(fitFR) # ARCH model diagnostics
0

######################## SARIMA + GARCH BANDS
fcsar=sarima.for(INFFR,4,7,1,1,3,1,1,4) # step ahead SARIMA forecast
Garchvol=predict(fitFR,n.ahead=4) # forecast GARCH volatility
sdGarch=as.numeric(Garchvol$standardDeviation) # extract forecasted standard deviations
sig0=sd(na.omit(ResFR)) # residual standard deviation from SARIMA
adj=sdGarch/sig0 # volatility adjustment factor
lower_sarima=fcsar$pred-1.96*fcsar$se # lower SARIMA 95% band
upper_sarima=fcsar$pred+1.96*fcsar$se # upper SARIMA 95% band
lower_garch=fcsar$pred-1.96*fcsar$se*adj # lower GARCH-adjusted band
upper_garch=fcsar$pred+1.96*fcsar$se*adj # upper GARCH-adjusted band
forecast_table=cbind(
  forecast=as.numeric(fcsar$pred), # point forecast
  se_sarima=as.numeric(fcsar$se), # SARIMA standard error
  sigma_garch=sdGarch, # GARCH volatility forecast
  lower95_sarima=as.numeric(lower_sarima), # lower SARIMA interval
  upper95_sarima=as.numeric(upper_sarima), # upper SARIMA interval
  lower95_garch=as.numeric(lower_garch), # lower adjusted interval
  upper95_garch=as.numeric(upper_garch) # upper adjusted interval
)
forecast_table # display forecast table

############ PLOT MODIFIED BANDS
forecast_ts=ts(forecast_table[,"forecast"],start=c(2025,1),frequency=4) # forecast series
lower_sar_ts=ts(forecast_table[,"lower95_sarima"],start=c(2025,1),frequency=4) # SARIMA lower band
upper_sar_ts=ts(forecast_table[,"upper95_sarima"],start=c(2025,1),frequency=4) # SARIMA upper band
lower_garch_ts=ts(forecast_table[,"lower95_garch"],start=c(2025,1),frequency=4) # GARCH lower band
upper_garch_ts=ts(forecast_table[,"upper95_garch"],start=c(2025,1),frequency=4) # GARCH upper band
a=1:4 # forecast horizons
ylim_all=range(c(forecast_ts,lower_sar_ts,upper_sar_ts,lower_garch_ts,upper_garch_ts)) # common y-axis range
plot(a,as.numeric(forecast_ts),type="l",col=1,lty=1,ylim=ylim_all,xaxt="n",xlab="Time",ylab="Inflation",main="SARIMA forecast with SARIMA and GARCH-adjusted intervals") # forecast path only
0
lines(a,as.numeric(lower_sar_ts),col=2,lty=2) # lower SARIMA band
lines(a,as.numeric(upper_sar_ts),col=2,lty=2) # upper SARIMA band
lines(a,as.numeric(lower_garch_ts),col=4,lty=1) # lower GARCH-adjusted band
lines(a,as.numeric(upper_garch_ts),col=4,lty=1) # upper GARCH-adjusted band
axis(1,at=a,labels=c("2025 Q1","2025 Q2","2025 Q3","2025 Q4")) # label forecast quarters
legend("topleft",legend=c("Forecast","SARIMA bands","GARCH-adjusted bands"),col=c(1,2,4),lty=c(1,2,1),bty="n") # add legend

############### PLOT THE FULL GRAPH
graphics.off() # clear previous plots
plot(INFFR,pch=1,type="o",xlim=c(1975,2026),ylim=range(INFFR,lower_garch_ts,upper_garch_ts),xlab="Time",ylab="Inflation",main="France inflation forecast with GARCH-adjusted intervals") # full historical plot
polygon(c(time(forecast_ts),rev(time(forecast_ts))),c(as.numeric(upper_garch_ts),rev(as.numeric(lower_garch_ts))),col=gray(0.85),border=NA) # shaded adjusted interval
lines(c(time(INFFR)[length(INFFR)],time(forecast_ts)),c(tail(INFFR,1),as.numeric(forecast_ts)),col=2,type="l") # connect last observation to forecast
points(forecast_ts,col=2,pch=1) # forecast points
points(time(INFFR)[length(INFFR)],tail(INFFR,1),col=1,pch=1) # last observed point
lines(lower_garch_ts,col="gray40") # lower adjusted band
lines(upper_garch_ts,col="gray40") # upper adjusted band

#################### CONDITIONAL HETEROSCEDASTICITY : FOREIGN COUNTRY US
par(mfrow=c(1,1),mar=c(3,4,2,2),oma=c(0,0,2,0)) # single plot window
acf2(dINFUSS) # ACF and PACF for seasonal differenced US inflation
mUS=sarima(INFUS,2,1,0,0,1,1,4) # SARIMA model for US inflation
ResUS=mUS$fit$residuals # residuals from US SARIMA model
par(mfrow=c(1,2)) # two plots side by side
plot.ts(ResUS,ylab="Residuals",main="Residuals for SARIMA US") # US residuals
plot.ts(ResUS^2,ylab="Residuals",main="Squared residuals for SARIMA US") # US squared residuals
Box.test(ResUS,lag=8) # autocorrelation test on US residuals
ResUS2=ResUS^2 # squared US residuals
ArchTest(ResUS,8) # ARCH test for US residuals
Box.test(ResUS2,lag=8) # autocorrelation test on squared US residuals
################ SELECT THE BEST ARCH/GARCH MODEL FOR US
aic_stats=numeric(3) # store AIC for ARCH orders
bic_stats=numeric(3) # store BIC for ARCH orders
for(p in 1:3) { # loop over ARCH orders
  fit=garchFit(as.formula(paste("~ garch(",p,",0)",sep="")),
               data=ResUS,include.mean=FALSE,trace=FALSE) # estimate 
  aic_stats[p]=fit@fit$ics["AIC"] # save AIC
  bic_stats[p]=fit@fit$ics["BIC"] # save BIC
}
aic_stats # display AIC values
bic_stats # display BIC values
which.min(aic_stats) # best ARCH order by AIC
which.min(bic_stats) # best ARCH order by BIC

aic_stats=numeric(3) # reset AIC values
bic_stats=numeric(3) # reset BIC values
for(q in 0:2) { # loop over q with p fixed at 2
  fit=garchFit(as.formula(paste("~ garch(3,",q,")",sep="")),
               data=ResUS,include.mean=FALSE,trace=FALSE) # estimate 
  aic_stats[q+1]=fit@fit$ics["AIC"] # save AIC
  bic_stats[q+1]=fit@fit$ics["BIC"] # save BIC
}
aic_stats # display AIC values
bic_stats # display BIC values
which.min(aic_stats)-1 # best q by AIC
which.min(bic_stats)-1 # best q by BIC

################ ESTIMATION
fitUS=garchFit(ResUS~garch(3,1),data=ResUS,include.mean=FALSE,trace=FALSE) # estimate GARCH for US residuals
options(scipen=3)
summary(fitUS) # GARCH model summary
plot(fitUS) # GARCH model diagnostics
0
######################## SARIMA + GARCH BANDS
fcsarus=sarima.for(INFUS,4,2,1,0,0,1,1,4) # 4-step ahead SARIMA forecast for US
Garchvol=predict(fitUS,n.ahead=4) # forecast GARCH volatility
sdGarch=as.numeric(Garchvol$standardDeviation) # extract forecasted standard deviations
sig0=sd(na.omit(ResUS)) # residual standard deviation from SARIMA
adj=sdGarch/sig0 # volatility adjustment factor
lower_sarima=fcsarus$pred-1.96*fcsarus$se # lower SARIMA 95% band
upper_sarima=fcsarus$pred+1.96*fcsarus$se # upper SARIMA 95% band
lower_garch=fcsarus$pred-1.96*fcsarus$se*adj # lower GARCH-adjusted band
upper_garch=fcsarus$pred+1.96*fcsarus$se*adj # upper GARCH-adjusted band
forecast_table=cbind(
  forecast=as.numeric(fcsarus$pred), # point forecast
  se_sarima=as.numeric(fcsarus$se), # SARIMA standard error
  sigma_garch=sdGarch, # GARCH volatility forecast
  lower95_sarima=as.numeric(lower_sarima), # lower SARIMA interval
  upper95_sarima=as.numeric(upper_sarima), # upper SARIMA interval
  lower95_garch=as.numeric(lower_garch), # lower adjusted interval
  upper95_garch=as.numeric(upper_garch) # upper adjusted interval
)
forecast_table # display forecast table

############ PLOT MODIFIED BANDS
forecast_ts=ts(forecast_table[,"forecast"],start=c(2025,1),frequency=4) # forecast series
lower_sar_ts=ts(forecast_table[,"lower95_sarima"],start=c(2025,1),frequency=4) # SARIMA lower band
upper_sar_ts=ts(forecast_table[,"upper95_sarima"],start=c(2025,1),frequency=4) # SARIMA upper band
lower_garch_ts=ts(forecast_table[,"lower95_garch"],start=c(2025,1),frequency=4) # GARCH lower band
upper_garch_ts=ts(forecast_table[,"upper95_garch"],start=c(2025,1),frequency=4) # GARCH upper band
a=1:4 # forecast horizons
ylim_all=range(c(forecast_ts,lower_sar_ts,upper_sar_ts,lower_garch_ts,upper_garch_ts)) # common y-axis range
plot(a,as.numeric(forecast_ts),type="l",col=1,lty=1,ylim=ylim_all,xaxt="n",
     xlab="Time",ylab="Inflation",
     main="US inflation forecast with SARIMA and GARCH-adjusted intervals") # forecast path only
lines(a,as.numeric(lower_sar_ts),col=2,lty=2) # lower SARIMA band
lines(a,as.numeric(upper_sar_ts),col=2,lty=2) # upper SARIMA band
lines(a,as.numeric(lower_garch_ts),col=4,lty=1) # lower GARCH-adjusted band
lines(a,as.numeric(upper_garch_ts),col=4,lty=1) # upper GARCH-adjusted band
axis(1,at=a,labels=c("2025 Q1","2025 Q2","2025 Q3","2025 Q4")) # label forecast quarters
legend("topleft",legend=c("Forecast","SARIMA bands","GARCH-adjusted bands"),
       col=c(1,2,4),lty=c(1,2,1),bty="n") # add legend

############### PLOT THE FULL GRAPH
graphics.off() # clear previous plots
plot(INFUS,pch=1,type="o",xlim=c(1975,2026),
     ylim=range(INFUS,lower_garch_ts,upper_garch_ts),
     xlab="Time",ylab="Inflation",
     main="US inflation forecast with GARCH-adjusted intervals") # full historical plot
polygon(c(time(forecast_ts),rev(time(forecast_ts))),
        c(as.numeric(upper_garch_ts),rev(as.numeric(lower_garch_ts))),
        col=gray(0.85),border=NA) # shaded adjusted interval
lines(c(time(INFUS)[length(INFUS)],time(forecast_ts)),
      c(tail(INFUS,1),as.numeric(forecast_ts)),
      col=2,type="l") # connect last observation to forecast
points(forecast_ts,col=2,pch=1) # forecast points
points(time(INFUS)[length(INFUS)],tail(INFUS,1),col=1,pch=1) # last observed point
lines(lower_garch_ts,col="gray40") # lower adjusted band
lines(upper_garch_ts,col="gray40") # upper adjusted band



################### QUESTION 6
library(vars)
library("astsa")
library("FinTS")
library("fGarch")
library("rgl")
library("urca")
library("tsDyn")
par(mfrow=c(3,1),mar=c(3,4,2,2),oma=c(0,0,2,0)) # layout for series plots
tsplot(INFFR,col=couleur,ylab="Inflation",xlab="Time",main="Inflation for France") # France inflation
tsplot(INFUS,col=couleur,ylab="Inflation",xlab="Time",main="Inflation for United States") # US inflation
tsplot(IR10FR,col=couleur,ylab="Interest Rate",xlab="Time",main="Interest Rate for France") # France interest rate
plot3d(INFFR,INFUS,IR10FR,ylab="Inflation US",xlab="Inflation France",zlab="Interest Rate France") # 3D plot of all variables

VARselect(x,lag.max=7,type="const") # select lag length for VAR
var5=VAR(x,5,type="const") # estimate VAR with lags
u_hat=ts(residuals(var5)) # extract VAR residuals
plot.ts(u_hat,main="Residuals") # inspect residuals
serial.test(var5,lags.pt=7) # Portmanteau test for residual serial correlation
serial.test(var5,lags.bg=7) # Breusch-Godfrey serial correlation test
arch.test(var5,lags.multi=7) # multivariate ARCH test
normality.test(var5) # residual normality test
cvar.t=ca.jo(x,type="trace",ecdet="const",K=5) # Johansen trace test
summary(cvar.t) # trace test summary
cvar.e=ca.jo(x,type="eigen",ecdet="const",K=5) # Johansen maximum eigenvalue test
summary(cvar.e) # max-eigen test summary
vecm_var=vec2var(cvar.e,r=1) # convert cointegrated system to VAR form
forecast=predict(vecm_var,n.ahead=4,ci=0.95) # 5-step ahead forecast

plot_fc=function(series,name,title) { # function to plot one forecast series
  f=frequency(series) # frequency of the observed series
  s=if(end(series)[2]<f) c(end(series)[1],end(series)[2]+1) else c(end(series)[1]+1,1) # next period
  fc=ts(forecast$fcst[[name]][,1],start=s,frequency=f) # forecast mean
  lo=ts(forecast$fcst[[name]][,2],start=s,frequency=f) # lower bound
  hi=ts(forecast$fcst[[name]][,3],start=s,frequency=f) # upper bound
  obs=window(series,start=c(2000,1)) # observed sample shown on graph
  plot(obs,type="o",pch=1,xlim=c(2000,tail(time(hi),1)),ylim=range(obs,lo,hi,na.rm=TRUE),xlab="Time",ylab="Inflation",main=title) # observed series
  polygon(c(time(fc),rev(time(fc))),c(as.numeric(hi),rev(as.numeric(lo))),col=gray(0.85),border=NA) # shaded forecast interval
  lines(obs,col=1) # observed path on top
  lines(c(tail(time(series),1),time(fc)),c(tail(series,1),as.numeric(fc)),col=2,lwd=2) # connect last observed point to forecast
  lines(lo,col="gray40",lty=2) # lower interval bound
  lines(hi,col="gray40",lty=2) # upper interval bound
  points(fc,col=2,pch=1) # forecast points
  abline(v=tail(time(series),1),lty=3,col="gray50") # mark forecast origin
  legend("topright",legend=c("Observed","Forecast","95% interval"),col=c(1,2,"gray40"),lty=c(1,1,2),pch=c(1,1,NA),bty="n") # legend
}

graphics.off()
par(mfrow=c(2,1),mar=c(3,4,2,2)) # layout for final inflation forecasts
plot_fc(INFFR,"INFFR","Forecast for French Inflation") # forecast plot for France inflation
plot_fc(INFUS,"INFUS","Forecast for US Inflation") # forecast plot for US inflation
plot(forecast) # default forecast plot
