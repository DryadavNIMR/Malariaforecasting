###########################################################################
rm(list=ls())
library(TTR)
library(zoo)
library(plyr)
library(forecast)
library(rlang)
library(xts)
library(imputeTS)
library(dplyr)
library(tidyr)
library(quantmod) # for getSymbols for share price data
library(Quandl)
library(devtools)
library(tseries)
library(ggplot2)
library(ggfortify)
#library(easyGgplot2)
library(ggseas)   # for nzbop data
library(GGally)
library(seasonal)
library(ggpubr)
library(rugarch)
library(FinTS)



#View(data)
###########################################################################
setwd("E:/NIMR/Malaria Data/Monthly Data/Kheda2008-17")

data<-read.csv("data_kheda_anand_dahod_upto2020.csv")

kheda<-subset(data, District=="KHEDA" &                           year!=2020)
dahod<-subset(data, District=="DAHOD" & year!=2010 & year!=2011 & year!=2020)
anand<-subset(data, District=="ANAND" & year!=2008 & year!=2009 & year!=2020)

kheda_2020<-subset(data, District=="KHEDA" & year==2020)
anand_2020<-subset(data, District=="ANAND" & year==2020)
dahod_2020<-subset(data, District=="DAHOD" & year==2020)




##########################################################################


###########################################################################
# Converting to time series object
###########################################################################

########## Data  2020

########## KHEDA
kheda_tc_ts<-ts(kheda_2020[,"Malaria.cases"], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_pf_ts<-ts(kheda_2020[,"pf"], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_pv_ts<-ts(kheda_2020[,"pv"], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_humidity_ts<-ts(kheda_2020[,"humidity"], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_ave.wind_ts<-ts(kheda_2020[,"ave.wind"], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_rainfall.mm._ts<-ts(kheda_2020[,"rainfall.mm."], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_max_temp_ts<-ts(kheda_2020[,"max_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_ave_temp_ts<-ts(kheda_2020[,"ave_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_min_temp_ts<-ts(kheda_2020[,"min_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
kheda_2020_ts<-data.frame(kheda_tc_ts,kheda_pf_ts, kheda_pv_ts, kheda_humidity_ts, kheda_ave.wind_ts, kheda_rainfall.mm._ts, kheda_max_temp_ts, kheda_ave_temp_ts, kheda_min_temp_ts)


########## ANAND
anand_tc_ts<-ts(anand_2020[,"Malaria.cases"], start=c(2020,1), end=c(2020,12), frequency=12)
anand_pf_ts<-ts(anand_2020[,"pf"], start=c(2020,1), end=c(2020,12), frequency=12)
anand_pv_ts<-ts(anand_2020[,"pv"], start=c(2020,1), end=c(2020,12), frequency=12)
anand_humidity_ts<-ts(anand_2020[,"humidity"], start=c(2020,1), end=c(2020,12), frequency=12)
anand_ave.wind_ts<-ts(anand_2020[,"ave.wind"], start=c(2020,1), end=c(2020,12), frequency=12)
anand_rainfall.mm._ts<-ts(anand_2020[,"rainfall.mm."], start=c(2020,1), end=c(2020,12), frequency=12)
anand_max_temp_ts<-ts(anand_2020[,"max_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
anand_ave_temp_ts<-ts(anand_2020[,"ave_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
anand_min_temp_ts<-ts(anand_2020[,"min_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
anand_2020_ts<-data.frame(anand_tc_ts,anand_pf_ts,anand_pv_ts, anand_humidity_ts, anand_ave.wind_ts, anand_rainfall.mm._ts, anand_max_temp_ts, anand_ave_temp_ts, anand_min_temp_ts)



########## DAHOD
# dahod_tc_ts<-ts(dahod_2020[,"Malaria.cases"], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_pf_ts<-ts(dahod_2020[,"pf"], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_pv_ts<-ts(dahod_2020[,"pv"], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_humidity_ts<-ts(dahod_2020[,"humidity"], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_ave.wind_ts<-ts(dahod_2020[,"ave.wind"], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_rainfall.mm._ts<-ts(dahod_2020[,"rainfall.mm."], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_max_temp_ts<-ts(dahod_2020[,"max_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_ave_temp_ts<-ts(dahod_2020[,"ave_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_min_temp_ts<-ts(dahod_2020[,"min_temp"], start=c(2020,1), end=c(2020,12), frequency=12)
# dahod_2020_ts<-data.frame(dahod_tc_ts,dahod_pf_ts,dahod_pv_ts, dahod_humidity_ts, dahod_ave.wind_ts, dahod_rainfall.mm._ts, dahod_max_temp_ts, dahod_ave_temp_ts, dahod_min_temp_ts)


##########DATA up to 2018

########## KHEDA
kheda_tc_ts<-ts(kheda[,"Malaria.cases"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_pf_ts<-ts(kheda[,"pf"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_pv_ts<-ts(kheda[,"pv"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_humidity_ts<-ts(kheda[,"humidity"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_ave.wind_ts<-ts(kheda[,"ave.wind"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_rainfall.mm._ts<-ts(kheda[,"rainfall.mm."], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_max_temp_ts<-ts(kheda[,"max_temp"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_ave_temp_ts<-ts(kheda[,"ave_temp"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_min_temp_ts<-ts(kheda[,"min_temp"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_2019_ts<-data.frame(kheda_tc_ts, kheda_pf_ts,  kheda_pv_ts,kheda_humidity_ts, kheda_ave.wind_ts, kheda_rainfall.mm._ts, kheda_max_temp_ts, kheda_ave_temp_ts, kheda_min_temp_ts)


##########anand
anand_tc_ts<-ts(anand[,"Malaria.cases"], start=c(2010,1), end=c(2019,12), frequency=12)
anand_pf_ts<-ts(anand[,"pf"], start=c(2010,1), end=c(2019,12), frequency=12)
anand_pv_ts<-ts(anand[,"pv"], start=c(2010,1), end=c(2019,12), frequency=12)
anand_humidity_ts<-ts(anand[,"humidity"], start=c(2010,1), end=c(2019,12), frequency=12)
anand_ave.wind_ts<-ts(anand[,"ave.wind"], start=c(2010,1), end=c(2019,12), frequency=12)
anand_rainfall.mm._ts<-ts(anand[,"rainfall.mm."], start=c(2010,1), end=c(2019,12), frequency=12)
anand_max_temp_ts<-ts(anand[,"max_temp"], start=c(2010,1), end=c(2019,12), frequency=12)
anand_ave_temp_ts<-ts(anand[,"ave_temp"], start=c(2010,1), end=c(2019,12), frequency=12)
anand_min_temp_ts<-ts(anand[,"min_temp"], start=c(2010,1), end=c(2019,12), frequency=12)
anand_2019_ts<-data.frame(anand_tc_ts, anand_pf_ts,anand_pv_ts, anand_humidity_ts, anand_ave.wind_ts, anand_rainfall.mm._ts, anand_max_temp_ts, anand_ave_temp_ts, anand_min_temp_ts)


#########dahod
# dahod_tc_ts<-ts(dahod[,"Malaria.cases"], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_pf_ts<-ts(dahod[,"pf"], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_pv_ts<-ts(dahod[,"pv"], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_humidity_ts<-ts(dahod[,"humidity"], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_ave.wind_ts<-ts(dahod[,"ave.wind"], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_rainfall.mm._ts<-ts(dahod[,"rainfall.mm."], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_max_temp_ts<-ts(dahod[,"max_temp"], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_ave_temp_ts<-ts(dahod[,"ave_temp"], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_min_temp_ts<-ts(dahod[,"min_temp"], start=c(2010,1), end=c(2019,12), frequency=12)
# dahod_2019_ts<-data.frame(dahod_tc_ts, dahod_pf_ts,dahod_pv_ts, dahod_humidity_ts, dahod_ave.wind_ts, dahod_rainfall.mm._ts, dahod_max_temp_ts, dahod_ave_temp_ts, dahod_min_temp_ts)


# ##########Full Data kheda
# 
# ########## KHEDA
# kheda_tc_ts<-ts(kheda[,"Malaria.cases"], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_pf_ts<-ts(kheda[,"pf"], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_pv_ts<-ts(kheda[,"pv"], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_humidity_ts<-ts(kheda[,"humidity"], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_ave.wind_ts<-ts(kheda[,"ave.wind"], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_rainfall.mm._ts<-ts(kheda[,"rainfall.mm."], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_max_temp_ts<-ts(kheda[,"max_temp"], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_ave_temp_ts<-ts(kheda[,"ave_temp"], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_min_temp_ts<-ts(kheda[,"min_temp"], start=c(2008,1), end=c(2018,12), frequency=12)
# kheda_ts<-data.frame(kheda_tc_ts,kheda_pf_ts,kheda_pv_ts, kheda_humidity_ts, kheda_ave.wind_ts, kheda_rainfall.mm._ts, kheda_max_temp_ts, kheda_ave_temp_ts, kheda_min_temp_ts)
# 
# ########## ANAND
# anand_tc_ts<-ts(anand[,"Malaria.cases"], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_pf_ts<-ts(anand[,"pf"], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_pv_ts<-ts(anand[,"pv"], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_humidity_ts<-ts(anand[,"humidity"], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_ave.wind_ts<-ts(anand[,"ave.wind"], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_rainfall.mm._ts<-ts(anand[,"rainfall.mm."], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_max_temp_ts<-ts(anand[,"max_temp"], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_ave_temp_ts<-ts(anand[,"ave_temp"], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_min_temp_ts<-ts(anand[,"min_temp"], start=c(2010,1), end=c(2018,12), frequency=12)
# anand_ts<-data.frame(anand_tc_ts,anand_pf_ts,anand_pv_ts, anand_humidity_ts, anand_ave.wind_ts, anand_rainfall.mm._ts, anand_max_temp_ts, anand_ave_temp_ts, anand_min_temp_ts)
# 
# 
# ########## DAHOD
# dahod_tc_ts<-ts(dahod[,"Malaria.cases"], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_pf_ts<-ts(dahod[,"pf"], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_pv_ts<-ts(dahod[,"pv"], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_humidity_ts<-ts(dahod[,"humidity"], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_ave.wind_ts<-ts(dahod[,"ave.wind"], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_rainfall.mm._ts<-ts(dahod[,"rainfall.mm."], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_max_temp_ts<-ts(dahod[,"max_temp"], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_ave_temp_ts<-ts(dahod[,"ave_temp"], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_min_temp_ts<-ts(dahod[,"min_temp"], start=c(2010,1), end=c(2018,12), frequency=12)
# dahod_ts<-data.frame(dahod_tc_ts,dahod_pf_ts,dahod_pv_ts, dahod_humidity_ts, dahod_ave.wind_ts, dahod_rainfall.mm._ts, dahod_max_temp_ts, dahod_ave_temp_ts, dahod_min_temp_ts)


# ###########################################################################
# # Time Series Plot
# ###########################################################################
# 
# ##############Malaria Cases (Plot 1)
# 
# ##########KHEDA
# par(mfrow=c(4,1), oma=c(5,0.1,0.1,6), mai=c(.001, .82,0.1,0.2), xpd=NA)
# plot(kheda_ts[,"kheda_max_temp_ts"], col="blueviolet", lwd=2, lty=1 , xaxt="n", xlab="", frame.plot=F, ylab="Temp", ylim=c(15,45))
# lines(kheda_ts[,"kheda_ave_temp_ts"], col="deeppink", lwd=2, lty=1, xaxt="n")
# lines(kheda_ts[,"kheda_min_temp_ts"], col="green", lwd=2,lty=1, xaxt="n")
# legend("right", legend=c("Max Temp", "Ave. Temp", "Min. Temp"),
#        col=c("blueviolet", "deeppink", "green"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=c(1,1,1))
# plot(kheda_ts[,"kheda_humidity_ts"],col="black", lwd=2, lty=1 , xaxt="n", xlab="", frame.plot=F,ylab="Humidity")
# legend("right", legend=c("Humidity"),
#        col=c("black"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# plot(kheda_ts[,"kheda_rainfall.mm._ts"],col="blue4", lwd=2, lty=1,  xaxt="n", xlab="",  frame.plot=F, ylab="Rain fall")
# legend("right", legend=c("Rainfall (mm)"),
#        col=c("blue4"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# plot(kheda_ts[,"kheda_pv_ts"],col="red", lwd=2, lty=1, xaxt="n", xlab="Year", frame.plot=F, ylab="Malaria cases")
# axis(1, at=seq(2008, 2018, by=1) , labels= seq(2008, 2018, by=1), cex.lab=1.5 )   
# legend("right", legend=c("PV Cases"),
#        col=c("red"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# 
# ##########KHEDA
# par(mfrow=c(4,1), oma=c(5,.1,0.1,6), mai=c(.001, .82,0.1,0.2), xpd=NA)
# plot(anand_ts[,"anand_max_temp_ts"], col="blueviolet", lwd=2, lty=1 , xaxt="n", xlab="", frame.plot=F, ylab="Temp", ylim=c(15,45))
# lines(anand_ts[,"anand_ave_temp_ts"], col="deeppink", lwd=2, lty=1, xaxt="n")
# lines(anand_ts[,"anand_min_temp_ts"], col="green", lwd=2,lty=1, xaxt="n")
# legend("right", legend=c("Max Temp", "Ave. Temp", "Min. Temp"),
#        col=c("blueviolet", "deeppink", "green"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=c(1,1,1))
# plot(anand_ts[,"anand_humidity_ts"],col="black", lwd=2, lty=1 , xaxt="n", xlab="", frame.plot=F,ylab="Humidity")
# legend("right", legend=c("Humidity"),
#        col=c("black"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# plot(anand_ts[,"anand_rainfall.mm._ts"],col="blue4", lwd=2, lty=1,  xaxt="n", xlab="",  frame.plot=F, ylab="Rain fall")
# legend("right", legend=c("Rainfall (mm)"),
#        col=c("blue4"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# plot(anand_ts[,"anand_pv_ts"],col="red", lwd=2, lty=1, xaxt="n", xlab="Year", frame.plot=F, ylab="Malaria cases")
# axis(1, at=seq(2008, 2018, by=1) , labels= seq(2008, 2018, by=1), cex.lab=1.5 )   
# legend("right", legend=c("PV Cases"),
#        col=c("red"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# 
# 
# ##############DAHOD
# par(mfrow=c(4,1), oma=c(5,.1,0.1,6), mai=c(.001, .82,0.1,0.2), xpd=NA)
# plot(dahod_ts[,"dahod_max_temp_ts"], col="blueviolet", lwd=2, lty=1 , xaxt="n", xlab="", frame.plot=F, ylab="Temp", ylim=c(15,45))
# lines(dahod_ts[,"dahod_ave_temp_ts"], col="deeppink", lwd=2, lty=1, xaxt="n")
# lines(dahod_ts[,"dahod_min_temp_ts"], col="green", lwd=2,lty=1, xaxt="n")
# legend("right", legend=c("Max Temp", "Ave. Temp", "Min. Temp"),
#        col=c("blueviolet", "deeppink", "green"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=c(1,1,1))
# plot(dahod_ts[,"dahod_humidity_ts"],col="black", lwd=2, lty=1 , xaxt="n", xlab="", frame.plot=F,ylab="Humidity")
# legend("right", legend=c("Humidity"),
#        col=c("black"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# plot(dahod_ts[,"dahod_rainfall.mm._ts"],col="blue4", lwd=2, lty=1,  xaxt="n", xlab="",  frame.plot=F, ylab="Rain fall")
# legend("right", legend=c("Rainfall (mm)"),
#        col=c("blue4"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# plot(dahod_ts[,"dahod_pv_ts"],col="red", lwd=2, lty=1, xaxt="n", xlab="Year", frame.plot=F, ylab="Malaria cases")
# axis(1, at=seq(2008, 2018, by=1) , labels= seq(2008, 2018, by=1), cex.lab=1.5 )   
# legend("right", legend=c("PV Cases"),
#        col=c("red"),  cex=0.9,
#        horiz=F,  title="", inset = c(-.35,0) , bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=1)
# 
# 
# 
# ##########Seasonal Plot (Plot 2)
# 
# ########Kheda
# par(mfrow=c(3,2), oma=c(1,.1,0.1,6), mai=c(.4, .4,0.4,0.2), xpd=NA)
# seasonplot(kheda_pv_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Malaria Cases", year.labels=F, xlab="", frame.plot=F)
# seasonplot(kheda_max_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Max. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# seasonplot(kheda_humidity_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Humidity", year.labels=F, frame.plot=F, xlab="")
# seasonplot(kheda_ave_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Ave. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# seasonplot(kheda_rainfall.mm._ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Rainfall", year.labels=F, frame.plot=F, xlab="")
# seasonplot(kheda_min_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Min. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# par(mfrow=c(1,1))
# legend("right", legend=c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
#        col=rainbow(11),  cex=0.9,
#        horiz=F,  title="", inset = c(-.48,0) , bty = "n", text.font = 3, text.width = 3.5, lwd=1, lty=1)
# 
# ########anand
# par(mfrow=c(3,2), oma=c(1,.1,0.1,6), mai=c(.4, .4,0.4,0.2), xpd=NA)
# seasonplot(anand_pv_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Malaria Cases", year.labels=F, xlab="", frame.plot=F)
# seasonplot(anand_max_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Max. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# seasonplot(anand_humidity_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Humidity", year.labels=F, frame.plot=F, xlab="")
# seasonplot(anand_ave_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Ave. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# seasonplot(anand_rainfall.mm._ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Rainfall", year.labels=F, frame.plot=F, xlab="")
# seasonplot(anand_min_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Min. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# par(mfrow=c(1,1))
# legend("right", legend=c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
#        col=rainbow(11),  cex=0.9,
#        horiz=F,  title="", inset = c(-.48,0) , bty = "n", text.font = 3, text.width = 3.5, lwd=1, lty=1)
# 
# 


#help("seasonplot")
# ########dahod
# par(mfrow=c(3,2), oma=c(1,.1,0.1,6), mai=c(.4, .4,0.4,0.2), xpd=NA)
# seasonplot(dahod_pv_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Malaria Cases", year.labels=F, xlab="", frame.plot=F)
# seasonplot(dahod_max_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Max. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# seasonplot(dahod_humidity_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Humidity", year.labels=F, frame.plot=F, xlab="")
# seasonplot(dahod_ave_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Ave. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# seasonplot(dahod_rainfall.mm._ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Rainfall", year.labels=F, frame.plot=F, xlab="")
# seasonplot(dahod_min_temp_ts, type="l" , col=rainbow(11), lty=1:10,lwd=1, main="Min. Temperature", year.labels=F, frame.plot=F, xlab="",ylim=c(15,45))
# par(mfrow=c(1,1))
# legend("right", legend=c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"),
#        col=rainbow(11),  cex=0.9,
#        horiz=F,  title="", inset = c(-.48,0) , bty = "n", text.font = 3, text.width = 3.5, lwd=1, lty=1)
# 
# 
# 
# 
# 
# ###############################################################
# ##########Some Simple forecasting Techniques
# ###############################################################
# 
# kheda_mean_forecast <- meanf(kheda_2016_ts$kheda_pv_ts,h=12)
# kheda_Seasonal_naive_forecast <- snaive(kheda_2016_ts$kheda_pv_ts,h=12)
# kheda_random_walk_forecast <- rwf(kheda_2016_ts$kheda_pv_ts,h=12)
# 
# anand_mean_forecast <- meanf(anand_2016_ts$anand_pv_ts,h=12)
# anand_Seasonal_naive_forecast <- snaive(anand_2016_ts$anand_pv_ts,h=12)
# anand_random_walk_forecast <- rwf(anand_2016_ts$anand_pv_ts,h=12)
# 
# dahod_mean_forecast <- meanf(dahod_2016_ts$dahod_pv_ts,h=12)
# dahod_Seasonal_naive_forecast <- snaive(dahod_2016_ts$dahod_pv_ts,h=12)
# dahod_random_walk_forecast <- rwf(dahod_2016_ts$dahod_pv_ts,h=12)


# kheda_random_walk_forecast$mean
# anand_random_walk_forecast$mean
# dahod_random_walk_forecast$mean


#########kheda
# par(mar=c(5, 5, 6, 5), xpd=NA)
# plot(kheda_2016_ts$kheda_pv_ts,col="forestgreen", yaxt="n", xaxt="n", xlab="",   ylab="",  pch=19, cex.axis = 1.5, cex.lab = 3,lty=1, frame.plot=FALSE, lwd=1)
# lines(kheda_2017_ts$kheda_pv_ts, col="forestgreen", lty=1, lwd=3)
# lines(kheda_mean_forecast$mean, lwd=3, col="darkgoldenrod1", lty=3)
# lines(kheda_random_walk_forecast$mean, lwd=3, col="brown1", lty=3)
# lines(kheda_Seasonal_naive_forecast$mean, lwd=3, col= "blue4", lty=3)
# title("Simple forecasting techniques", xlab="Years",ylab="Malaria Cases",  cex.lab = 1.2)
# axis(1, at=seq(2008, 2018, by=1) , labels= seq(2008, 2018, by=1),cex.axis=1.0,  lwd=1.5)   
# axis(2, cex.axis=1.0,lwd=1.5) 
# abline(v=2017, lty=20, xpd=F, lwd=1)
# legend("topleft", legend=c("Time series",  "Mean Forecast", "Random Walk Forecast", "Seasonal Naive Forecast"),
#        col=c("forestgreen", "darkgoldenrod1", "brown1", "blue4"),  cex=0.9,
#        horiz=F,  title="", inset =c(.005,-.25), bty = "n", text.font = 3, text.width = 1.5, lwd=3, lty=c(1,3,3,3,3), xpd=TRUE)


# #########anand
# par(mar=c(5, 5, 6, 5), xpd=NA)
# plot(anand_2016_ts$anand_pv_ts,col="forestgreen", yaxt="n", xaxt="n", xlab="",   ylab="",  pch=19, cex.axis = 1.5, cex.lab = 3,lty=1, frame.plot=FALSE, lwd=1)
# lines(anand_2017_ts$anand_pv_ts, col="forestgreen", lty=1, lwd=3)
# lines(anand_mean_forecast$mean, lwd=3, col="darkgoldenrod1", lty=3)
# lines(anand_random_walk_forecast$mean, lwd=3, col="brown1", lty=3)
# lines(anand_Seasonal_naive_forecast$mean, lwd=3, col= "blue4", lty=3)
# title("Simple forecasting techniques", xlab="Years",ylab="Malaria Cases",  cex.lab = 1.2)
# axis(1, at=seq(2008, 2018, by=1) , labels= seq(2008, 2018, by=1),cex.axis=1.0,  lwd=1.5)   
# axis(2, cex.axis=1.0,lwd=1.5) 
# abline(v=2017, lty=20, xpd=F, lwd=1)
# legend("topleft", legend=c("Time series",  "Mean Forecast", "Random Walk Forecast", "Seasonal Naive Forecast"),
#        col=c("forestgreen", "darkgoldenrod1", "brown1", "blue4"),  cex=0.9,
#        horiz=F,  title="", inset =c(.005,-.25), bty = "n", text.font = 3, text.width = 1.5, lwd=3, lty=c(1,3,3,3,3), xpd=TRUE)
# 
# #########dahod
# par(mar=c(5, 5, 6, 5), xpd=NA)
# plot(dahod_2016_ts$dahod_pv_ts,col="forestgreen", yaxt="n", xaxt="n", xlab="",   ylab="",  pch=19, cex.axis = 1.5, cex.lab = 3,lty=1, frame.plot=FALSE, lwd=1)
# lines(dahod_2017_ts$dahod_pv_ts, col="forestgreen", lty=1, lwd=3)
# lines(dahod_mean_forecast$mean, lwd=3, col="darkgoldenrod1", lty=3)
# lines(dahod_random_walk_forecast$mean, lwd=3, col="brown1", lty=3)
# lines(dahod_Seasonal_naive_forecast$mean, lwd=3, col= "blue4", lty=3)
# title("Simple forecasting techniques", xlab="Years",ylab="Malaria Cases",  cex.lab = 1.2)
# axis(1, at=seq(2008, 2018, by=1) , labels= seq(2008, 2018, by=1),cex.axis=1.0,  lwd=1.5)   
# axis(2, cex.axis=1.0,lwd=1.5) 
# abline(v=2017, lty=20, xpd=F, lwd=1)
# legend("topleft", legend=c("Time series",  "Mean Forecast", "Random Walk Forecast", "Seasonal Naive Forecast"),
#        col=c("forestgreen", "darkgoldenrod1", "brown1", "blue4"),  cex=0.9,
#        horiz=F,  title="", inset =c(.005,-.25), bty = "n", text.font = 3, text.width = 1.5, lwd=3, lty=c(1,3,3,3,3), xpd=TRUE)
# 
# 
#########Combined

# par(mar=c(5, 5, 6, 5), xpd=NA)
# #plot(kheda_2016_ts$kheda_tc_ts,col="forestgreen", yaxt="n", xaxt="n", xlab="",   ylab="",  pch=19, cex.axis = 1.5, cex.lab = 3,lty=1, frame.plot=FALSE, lwd=1)
# plot.ts(kheda_2017_ts$kheda_pf_ts)
# dahod_2017_ts$dahod_pf_ts
# dahod_mean_forecast$mean
# dahod_random_walk_forecast$mean
# dahod_Seasonal_naive_forecast$mean
# 
# plot(kheda_2017_ts$kheda_pf_ts, col="forestgreen", lty=1, lwd=3)
# axis(1, at=seq(2017.1, 2017.8, by=.1) , labels=seq(1:12) , cex.axis=1.0,  lwd=1.5)   
# 
# kheda_Seasonal_naive_forecast$mean
# 
# lines(kheda_mean_forecast$mean, lwd=3, col="darkgoldenrod1", lty=3)
# lines(kheda_random_walk_forecast$mean, lwd=3, col="brown1", lty=3)
# lines(kheda_Seasonal_naive_forecast$mean, lwd=3, col= "blue4", lty=3)
# title("Simple forecasting techniques", xlab="Years",ylab="Malaria Cases",  cex.lab = 1.2)
# axis(1, at=seq(1, 12, by=1) , labels=seq(1:12) , cex.axis=1.0,  lwd=1.5)   
# axis(2, cex.axis=1.0,lwd=1.5) 
# abline(v=2017, lty=20, xpd=F, lwd=1)
# legend("topleft", legend=c("Time series",  "Mean Forecast", "Random Walk Forecast", "Seasonal Naive Forecast"),
#        col=c("forestgreen", "darkgoldenrod1", "brown1", "blue4"),  cex=0.9,
#        horiz=F,  title="", inset =c(.005,-.25), bty = "n", text.font = 3, text.width = 1.5, lwd=3, lty=c(1,3,3,3,3), xpd=TRUE)

########Residul analysis

# res_snaive<-residuals(Seasonal_naive_forecast)
# res_mean<-residuals(mean_forecast)
# res_random<-residuals(random_walk_forecast)
# 
# plot(res_mean,col="blue",ylim=c(-150, 300), lwd=2)
# lines(res_snaive,col="red",ylim=c(-150, 300), lwd=2)
# lines(res_random,col="black",ylim=c(-150, 300), lwd=2)
# 
# ########Accuracy
# 
# accuracy(mean_forecast,kheda_2017 )
# accuracy(Seasonal_naive_forecast,kheda_2017 )
# accuracy(random_walk_forecast,kheda_2017 )


#########
# names(kheda)
# 
# autoplot(kheda_ts[,c(3,5,6,10, 11 ,12 )])+theme_classic()
# kheda_ts[,rainfall.mm.]<-kheda_ts[,rainfall.mm.<=300]
# kheda[rainfall.mm.>=300]<-NA



########################
# kheda$Malaria.cases[kheda$Malaria.cases==259]<-NA
# kheda$rainfall.mm.[kheda$rainfall.mm.>=350]<-NA
# kheda$ave.wind[kheda$ave.wind>20]<-NA
# kheda$ave.gust[kheda$ave.gust>30]<-NA
# 
# 
# p1<-ggplot(kheda, aes( x=rainfall.mm., y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="black", size=1.0, fill="green", method="lm")+theme_classic()+labs(x="Rain fall (mm)", y="Malaria Cases") +stat_cor()
# p2<-ggplot(kheda, aes( x=humidity, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="black", size=1.0, fill="green", method="lm")+theme_classic()+labs(x="Humidity (%)", y="Malaria Cases") +stat_cor() #+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p3<-ggplot(kheda, aes( x=max_temp, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="black", size=1.0, fill="red", method="lm")+theme_classic()+labs(x="Maximum Temp.", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p4<-ggplot(kheda, aes( x=ave_temp, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="black", size=1.0, fill="red", method="lm")+theme_classic()+labs(x="Average Temp.", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p5<-ggplot(kheda, aes( x=min_temp, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="black", size=1.0, fill="green", method="lm")+theme_classic()+labs(x="Minimum Temp.", y="Malaria Cases") +stat_cor()
# p6<-ggplot(kheda, aes( x=max.wind, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="black", size=1.0, fill="red", method="lm")+theme_classic()+labs(x="Maximum wind speed", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p7<-ggplot(kheda, aes( x=ave.wind, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="black", size=1.0, fill="red", method="lm")+theme_classic()+labs(x="Average wind speed", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p8<-ggplot(kheda, aes( x=ave.gust, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="black", size=1.0, fill="red", method="lm")+theme_classic()+labs(x="Average gust", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# ggplot2.multiplot(p1, p2,p3,p4,p5,p6,p7,p8, cols=4)
# ggarrange(p1, p2,p3,p4,p5,p6,p7,p8, ncols=4)
# 
# 
# ggarrange(p1, p2,p3,p4,p5,p6,p7,p8, ncol=4, nrow=2)
############### With Linear Fit

# p1<-ggplot(kheda, aes( x=rainfall.mm., y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="red", size=1.0, fill="red", alpha=.5)+ geom_smooth(col="green", size=1.0, fill="green", method="lm", alpha=.5)+theme_classic()+labs(x="Rain fall (mm)", y="Malaria Cases")  +stat_cor()
# p2<-ggplot(kheda, aes( x=humidity, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="red", size=1.0, fill="red", alpha=.5)+ geom_smooth(col="green", size=1.0, fill="green", method="lm", alpha=.5)+theme_classic()+labs(x="Humidity (%)", y="Malaria Cases") +stat_cor() #+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p3<-ggplot(kheda, aes( x=max_temp, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="red", size=1.0, fill="red", alpha=.5)+ geom_smooth(col="green", size=1.0, fill="green", method="lm", alpha=.5)+theme_classic()+labs(x="Maximum Temp.", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p4<-ggplot(kheda, aes( x=ave_temp, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="red", size=1.0, fill="red", alpha=.5)+ geom_smooth(col="green", size=1.0, fill="green", method="lm", alpha=.5)+theme_classic()+labs(x="Average Temp.", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p5<-ggplot(kheda, aes( x=min_temp, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="red", size=1.0, fill="red", alpha=.5)+ geom_smooth(col="green", size=1.0, fill="green", method="lm", alpha=.5)+theme_classic()+labs(x="Minimum Temp.", y="Malaria Cases") +stat_cor()
# p6<-ggplot(kheda, aes( x=max.wind, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="red", size=1.0, fill="red", alpha=.5)+ geom_smooth(col="green", size=1.0, fill="green", method="lm", alpha=.5)+theme_classic()+labs(x="Maximum wind speed", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p7<-ggplot(kheda, aes( x=ave.wind, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="red", size=1.0, fill="red", alpha=.5)+ geom_smooth(col="green", size=1.0, fill="green", method="lm", alpha=.5)+theme_classic()+labs(x="Average wind speed", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# p8<-ggplot(kheda, aes( x=ave.gust, y=Malaria.cases))+geom_point(shape=16, size=1)+geom_smooth(col="red", size=1.0, fill="red", alpha=.5)+ geom_smooth(col="green", size=1.0, fill="green", method="lm", alpha=.5)+theme_classic()+labs(x="Average gust", y="Malaria Cases") +stat_cor()#+theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank(), axis.line.y=element_blank())
# ggplot2.multiplot(p1, p2,p3,p4,p5,p6,p7,p8, cols=4)



#////////////////////////////////////////////////////////////////////////#
##### Observed Cases
#////////////////////////////////////////////////////////////////////////#

# View(kheda_2017_ts$kheda_pv_ts)
# View(kheda_2017_ts$kheda_pf_ts)
# View(kheda_2017_ts$kheda_tc_ts)
# 
# View(anand_2017_ts$anand_pv_ts)
# View(anand_2017_ts$anand_pf_ts)
# View(anand_2017_ts$anand_tc_ts)
# 
# View(dahod_2017_ts$dahod_pv_ts)
# View(dahod_2017_ts$dahod_pf_ts)
# View(dahod_2017_ts$dahod_tc_ts)


#////////////////////////////////////////////////////////////////////////#
##### Regression model between rainfal, humidity and malaria cases
#////////////////////////////////////////////////////////////////////////#

fit.model_kheda_pv <- tslm(kheda_pv_ts~kheda_humidity_ts+kheda_rainfall.mm._ts+kheda_min_temp_ts, data=kheda_2019_ts)
fit.model_anand_pv <- tslm(anand_pv_ts~anand_humidity_ts+anand_rainfall.mm._ts+anand_min_temp_ts, data=anand_2019_ts)
# fit.model_dahod_pv <- tslm(dahod_pv_ts~dahod_humidity_ts+dahod_rainfall.mm._ts+dahod_min_temp_ts, data=dahod_2019_ts)

fit.model_kheda_pf <- tslm(kheda_pf_ts~kheda_humidity_ts+kheda_rainfall.mm._ts+kheda_min_temp_ts, data=kheda_2019_ts)
fit.model_anand_pf <- tslm(anand_pf_ts~anand_humidity_ts+anand_rainfall.mm._ts+anand_min_temp_ts, data=anand_2019_ts)
# fit.model_dahod_pf <- tslm(dahod_pf_ts~dahod_humidity_ts+dahod_rainfall.mm._ts+dahod_min_temp_ts, data=dahod_2019_ts)

fit.model_kheda_tc <- tslm(kheda_tc_ts~kheda_humidity_ts+kheda_rainfall.mm._ts+kheda_min_temp_ts, data=kheda_2019_ts)
fit.model_anand_tc <- tslm(anand_tc_ts~anand_humidity_ts+anand_rainfall.mm._ts+anand_min_temp_ts, data=anand_2019_ts)
# fit.model_dahod_tc <- tslm(dahod_tc_ts~dahod_humidity_ts+dahod_rainfall.mm._ts+dahod_min_temp_ts, data=dahod_2019_ts)


summary(fit.model_kheda_pv)
summary(fit.model_anand_pv)
# summary(fit.model_dahod_pv)

summary(fit.model_kheda_pf)
summary(fit.model_anand_pf)
# summary(fit.model_dahod_pf)

summary(fit.model_kheda_tc)
summary(fit.model_anand_tc)
# summary(fit.model_dahod_tc)




fcast_kheda_pv = forecast(fit.model_kheda_pv, h=12, newdata=kheda_2020_ts) 
fcast_anand_pv = forecast(fit.model_anand_pv, h=12, newdata=anand_2020_ts) 
# fcast_dahod_pv = forecast(fit.model_dahod_pv, h=12, newdata=dahod_2020_ts) 

fcast_kheda_pf = forecast(fit.model_kheda_pf, h=12, newdata=kheda_2020_ts) 
fcast_anand_pf = forecast(fit.model_anand_pf, h=12, newdata=anand_2020_ts) 
# fcast_dahod_pf = forecast(fit.model_dahod_pf, h=12, newdata=dahod_2020_ts) 

fcast_kheda_tc = forecast(fit.model_kheda_tc, h=12, newdata=kheda_2020_ts) 
fcast_anand_tc = forecast(fit.model_anand_tc, h=12, newdata=anand_2020_ts) 
# fcast_dahod_tc = forecast(fit.model_dahod_tc, h=12, newdata=dahod_2020_ts) 


TC_Reg_kheda_pv<-data.frame(round(fcast_kheda_pv$mean), round(fcast_kheda_pv$lower[,1]) , round(fcast_kheda_pv$upper[,1]))
TC_Reg_kheda_pf<-data.frame(round(fcast_kheda_pf$mean), round(fcast_kheda_pf$lower[,1]) , round(fcast_kheda_pf$upper[,1]))
TC_Reg_kheda_tc<-data.frame(round(fcast_kheda_tc$mean), round(fcast_kheda_tc$lower[,1]) , round(fcast_kheda_tc$upper[,1]))

TC_Reg_anand_pv<-data.frame(round(fcast_anand_pv$mean), round(fcast_anand_pv$lower[,1]) , round(fcast_anand_pv$upper[,1]))
TC_Reg_anand_pf<-data.frame(round(fcast_anand_pf$mean), round(fcast_anand_pf$lower[,1]) , round(fcast_anand_pf$upper[,1]))
 TC_Reg_anand_tc<-data.frame(round(fcast_anand_tc$mean), round(fcast_anand_tc$lower[,1]) , round(fcast_anand_tc$upper[,1]))

# TC_Reg_dahod_pv<-data.frame(round(fcast_dahod_pv$mean), round(fcast_dahod_pv$lower[,1]) , round(fcast_dahod_pv$upper[,1]))
# TC_Reg_dahod_pf<-data.frame(round(fcast_dahod_pf$mean), round(fcast_dahod_pf$lower[,1]) , round(fcast_dahod_pf$upper[,1]))
# TC_Reg_dahod_tc<-data.frame(round(fcast_dahod_tc$mean), round(fcast_dahod_tc$lower[,1]) , round(fcast_dahod_tc$upper[,1]))


# View(TC_Reg_kheda_pv)
# View(TC_Reg_kheda_pf)
# View(TC_Reg_kheda_tc)
# 
# View(TC_Reg_anand_pv)
# View(TC_Reg_anand_pf)
# View(TC_Reg_anand_tc)
# 
# View(TC_Reg_dahod_pv)
# View(TC_Reg_dahod_pf)
# View(TC_Reg_dahod_tc)

# View(accuracy(fit.model_kheda_pv))
# View(accuracy(fit.model_kheda_pf))
# View(accuracy(fit.model_kheda_tc))
# 
# View(accuracy(fit.model_anand_pv))
# View(accuracy(fit.model_anand_pf))
# View(accuracy(fit.model_anand_tc))

# View(accuracy(fit.model_dahod_pv))
# View(accuracy(fit.model_dahod_pf))
# View(accuracy(fit.model_dahod_tc))


# par(mar=c(4, 4, 4, 5), xpd=T)
# plot(kheda_2016_ts$kheda_pv_ts, col="deeppink1", lwd=2,frame.plot=F, yaxt="n", xaxt="n", xlab="",   ylab="", cex.axis = 1.5, cex.lab = 2, data=kheda_2016_ts)
# lines(kheda_2017_ts$kheda_pv_ts, col="Black", series="Data", lwd=2.5, lty=1)
# lines(fitted(fit.model_kheda), col="darkorchid", series="Data", lwd=2.5, lty=1)
# lines(fcast_kheda$mean, col="blue", series="Data", lwd=2.5, lty=1)
# lines(fcast_kheda$lower[,1], col="gray", lwd=2.5, lty=1)
# lines(fcast_kheda$upper[,1], col="gray", series="Data", lwd=2.5, lty=1)
# title("Prediction of Malaria Cases Using Linear Model", xlab="Years",ylab="Malaria Cases",  cex.lab = 1.2)
# axis(1, at=seq(2008, 2018, by=1) , labels= seq(2008, 2018, by=1),cex.axis=1.0,  lwd=1.5)   
# axis(2, cex.axis=1.0,lwd=1.5) 
# legend("topleft", legend=c("Observed cases", "Predicted Cases"),
#        col=c("deeppink1", "darkorchid"),  cex=0.9,
#        horiz=F,  title="", bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=c(1,1) )
# abline(v=2017, lty=20, xpd=F, lwd=1)
# legend("topright", inset = c(-.40,0),  legend=c("Actual ", "Forecasted", "80% CI"),
#        col=c("black", "blue", "gray", "gray"),  cex=0.9,
#        horiz=F,  title="", bty = "n", text.font = 4, text.width = 3.5, lwd=2 )




#////////////////////////////////////////////////////////////////////////#
##### Regression model With trend and seasonal component
#////////////////////////////////////////////////////////////////////////#

fit.model_S_T_kheda_pv <- tslm(kheda_pv_ts~kheda_humidity_ts+kheda_rainfall.mm._ts+kheda_min_temp_ts+trend+season, data=kheda_2019_ts)
fit.model_S_T_anand_pv <- tslm(anand_pv_ts~anand_humidity_ts+anand_rainfall.mm._ts+anand_min_temp_ts+trend+season, data=anand_2019_ts)
# fit.model_S_T_dahod_pv <- tslm(dahod_pv_ts~dahod_humidity_ts+dahod_rainfall.mm._ts+dahod_min_temp_ts+trend+season, data=dahod_2019_ts)

fit.model_S_T_kheda_pf <- tslm(kheda_pf_ts~kheda_humidity_ts+kheda_rainfall.mm._ts+kheda_min_temp_ts+trend+season, data=kheda_2019_ts)
fit.model_S_T_anand_pf <- tslm(anand_pf_ts~anand_humidity_ts+anand_rainfall.mm._ts+anand_min_temp_ts+trend+season, data=anand_2019_ts)
# fit.model_S_T_dahod_pf <- tslm(dahod_pf_ts~dahod_humidity_ts+dahod_rainfall.mm._ts+dahod_min_temp_ts+trend+season, data=dahod_2019_ts)

fit.model_S_T_kheda_tc <- tslm(kheda_tc_ts~kheda_humidity_ts+kheda_rainfall.mm._ts+kheda_min_temp_ts+trend+season, data=kheda_2019_ts)
fit.model_S_T_anand_tc <- tslm(anand_tc_ts~anand_humidity_ts+anand_rainfall.mm._ts+anand_min_temp_ts+trend+season, data=anand_2019_ts)
# fit.model_S_T_dahod_tc <- tslm(dahod_tc_ts~dahod_humidity_ts+dahod_rainfall.mm._ts+dahod_min_temp_ts+trend+season, data=dahod_2019_ts)


fcast_S_T_kheda_pv = forecast(fit.model_S_T_kheda_pv, h=12, newdata=kheda_2020_ts) 
fcast_S_T_anand_pv = forecast(fit.model_S_T_anand_pv, h=12, newdata=anand_2020_ts) 
# fcast_S_T_dahod_pv = forecast(fit.model_S_T_dahod_pv, h=12, newdata=dahod_2020_ts) 

fcast_S_T_kheda_pf = forecast(fit.model_S_T_kheda_pf, h=12, newdata=kheda_2020_ts) 
fcast_S_T_anand_pf = forecast(fit.model_S_T_anand_pf, h=12, newdata=anand_2020_ts) 
# fcast_S_T_dahod_pf = forecast(fit.model_S_T_dahod_pf, h=12, newdata=dahod_2020_ts) 

fcast_S_T_kheda_tc = forecast(fit.model_S_T_kheda_tc, h=12, newdata=kheda_2020_ts) 
fcast_S_T_anand_tc = forecast(fit.model_S_T_anand_tc, h=12, newdata=anand_2020_ts) 
# fcast_S_T_dahod_tc = forecast(fit.model_S_T_dahod_tc, h=12, newdata=dahod_2020_ts) 


TC_Reg_trend_kheda_pv<-data.frame(round(fcast_S_T_kheda_pv$mean), round(fcast_S_T_kheda_pv$lower[,1]) , round(fcast_S_T_kheda_pv$upper[,1]))
TC_Reg_trend_kheda_pf<-data.frame(round(fcast_S_T_kheda_pf$mean), round(fcast_S_T_kheda_pf$lower[,1]) , round(fcast_S_T_kheda_pf$upper[,1]))
TC_Reg_trend_kheda_tc<-data.frame(round(fcast_S_T_kheda_tc$mean), round(fcast_S_T_kheda_tc$lower[,1]) , round(fcast_S_T_kheda_tc$upper[,1]))

TC_Reg_trend_anand_pv<-data.frame(round(fcast_S_T_anand_pv$mean), round(fcast_S_T_anand_pv$lower[,1]) , round(fcast_S_T_anand_pv$upper[,1]))
TC_Reg_trend_anand_pf<-data.frame(round(fcast_S_T_anand_pf$mean), round(fcast_S_T_anand_pf$lower[,1]) , round(fcast_S_T_anand_pf$upper[,1]))
TC_Reg_trend_anand_tc<-data.frame(round(fcast_S_T_anand_tc$mean), round(fcast_S_T_anand_tc$lower[,1]) , round(fcast_S_T_anand_tc$upper[,1]))

# TC_Reg_trend_dahod_pv<-data.frame(round(fcast_S_T_dahod_pv$mean), round(fcast_S_T_dahod_pv$lower[,1]) , round(fcast_S_T_dahod_pv$upper[,1]))
# TC_Reg_trend_dahod_pf<-data.frame(round(fcast_S_T_dahod_pf$mean), round(fcast_S_T_dahod_pf$lower[,1]) , round(fcast_S_T_dahod_pf$upper[,1]))
# TC_Reg_trend_dahod_tc<-data.frame(round(fcast_S_T_dahod_tc$mean), round(fcast_S_T_dahod_tc$lower[,1]) , round(fcast_S_T_dahod_tc$upper[,1]))


# View(TC_Reg_trend_kheda_pv)
# View(TC_Reg_trend_kheda_pf)
# View(TC_Reg_trend_kheda_tc)
# 
# View(TC_Reg_trend_anand_pv)
# View(TC_Reg_trend_anand_pf)
# View(TC_Reg_trend_anand_tc)
# 
# View(TC_Reg_trend_dahod_pv)
# View(TC_Reg_trend_dahod_pf)
# View(TC_Reg_trend_dahod_tc)
# 


# View(accuracy(fit.model_S_T_kheda_pv))
# View(accuracy(fit.model_S_T_kheda_pf))
# View(accuracy(fit.model_S_T_kheda_tc))
# 
# View(accuracy(fit.model_S_T_anand_pv))
# View(accuracy(fit.model_S_T_anand_pf))
# View(accuracy(fit.model_S_T_anand_tc))
# 
# View(accuracy(fit.model_S_T_dahod_pv))
# View(accuracy(fit.model_S_T_dahod_pf))
# View(accuracy(fit.model_S_T_dahod_tc))


# par(mar=c(4, 4, 4, 5), xpd=T)
# plot(kheda_2016_ts$tc_kheda_ts, col="deeppink1", lwd=2,frame.plot=F, yaxt="n", xaxt="n", xlab="",   ylab="", cex.axis = 1.5, cex.lab = 2, data=kheda_2016_ts)
# lines(kheda_2017_ts$tc_kheda_ts, col="Black", series="Data", lwd=2.5, lty=1)
# lines(fitted(fit.model), col="darkorchid", series="Data", lwd=2.5, lty=1)
# lines(fcast$mean, col="blue", series="Data", lwd=3, lty=3)
# lines(fcast$lower[,1], col="gray", lwd=2.5, lty=1)
# lines(fcast$upper[,1], col="gray", series="Data", lwd=2.5, lty=1)
# title("Prediction of Malaria Cases Using Linear Model", xlab="Years",ylab="Malaria Cases",  cex.lab = 1.2)
# axis(1, at=seq(2008, 2018, by=1) , labels= seq(2008, 2018, by=1),cex.axis=1.0,  lwd=1.5)   
# axis(2, cex.axis=1.0,lwd=1.5) 
# legend("topleft", legend=c("Observed cases", "Predicted Cases"),
#        col=c("deeppink1", "darkorchid"),  cex=0.9,
#        horiz=F,  title="", bty = "n", text.font = 4, text.width = 3.5, lwd=2, lty=c(1,1) )
# abline(v=2017, lty=20, xpd=F, lwd=1)
# legend("topright", inset = c(-.40,-.2),  legend=c("Actual ", "Forecasted", "80% CI"),
#        col=c("black", "blue", "gray", "gray"),  cex=0.9,
#        horiz=F,  title="", bty = "n", text.font = 4, text.width = 3.5, lwd=2 )

#////////////////////////////////////////////////////////////////////////#
#####Holt-Winter's seasonla method
#////////////////////////////////////////////////////////////////////////#

model_hw_kheda_pv<-HoltWinters(kheda_2019_ts$kheda_pv_ts)
model_hw_anand_pv<-HoltWinters(anand_2019_ts$anand_pv_ts)
# model_hw_dahod_pv<-HoltWinters(dahod_2019_ts$dahod_pv_ts)

model_hw_kheda_pf<-HoltWinters(kheda_2019_ts$kheda_pf_ts)
model_hw_anand_pf<-HoltWinters(anand_2019_ts$anand_pf_ts)
# model_hw_dahod_pf<-HoltWinters(dahod_2019_ts$dahod_pf_ts)

model_hw_kheda_tc<-HoltWinters(kheda_2019_ts$kheda_tc_ts)
model_hw_anand_tc<-HoltWinters(anand_2019_ts$anand_tc_ts)
# model_hw_dahod_tc<-HoltWinters(dahod_2019_ts$dahod_tc_ts)


forecast_hw_kheda_pv<-forecast:::forecast.HoltWinters(model_hw_kheda_pv, h=12)
forecast_hw_anand_pv<-forecast:::forecast.HoltWinters(model_hw_anand_pv, h=12)
# forecast_hw_dahod_pv<-forecast:::forecast.HoltWinters(model_hw_dahod_pv, h=12)

forecast_hw_kheda_pf<-forecast:::forecast.HoltWinters(model_hw_kheda_pf, h=12)
forecast_hw_anand_pf<-forecast:::forecast.HoltWinters(model_hw_anand_pf, h=12)
# forecast_hw_dahod_pf<-forecast:::forecast.HoltWinters(model_hw_dahod_pf, h=12)

forecast_hw_kheda_tc<-forecast:::forecast.HoltWinters(model_hw_kheda_tc, h=12)
forecast_hw_anand_tc<-forecast:::forecast.HoltWinters(model_hw_anand_tc, h=12)
# forecast_hw_dahod_tc<-forecast:::forecast.HoltWinters(model_hw_dahod_tc, h=12)

HW_kheda_pv<-data.frame(round(forecast_hw_kheda_pv$mean), round(forecast_hw_kheda_pv$lower[,1]) , round(forecast_hw_kheda_pv$upper[,1]))
HW_kheda_pf<-data.frame(round(forecast_hw_kheda_pf$mean), round(forecast_hw_kheda_pf$lower[,1]) , round(forecast_hw_kheda_pf$upper[,1]))
HW_kheda_tc<-data.frame(round(forecast_hw_kheda_tc$mean), round(forecast_hw_kheda_tc$lower[,1]) , round(forecast_hw_kheda_tc$upper[,1]))

HW_anand_pv<-data.frame(round(forecast_hw_anand_pv$mean), round(forecast_hw_anand_pv$lower[,1]) , round(forecast_hw_anand_pv$upper[,1]))
HW_anand_pf<-data.frame(round(forecast_hw_anand_pf$mean), round(forecast_hw_anand_pf$lower[,1]) , round(forecast_hw_anand_pf$upper[,1]))
HW_anand_tc<-data.frame(round(forecast_hw_anand_tc$mean), round(forecast_hw_anand_tc$lower[,1]) , round(forecast_hw_anand_tc$upper[,1]))

# HW_dahod_pv<-data.frame(round(forecast_hw_dahod_pv$mean), round(forecast_hw_dahod_pv$lower[,1]) , round(forecast_hw_dahod_pv$upper[,1]))
# HW_dahod_pf<-data.frame(round(forecast_hw_dahod_pf$mean), round(forecast_hw_dahod_pf$lower[,1]) , round(forecast_hw_dahod_pf$upper[,1]))
# HW_dahod_tc<-data.frame(round(forecast_hw_dahod_tc$mean), round(forecast_hw_dahod_tc$lower[,1]) , round(forecast_hw_dahod_tc$upper[,1]))


# View(HW_kheda_pv)
# View(HW_kheda_pf)
# View(HW_kheda_tc)
# 
# View(HW_anand_pv)
# View(HW_anand_pf)
# View(HW_anand_tc)
# 
# View(HW_dahod_pv)
# View(HW_dahod_pf)
# View(HW_dahod_tc)



# View(accuracy(forecast_hw_kheda_pv))
# View(accuracy(forecast_hw_kheda_pf))
# View(accuracy(forecast_hw_kheda_tc))
# 
# View(accuracy(forecast_hw_anand_pv))
# View(accuracy(forecast_hw_anand_pf))
# View(accuracy(forecast_hw_anand_tc))
# 
# View(accuracy(forecast_hw_dahod_pv))
# View(accuracy(forecast_hw_dahod_pf))
# View(accuracy(forecast_hw_dahod_tc))


#////////////////////////////////////////////////////////////////////////#
#####ARIMA (AIC,BIC, log-AIC, log-BIC)
#////////////////////////////////////////////////////////////////////////#

##########KHEDA

##########pv
model_ARIMA_kheda_aic_pv<-auto.arima(kheda_2019_ts$kheda_pv_ts, trace = TRUE, test="kpss", ic="aic")
model_ARIMA_kheda_bic_pv<-auto.arima(kheda_2019_ts$kheda_pv_ts, trace = TRUE, test="kpss", ic="bic")
model_ARIMA_kheda_log_aic_pv<-auto.arima(log(kheda_2019_ts$kheda_pv_ts+1), trace = TRUE, test="kpss", ic="aic")
model_ARIMA_kheda_log_bic_pv<-auto.arima(log(kheda_2019_ts$kheda_pv_ts+1), trace = TRUE, test="kpss", ic="bic")

# plot.ts(model_ARIMA_kheda_aic_pv$residuals)
# Box.test(model_ARIMA_kheda_aic_pv$residuals, lag=20, type="Ljung-Box")
# acf(model_ARIMA_kheda_aic_pv$residuals, lag=20, main="ACF Plot")
# jarque.bera.test(model_ARIMA_kheda_aic_pv$residuals)


##########pF
model_ARIMA_kheda_aic_pf<-auto.arima(kheda_2019_ts$kheda_pf_ts, trace = TRUE, test="kpss", ic="aic")
model_ARIMA_kheda_bic_pf<-auto.arima(kheda_2019_ts$kheda_pf_ts, trace = TRUE, test="kpss", ic="bic")
model_ARIMA_kheda_log_aic_pf<-auto.arima(log(kheda_2019_ts$kheda_pf_ts+1), trace = TRUE, test="kpss", ic="aic")
model_ARIMA_kheda_log_bic_pf<-auto.arima(log(kheda_2019_ts$kheda_pf_ts+1), trace = TRUE, test="kpss", ic="bic")

# plot.ts(model_ARIMA_kheda_aic_pf$residuals)
# Box.test(model_ARIMA_kheda_aic_pf$residuals, lag=20, type="Ljung-Box")
# acf(model_ARIMA_kheda_aic_pf$residuals, lag=20, main="ACF Plot")
# jarque.bera.test(model_ARIMA_kheda_aic_pf$residuals)


##########TOTAL
model_ARIMA_kheda_aic_tc<-auto.arima(kheda_2019_ts$kheda_tc_ts, trace = TRUE, test="kpss", ic="aic")
model_ARIMA_kheda_bic_tc<-auto.arima(kheda_2019_ts$kheda_tc_ts, trace = TRUE, test="kpss", ic="bic")
model_ARIMA_kheda_log_aic_tc<-auto.arima(log(kheda_2019_ts$kheda_tc_ts+1), trace = TRUE, test="kpss", ic="aic")
model_ARIMA_kheda_log_bic_tc<-auto.arima(log(kheda_2019_ts$kheda_tc_ts+1), trace = TRUE, test="kpss", ic="bic")

# plot.ts(model_ARIMA_kheda_aic_tc$residuals)
# Box.test(model_ARIMA_kheda_aic_tc$residuals, lag=20, type="Ljung-Box")
# acf(model_ARIMA_kheda_aic_tc$residuals, lag=20, main="ACF Plot")
# jarque.bera.test(model_ARIMA_kheda_aic_tc$residuals)


##########ANAND

##########pv
model_ARIMA_anand_aic_pv<-auto.arima(anand_2019_ts$anand_pv_ts, trace = TRUE, test="kpss", ic="aic")
model_ARIMA_anand_bic_pv<-auto.arima(anand_2019_ts$anand_pv_ts, trace = TRUE, test="kpss", ic="bic")
model_ARIMA_anand_log_aic_pv<-auto.arima(log(anand_2019_ts$anand_pv_ts+1), trace = TRUE, test="kpss", ic="aic")
model_ARIMA_anand_log_bic_pv<-auto.arima(log(anand_2019_ts$anand_pv_ts+1), trace = TRUE, test="kpss", ic="bic")

# plot.ts(model_ARIMA_anand_aic_pv$residuals)
# Box.test(model_ARIMA_anand_aic_pv$residuals, lag=20, type="Ljung-Box")
# acf(model_ARIMA_anand_aic_pv$residuals, lag=20, main="ACF Plot")
# jarque.bera.test(model_ARIMA_anand_aic_pv$residuals)

##########pf
model_ARIMA_anand_aic_pf<-auto.arima(anand_2019_ts$anand_pf_ts, trace = TRUE, test="kpss", ic="aic")
model_ARIMA_anand_bic_pf<-auto.arima(anand_2019_ts$anand_pf_ts, trace = TRUE, test="kpss", ic="bic")
model_ARIMA_anand_log_aic_pf<-auto.arima(log(anand_2019_ts$anand_pf_ts+1), trace = TRUE, test="kpss", ic="aic")
model_ARIMA_anand_log_bic_pf<-auto.arima(log(anand_2019_ts$anand_pf_ts+1), trace = TRUE, test="kpss", ic="bic")

# plot.ts(model_ARIMA_anand_aic_pf$residuals)
# Box.test(model_ARIMA_anand_aic_pf$residuals, lag=20, type="Ljung-Box")
# acf(model_ARIMA_anand_aic_pf$residuals, lag=20, main="ACF Plot")
# jarque.bera.test(model_ARIMA_anand_aic_pf$residuals)

##########Total
model_ARIMA_anand_aic_tc<-auto.arima(anand_2019_ts$anand_tc_ts, trace = TRUE, test="kpss", ic="aic")
model_ARIMA_anand_bic_tc<-auto.arima(anand_2019_ts$anand_tc_ts, trace = TRUE, test="kpss", ic="bic")
model_ARIMA_anand_log_aic_tc<-auto.arima(log(anand_2019_ts$anand_tc_ts+1), trace = TRUE, test="kpss", ic="aic")
model_ARIMA_anand_log_bic_tc<-auto.arima(log(anand_2019_ts$anand_tc_ts+1), trace = TRUE, test="kpss", ic="bic")

# plot.ts(model_ARIMA_anand_aic_tc$residuals)
# Box.test(model_ARIMA_anand_aic_tc$residuals, lag=20, type="Ljung-Box")
# acf(model_ARIMA_anand_aic_tc$residuals, lag=20, main="ACF Plot")
# jarque.bera.test(model_ARIMA_anand_aic_tc$residuals)


##########DAHOD

##########pv
# model_ARIMA_dahod_aic_pv<-auto.arima(dahod_2019_ts$dahod_pv_ts, trace = TRUE, test="kpss", ic="aic")
# model_ARIMA_dahod_bic_pv<-auto.arima(dahod_2019_ts$dahod_pv_ts, trace = TRUE, test="kpss", ic="bic")
# model_ARIMA_dahod_log_aic_pv<-auto.arima(log(dahod_2019_ts$dahod_pv_ts+1), trace = TRUE, test="kpss", ic="aic")
# model_ARIMA_dahod_log_bic_pv<-auto.arima(log(dahod_2019_ts$dahod_pv_ts+1), trace = TRUE, test="kpss", ic="bic")
# 
# # plot.ts(model_ARIMA_dahod_aic_pv$residuals)
# # Box.test(model_ARIMA_dahod_aic_pv$residuals, lag=20, type="Ljung-Box")
# # acf(model_ARIMA_dahod_aic_pv$residuals, lag=20, main="ACF Plot")
# # jarque.bera.test(model_ARIMA_dahod_aic_pv$residuals)
# 
# 
# ##########pf
# model_ARIMA_dahod_aic_pf<-auto.arima(dahod_2019_ts$dahod_pf_ts, trace = TRUE, test="kpss", ic="aic")
# model_ARIMA_dahod_bic_pf<-auto.arima(dahod_2019_ts$dahod_pf_ts, trace = TRUE, test="kpss", ic="bic")
# model_ARIMA_dahod_log_aic_pf<-auto.arima(log(dahod_2019_ts$dahod_pf_ts+1), trace = TRUE, test="kpss", ic="aic")
# model_ARIMA_dahod_log_bic_pf<-auto.arima(log(dahod_2019_ts$dahod_pf_ts+1), trace = TRUE, test="kpss", ic="bic")
# 
# # plot.ts(model_ARIMA_dahod_aic_pf$residuals)
# # Box.test(model_ARIMA_dahod_aic_pf$residuals, lag=20, type="Ljung-Box")
# # acf(model_ARIMA_dahod_aic_pf$residuals, lag=20, main="ACF Plot")
# # jarque.bera.test(model_ARIMA_dahod_aic_pf$residuals)
# 
# ##########Total
# model_ARIMA_dahod_aic_tc<-auto.arima(dahod_2019_ts$dahod_tc_ts, trace = TRUE, test="kpss", ic="aic")
# model_ARIMA_dahod_bic_tc<-auto.arima(dahod_2019_ts$dahod_tc_ts, trace = TRUE, test="kpss", ic="bic")
# model_ARIMA_dahod_log_aic_tc<-auto.arima(log(dahod_2019_ts$dahod_tc_ts+1), trace = TRUE, test="kpss", ic="aic")
# model_ARIMA_dahod_log_bic_tc<-auto.arima(log(dahod_2019_ts$dahod_tc_ts+1), trace = TRUE, test="kpss", ic="bic")
# 
# plot.ts(model_ARIMA_dahod_aic_tc$residuals)
# Box.test(model_ARIMA_dahod_aic_tc$residuals, lag=20, type="Ljung-Box")
# acf(model_ARIMA_dahod_aic_tc$residuals, lag=20, main="ACF Plot")
# jarque.bera.test(model_ARIMA_dahod_aic_tc$residuals)


##########FORECASTING FOR KHEDA
fct_ARIMA_kheda_aic_pv<-forecast(model_ARIMA_kheda_aic_pv, h=12)
fct_ARIMA_kheda_bic_pv<-forecast(model_ARIMA_kheda_bic_pv, h=12)
fct_ARIMA_kheda_log_aic_pv<-forecast(model_ARIMA_kheda_log_aic_pv, h=12)
fct_ARIMA_kheda_log_bic_pv<-forecast(model_ARIMA_kheda_log_bic_pv, h=12)

fct_ARIMA_kheda_aic_pf<-forecast(model_ARIMA_kheda_aic_pf, h=12)
fct_ARIMA_kheda_bic_pf<-forecast(model_ARIMA_kheda_bic_pf, h=12)
fct_ARIMA_kheda_log_aic_pf<-forecast(model_ARIMA_kheda_log_aic_pf, h=12)
fct_ARIMA_kheda_log_bic_pf<-forecast(model_ARIMA_kheda_log_bic_pf, h=12)

fct_ARIMA_kheda_aic_tc<-forecast(model_ARIMA_kheda_aic_tc, h=12)
fct_ARIMA_kheda_bic_tc<-forecast(model_ARIMA_kheda_bic_tc, h=12)
fct_ARIMA_kheda_log_aic_tc<-forecast(model_ARIMA_kheda_log_aic_tc, h=12)
fct_ARIMA_kheda_log_bic_tc<-forecast(model_ARIMA_kheda_log_bic_tc, h=12)


##########FORECASTING FOR ANAND
fct_ARIMA_anand_aic_pv<-forecast(model_ARIMA_anand_aic_pv, h=12)
fct_ARIMA_anand_bic_pv<-forecast(model_ARIMA_anand_bic_pv, h=12)
fct_ARIMA_anand_log_aic_pv<-forecast(model_ARIMA_anand_log_aic_pv, h=12)
fct_ARIMA_anand_log_bic_pv<-forecast(model_ARIMA_anand_log_bic_pv, h=12)

fct_ARIMA_anand_aic_pf<-forecast(model_ARIMA_anand_aic_pf, h=12)
fct_ARIMA_anand_bic_pf<-forecast(model_ARIMA_anand_bic_pf, h=12)
fct_ARIMA_anand_log_aic_pf<-forecast(model_ARIMA_anand_log_aic_pf, h=12)
fct_ARIMA_anand_log_bic_pf<-forecast(model_ARIMA_anand_log_bic_pf, h=12)

fct_ARIMA_anand_aic_tc<-forecast(model_ARIMA_anand_aic_tc, h=12)
fct_ARIMA_anand_bic_tc<-forecast(model_ARIMA_anand_bic_tc, h=12)
fct_ARIMA_anand_log_aic_tc<-forecast(model_ARIMA_anand_log_aic_tc, h=12)
fct_ARIMA_anand_log_bic_tc<-forecast(model_ARIMA_anand_log_bic_tc, h=12)

##########FORECASTING FOR DAHOD
# fct_ARIMA_dahod_aic_pv<-forecast(model_ARIMA_dahod_aic_pv, h=12)
# fct_ARIMA_dahod_bic_pv<-forecast(model_ARIMA_dahod_bic_pv, h=12)
# fct_ARIMA_dahod_log_aic_pv<-forecast(model_ARIMA_dahod_log_aic_pv, h=12)
# fct_ARIMA_dahod_log_bic_pv<-forecast(model_ARIMA_dahod_log_bic_pv, h=12)
# 
# fct_ARIMA_dahod_aic_pf<-forecast(model_ARIMA_dahod_aic_pf, h=12)
# fct_ARIMA_dahod_bic_pf<-forecast(model_ARIMA_dahod_bic_pf, h=12)
# fct_ARIMA_dahod_log_aic_pf<-forecast(model_ARIMA_dahod_log_aic_pf, h=12)
# fct_ARIMA_dahod_log_bic_pf<-forecast(model_ARIMA_dahod_log_bic_pf, h=12)
# 
# fct_ARIMA_dahod_aic_tc<-forecast(model_ARIMA_dahod_aic_tc, h=12)
# fct_ARIMA_dahod_bic_tc<-forecast(model_ARIMA_dahod_bic_tc, h=12)
# fct_ARIMA_dahod_log_aic_tc<-forecast(model_ARIMA_dahod_log_aic_tc, h=12)
# fct_ARIMA_dahod_log_bic_tc<-forecast(model_ARIMA_dahod_log_bic_tc, h=12)
# 


########Forecasted values

***************AIC


ARIMA_AIC_kheda_pv<-data.frame(round(fct_ARIMA_kheda_aic_pv$mean), round(fct_ARIMA_kheda_aic_pv$lower[,1]) , round(fct_ARIMA_kheda_aic_pv$upper[,1]))
ARIMA_AIC_kheda_pf<-data.frame(round(fct_ARIMA_kheda_aic_pf$mean), round(fct_ARIMA_kheda_aic_pf$lower[,1]) , round(fct_ARIMA_kheda_aic_pf$upper[,1]))
ARIMA_AIC_kheda_tc<-data.frame(round(fct_ARIMA_kheda_aic_tc$mean), round(fct_ARIMA_kheda_aic_tc$lower[,1]) , round(fct_ARIMA_kheda_aic_tc$upper[,1]))

ARIMA_AIC_anand_pv<-data.frame(round(fct_ARIMA_anand_aic_pv$mean), round(fct_ARIMA_anand_aic_pv$lower[,1]) , round(fct_ARIMA_anand_aic_pv$upper[,1]))
ARIMA_AIC_anand_pf<-data.frame(round(fct_ARIMA_anand_aic_pf$mean), round(fct_ARIMA_anand_aic_pf$lower[,1]) , round(fct_ARIMA_anand_aic_pf$upper[,1]))
ARIMA_AIC_anand_tc<-data.frame(round(fct_ARIMA_anand_aic_tc$mean), round(fct_ARIMA_anand_aic_tc$lower[,1]) , round(fct_ARIMA_anand_aic_tc$upper[,1]))

# ARIMA_AIC_dahod_pv<-data.frame(round(fct_ARIMA_dahod_aic_pv$mean), round(fct_ARIMA_dahod_aic_pv$lower[,1]) , round(fct_ARIMA_dahod_aic_pv$upper[,1]))
# ARIMA_AIC_dahod_pf<-data.frame(round(fct_ARIMA_dahod_aic_pf$mean), round(fct_ARIMA_dahod_aic_pf$lower[,1]) , round(fct_ARIMA_dahod_aic_pf$upper[,1]))
# ARIMA_AIC_dahod_tc<-data.frame(round(fct_ARIMA_dahod_aic_tc$mean), round(fct_ARIMA_dahod_aic_tc$lower[,1]) , round(fct_ARIMA_dahod_aic_tc$upper[,1]))


# View(ARIMA_AIC_kheda_pv)
# View(ARIMA_AIC_kheda_pf)
# View(ARIMA_AIC_kheda_tc)
# 
# View(ARIMA_AIC_anand_pv)
# View(ARIMA_AIC_anand_pf)
# View(ARIMA_AIC_anand_tc)
# 
# View(ARIMA_AIC_dahod_pv)
# View(ARIMA_AIC_dahod_pf)
# View(ARIMA_AIC_dahod_tc)
# 



#***************BIC
ARIMA_BIC_kheda_pv<-data.frame(round(fct_ARIMA_kheda_bic_pv$mean), round(fct_ARIMA_kheda_bic_pv$lower[,1]) , round(fct_ARIMA_kheda_bic_pv$upper[,1]))
ARIMA_BIC_kheda_pf<-data.frame(round(fct_ARIMA_kheda_bic_pf$mean), round(fct_ARIMA_kheda_bic_pf$lower[,1]) , round(fct_ARIMA_kheda_bic_pf$upper[,1]))
ARIMA_BIC_kheda_tc<-data.frame(round(fct_ARIMA_kheda_bic_tc$mean), round(fct_ARIMA_kheda_bic_tc$lower[,1]) , round(fct_ARIMA_kheda_bic_tc$upper[,1]))

ARIMA_BIC_anand_pv<-data.frame(round(fct_ARIMA_anand_bic_pv$mean), round(fct_ARIMA_anand_bic_pv$lower[,1]) , round(fct_ARIMA_anand_bic_pv$upper[,1]))
ARIMA_BIC_anand_pf<-data.frame(round(fct_ARIMA_anand_bic_pf$mean), round(fct_ARIMA_anand_bic_pf$lower[,1]) , round(fct_ARIMA_anand_bic_pf$upper[,1]))
ARIMA_BIC_anand_tc<-data.frame(round(fct_ARIMA_anand_bic_tc$mean), round(fct_ARIMA_anand_bic_tc$lower[,1]) , round(fct_ARIMA_anand_bic_tc$upper[,1]))

# ARIMA_BIC_dahod_pv<-data.frame(round(fct_ARIMA_dahod_bic_pv$mean), round(fct_ARIMA_dahod_bic_pv$lower[,1]) , round(fct_ARIMA_dahod_bic_pv$upper[,1]))
# ARIMA_BIC_dahod_pf<-data.frame(round(fct_ARIMA_dahod_bic_pf$mean), round(fct_ARIMA_dahod_bic_pf$lower[,1]) , round(fct_ARIMA_dahod_bic_pf$upper[,1]))
# ARIMA_BIC_dahod_tc<-data.frame(round(fct_ARIMA_dahod_bic_tc$mean), round(fct_ARIMA_dahod_bic_tc$lower[,1]) , round(fct_ARIMA_dahod_bic_tc$upper[,1]))


# View(ARIMA_BIC_kheda_pv)
# View(ARIMA_BIC_kheda_pf)
# View(ARIMA_BIC_kheda_tc)
# 
# View(ARIMA_BIC_anand_pv)
# View(ARIMA_BIC_anand_pf)
# View(ARIMA_BIC_anand_tc)
# 
# View(ARIMA_BIC_dahod_pv)
# View(ARIMA_BIC_dahod_pf)
# View(ARIMA_BIC_dahod_tc)
# 


#***************LOG-AIC
ARIMA_LOG_AIC_kheda_pv<-data.frame(round(exp(fct_ARIMA_kheda_log_aic_pv$mean)), round(exp(fct_ARIMA_kheda_log_aic_pv$lower[,1])) , round(exp(fct_ARIMA_kheda_log_aic_pv$upper[,1])))
ARIMA_LOG_AIC_kheda_pf<-data.frame(round(exp(fct_ARIMA_kheda_log_aic_pf$mean)), round(exp(fct_ARIMA_kheda_log_aic_pf$lower[,1])) , round(exp(fct_ARIMA_kheda_log_aic_pf$upper[,1])))
ARIMA_LOG_AIC_kheda_tc<-data.frame(round(exp(fct_ARIMA_kheda_log_aic_tc$mean)), round(exp(fct_ARIMA_kheda_log_aic_pf$lower[,1])) , round(exp(fct_ARIMA_kheda_log_aic_tc$upper[,1])))

ARIMA_LOG_AIC_anand_pv<-data.frame(round(exp(fct_ARIMA_anand_log_aic_pv$mean)), round(exp(fct_ARIMA_anand_log_aic_pv$lower[,1])) , round(exp(fct_ARIMA_anand_log_aic_pv$upper[,1])))
ARIMA_LOG_AIC_anand_pf<-data.frame(round(exp(fct_ARIMA_anand_log_aic_pf$mean)), round(exp(fct_ARIMA_anand_log_aic_pf$lower[,1])) , round(exp(fct_ARIMA_anand_log_aic_pf$upper[,1])))
ARIMA_LOG_AIC_anand_tc<-data.frame(round(exp(fct_ARIMA_anand_log_aic_tc$mean)), round(exp(fct_ARIMA_anand_log_aic_pf$lower[,1])) , round(exp(fct_ARIMA_anand_log_aic_tc$upper[,1])))

# ARIMA_LOG_AIC_dahod_pv<-data.frame(round(exp(fct_ARIMA_dahod_log_aic_pv$mean)), round(exp(fct_ARIMA_dahod_log_aic_pv$lower[,1])) , round(exp(fct_ARIMA_dahod_log_aic_pv$upper[,1])))
# ARIMA_LOG_AIC_dahod_pf<-data.frame(round(exp(fct_ARIMA_dahod_log_aic_pf$mean)), round(exp(fct_ARIMA_dahod_log_aic_pf$lower[,1])) , round(exp(fct_ARIMA_dahod_log_aic_pf$upper[,1])))
# ARIMA_LOG_AIC_dahod_tc<-data.frame(round(exp(fct_ARIMA_dahod_log_aic_tc$mean)), round(exp(fct_ARIMA_dahod_log_aic_pf$lower[,1])) , round(exp(fct_ARIMA_dahod_log_aic_tc$upper[,1])))



# View(ARIMA_LOG_AIC_kheda_pv)
# View(ARIMA_LOG_AIC_kheda_pf)
# View(ARIMA_LOG_AIC_kheda_tc)
# 
# 
# View(ARIMA_LOG_AIC_anand_pv)
# View(ARIMA_LOG_AIC_anand_pf)
# View(ARIMA_LOG_AIC_anand_tc)
# 
# View(ARIMA_LOG_AIC_dahod_pv)
# View(ARIMA_LOG_AIC_dahod_pf)
# View(ARIMA_LOG_AIC_dahod_tc)




#***************LOG-BIC
ARIMA_LOG_BIC_kheda_pv<-data.frame(round(exp(fct_ARIMA_kheda_log_bic_pv$mean)), round(exp(fct_ARIMA_kheda_log_bic_pv$lower[,1])) , round(exp(fct_ARIMA_kheda_log_bic_pv$upper[,1])))
ARIMA_LOG_BIC_kheda_pf<-data.frame(round(exp(fct_ARIMA_kheda_log_bic_pf$mean)), round(exp(fct_ARIMA_kheda_log_bic_pf$lower[,1])) , round(exp(fct_ARIMA_kheda_log_bic_pf$upper[,1])))
ARIMA_LOG_BIC_kheda_tc<-data.frame(round(exp(fct_ARIMA_kheda_log_bic_tc$mean)), round(exp(fct_ARIMA_kheda_log_bic_pf$lower[,1])) , round(exp(fct_ARIMA_kheda_log_bic_tc$upper[,1])))

ARIMA_LOG_BIC_anand_pv<-data.frame(round(exp(fct_ARIMA_anand_log_bic_pv$mean)), round(exp(fct_ARIMA_anand_log_bic_pv$lower[,1])) , round(exp(fct_ARIMA_anand_log_bic_pv$upper[,1])))
ARIMA_LOG_BIC_anand_pf<-data.frame(round(exp(fct_ARIMA_anand_log_bic_pf$mean)), round(exp(fct_ARIMA_anand_log_bic_pf$lower[,1])) , round(exp(fct_ARIMA_anand_log_bic_pf$upper[,1])))
ARIMA_LOG_BIC_anand_tc<-data.frame(round(exp(fct_ARIMA_anand_log_bic_tc$mean)), round(exp(fct_ARIMA_anand_log_bic_pf$lower[,1])) , round(exp(fct_ARIMA_anand_log_bic_tc$upper[,1])))

# ARIMA_LOG_BIC_dahod_pv<-data.frame(round(exp(fct_ARIMA_dahod_log_bic_pv$mean)), round(exp(fct_ARIMA_dahod_log_bic_pv$lower[,1])) , round(exp(fct_ARIMA_dahod_log_bic_pv$upper[,1])))
# ARIMA_LOG_BIC_dahod_pf<-data.frame(round(exp(fct_ARIMA_dahod_log_bic_pf$mean)), round(exp(fct_ARIMA_dahod_log_bic_pf$lower[,1])) , round(exp(fct_ARIMA_dahod_log_bic_pf$upper[,1])))
# ARIMA_LOG_BIC_dahod_tc<-data.frame(round(exp(fct_ARIMA_dahod_log_bic_tc$mean)), round(exp(fct_ARIMA_dahod_log_bic_pf$lower[,1])) , round(exp(fct_ARIMA_dahod_log_bic_tc$upper[,1])))

# View(ARIMA_LOG_BIC_kheda_pv)
# View(ARIMA_LOG_BIC_kheda_pf)
# View(ARIMA_LOG_BIC_kheda_tc)
# 
# View(ARIMA_LOG_BIC_anand_pv)
# View(ARIMA_LOG_BIC_anand_pf)
# View(ARIMA_LOG_BIC_anand_tc)
# 
# View(ARIMA_LOG_BIC_dahod_pv)
# View(ARIMA_LOG_BIC_dahod_pf)
# View(ARIMA_LOG_BIC_dahod_tc)



########################Error

##### AIC
# View(accuracy(fct_ARIMA_kheda_aic_pv))
# View(accuracy(fct_ARIMA_kheda_aic_pf))
# View(accuracy(fct_ARIMA_kheda_aic_tc))
# 
# View(accuracy(fct_ARIMA_anand_aic_pv))
# View(accuracy(fct_ARIMA_anand_aic_pf))
# View(accuracy(fct_ARIMA_anand_aic_tc))
# 
# View(accuracy(fct_ARIMA_dahod_aic_pv))
# View(accuracy(fct_ARIMA_dahod_aic_pf))
# View(accuracy(fct_ARIMA_dahod_aic_tc))
# 
# ##### BIC
# 
# View(accuracy(fct_ARIMA_kheda_bic_pv))
# View(accuracy(fct_ARIMA_kheda_bic_pf))
# View(accuracy(fct_ARIMA_kheda_bic_tc))
# 
# View(accuracy(fct_ARIMA_anand_bic_pv))
# View(accuracy(fct_ARIMA_anand_bic_pf))
# View(accuracy(fct_ARIMA_anand_bic_tc))
# 
# View(accuracy(fct_ARIMA_dahod_bic_pv))
# View(accuracy(fct_ARIMA_dahod_bic_pf))
# View(accuracy(fct_ARIMA_dahod_bic_tc))
# 
# 
# ##### Log AIC
# 
# # kheda_2016_ts$kheda_pv_ts
# # round(exp(fct_ARIMA_kheda_log_aic_pv$fitted))
# 
# View(Error_log_AIC_kheda_pv<-data.frame(kheda_2017_ts$kheda_pv_ts, round(exp(fct_ARIMA_kheda_log_aic_pv$fitted))))
# View(Error_log_AIC_kheda_pf<-data.frame(kheda_2017_ts$kheda_pf_ts, round(exp(fct_ARIMA_kheda_log_aic_pf$fitted))))
# View(Error_log_AIC_kheda_tc<-data.frame(kheda_2017_ts$kheda_tc_ts, round(exp(fct_ARIMA_kheda_log_aic_tc$fitted))))
# 
# 
# View(Error_log_AIC_anand_pv<-data.frame(anand_2017_ts$anand_pv_ts, round(exp(fct_ARIMA_anand_log_aic_pv$fitted))))
# View(Error_log_AIC_anand_pf<-data.frame(anand_2017_ts$anand_pf_ts, round(exp(fct_ARIMA_anand_log_aic_pf$fitted))))
# View(Error_log_AIC_anand_tc<-data.frame(anand_2017_ts$anand_tc_ts, round(exp(fct_ARIMA_anand_log_aic_tc$fitted))))
# 
# 
# View(Error_log_AIC_dahod_pv<-data.frame(dahod_2017_ts$dahod_pv_ts, round(exp(fct_ARIMA_dahod_log_aic_pv$fitted))))
# View(Error_log_AIC_dahod_pf<-data.frame(dahod_2017_ts$dahod_pf_ts, round(exp(fct_ARIMA_dahod_log_aic_pf$fitted))))
# View(Error_log_AIC_dahod_tc<-data.frame(dahod_2017_ts$dahod_tc_ts, round(exp(fct_ARIMA_dahod_log_aic_tc$fitted))))
# 
# 
# 
# ##### Log bic
# 
# View(Error_log_BIC_kheda_pv<-data.frame(kheda_2017_ts$kheda_pv_ts, round(exp(fct_ARIMA_kheda_log_bic_pv$fitted))))
# View(Error_log_BIC_kheda_pf<-data.frame(kheda_2017_ts$kheda_pf_ts, round(exp(fct_ARIMA_kheda_log_bic_pf$fitted))))
# View(Error_log_BIC_kheda_tc<-data.frame(kheda_2017_ts$kheda_tc_ts, round(exp(fct_ARIMA_kheda_log_bic_tc$fitted))))
# 
# 
# View(Error_log_BIC_anand_pv<-data.frame(anand_2017_ts$anand_pv_ts, round(exp(fct_ARIMA_anand_log_bic_pv$fitted))))
# View(Error_log_BIC_anand_pf<-data.frame(anand_2017_ts$anand_pf_ts, round(exp(fct_ARIMA_anand_log_bic_pf$fitted))))
# View(Error_log_BIC_anand_tc<-data.frame(anand_2017_ts$anand_tc_ts, round(exp(fct_ARIMA_anand_log_bic_tc$fitted))))
# 
# 
# View(Error_log_BIC_dahod_pv<-data.frame(dahod_2017_ts$dahod_pv_ts, round(exp(fct_ARIMA_dahod_log_bic_pv$fitted))))
# View(Error_log_BIC_dahod_pf<-data.frame(dahod_2017_ts$dahod_pf_ts, round(exp(fct_ARIMA_dahod_log_bic_pf$fitted))))
# View(Error_log_BIC_dahod_tc<-data.frame(dahod_2017_ts$dahod_tc_ts, round(exp(fct_ARIMA_dahod_log_bic_tc$fitted))))
# 



###########
#################################################################################
##############ARIMA Regression
#################################################################################

#fcast_S_T_kheda = forecast(fit.model_S_T_kheda, h=12, newdata=kheda_2017_ts) 
#fcast_S_T_anand = forecast(fit.model_S_T_anand, h=12, newdata=anand_2017_ts) 
#fcast_S_T_dahod = forecast(fit.model_S_T_dahod, h=12, newdata=dahod_2017_ts) 



xreg_kheda<-cbind(kheda_2019_ts[, 4], kheda_2019_ts[, 6], kheda_2019_ts[, 9])
xreg_anand<-cbind(anand_2019_ts[, 4], anand_2019_ts[, 6], anand_2019_ts[, 9])
xreg_dahod<-cbind(dahod_2019_ts[, 4], dahod_2019_ts[, 6], dahod_2019_ts[, 9])


xreg_kheda_2020<-cbind(kheda_2020_ts[, 4], kheda_2020_ts[, 6], kheda_2020_ts[, 9])
xreg_anand_2020<-cbind(anand_2020_ts[, 4], anand_2020_ts[, 6], anand_2020_ts[, 9])
# xreg_dahod_2020<-cbind(dahod_2020_ts[, 4], dahod_2020_ts[, 6], dahod_2020_ts[, 9])


model_ARIMA_Reg_kheda_pv<-auto.arima(kheda_2019_ts[,"kheda_pv_ts"], xreg=xreg_kheda)
model_ARIMA_Reg_anand_pv<-auto.arima(anand_2019_ts[,"anand_pv_ts"], xreg=xreg_anand)
# model_ARIMA_Reg_dahod_pv<-auto.arima(dahod_2019_ts[,"dahod_pv_ts"], xreg=xreg_dahod)



model_ARIMA_Reg_kheda_pf<-auto.arima(kheda_2019_ts[,"kheda_pf_ts"], xreg=xreg_kheda)
model_ARIMA_Reg_anand_pf<-auto.arima(anand_2019_ts[,"anand_pf_ts"], xreg=xreg_anand)
# model_ARIMA_Reg_dahod_pf<-auto.arima(dahod_2019_ts[,"dahod_pf_ts"], xreg=xreg_dahod)


model_ARIMA_Reg_kheda_tc<-auto.arima(kheda_2019_ts[,"kheda_tc_ts"], xreg=xreg_kheda)
model_ARIMA_Reg_anand_tc<-auto.arima(anand_2019_ts[,"anand_tc_ts"], xreg=xreg_anand)
# model_ARIMA_Reg_dahod_tc<-auto.arima(dahod_2019_ts[,"dahod_tc_ts"], xreg=xreg_dahod)


fcast_reg_kheda_pv = forecast(model_ARIMA_Reg_kheda_pv, h=12, xreg=xreg_kheda_2020) 
fcast_reg_kheda_pf = forecast(model_ARIMA_Reg_kheda_pf, h=12, xreg=xreg_kheda_2020) 
fcast_reg_kheda_tc = forecast(model_ARIMA_Reg_kheda_tc, h=12, xreg=xreg_kheda_2020) 

fcast_reg_anand_pv = forecast(model_ARIMA_Reg_anand_pv, h=12, xreg=xreg_anand_2020) 
fcast_reg_anand_pf = forecast(model_ARIMA_Reg_anand_pf, h=12, xreg=xreg_anand_2020) 
fcast_reg_anand_tc = forecast(model_ARIMA_Reg_anand_tc, h=12, xreg=xreg_anand_2020) 

# 
# fcast_reg_dahod_pv = forecast(model_ARIMA_Reg_dahod_pv, h=12, xreg=xreg_dahod_2018) 
# fcast_reg_dahod_pf = forecast(model_ARIMA_Reg_dahod_pf, h=12, xreg=xreg_dahod_2018) 
# fcast_reg_dahod_tc = forecast(model_ARIMA_Reg_dahod_tc, h=12, xreg=xreg_dahod_2018) 


ARIMA_REG_kheda_pv<-data.frame(round(fcast_reg_kheda_pv$mean), round(fcast_reg_kheda_pv$lower[,1]) , round(fcast_reg_kheda_pv$upper[,1]))
ARIMA_REG_kheda_pf<-data.frame(round(fcast_reg_kheda_pf$mean), round(fcast_reg_kheda_pf$lower[,1]) , round(fcast_reg_kheda_pf$upper[,1]))
ARIMA_REG_kheda_tc<-data.frame(round(fcast_reg_kheda_tc$mean), round(fcast_reg_kheda_tc$lower[,1]) , round(fcast_reg_kheda_tc$upper[,1]))

ARIMA_REG_anand_pv<-data.frame(round(fcast_reg_anand_pv$mean), round(fcast_reg_anand_pv$lower[,1]) , round(fcast_reg_anand_pv$upper[,1]))
ARIMA_REG_anand_pf<-data.frame(round(fcast_reg_anand_pf$mean), round(fcast_reg_anand_pf$lower[,1]) , round(fcast_reg_anand_pf$upper[,1]))
ARIMA_REG_anand_tc<-data.frame(round(fcast_reg_anand_tc$mean), round(fcast_reg_anand_tc$lower[,1]) , round(fcast_reg_anand_tc$upper[,1]))

# ARIMA_REG_dahod_pv<-data.frame(round(fcast_reg_dahod_pv$mean), round(fcast_reg_dahod_pv$lower[,1]) , round(fcast_reg_dahod_pv$upper[,1]))
# ARIMA_REG_dahod_pf<-data.frame(round(fcast_reg_dahod_pf$mean), round(fcast_reg_dahod_pf$lower[,1]) , round(fcast_reg_dahod_pf$upper[,1]))
# ARIMA_REG_dahod_tc<-data.frame(round(fcast_reg_dahod_tc$mean), round(fcast_reg_dahod_tc$lower[,1]) , round(fcast_reg_dahod_tc$upper[,1]))



View(accuracy(model_ARIMA_Reg_kheda_pv))
View(accuracy(model_ARIMA_Reg_kheda_pf))
View(accuracy(model_ARIMA_Reg_kheda_tc))

View(accuracy(model_ARIMA_Reg_anand_pv))
View(accuracy(model_ARIMA_Reg_anand_pf))
View(accuracy(model_ARIMA_Reg_anand_tc))

View(accuracy(model_ARIMA_Reg_dahod_pv))
View(accuracy(model_ARIMA_Reg_dahod_pf))
View(accuracy(model_ARIMA_Reg_dahod_tc))



#########################################################################
#########################################################################

#########Final Excel Output

#########################################################################
#########################################################################



final_kheda_pv<-data.frame(kheda_2020_ts$kheda_pv_ts,TC_Reg_kheda_pv, TC_Reg_trend_kheda_pv, HW_kheda_pv, ARIMA_AIC_kheda_pv, ARIMA_BIC_kheda_pv, ARIMA_LOG_AIC_kheda_pv, ARIMA_LOG_BIC_kheda_pv, ARIMA_REG_kheda_pv)
final_kheda_pf<-data.frame(kheda_2020_ts$kheda_pf_ts,TC_Reg_kheda_pf, TC_Reg_trend_kheda_pf, HW_kheda_pf, ARIMA_AIC_kheda_pf, ARIMA_BIC_kheda_pf, ARIMA_LOG_AIC_kheda_pf, ARIMA_LOG_BIC_kheda_pf, ARIMA_REG_kheda_pf)
final_kheda_tc<-data.frame(kheda_2020_ts$kheda_tc_ts,TC_Reg_kheda_tc, TC_Reg_trend_kheda_tc, HW_kheda_tc, ARIMA_AIC_kheda_tc, ARIMA_BIC_kheda_tc, ARIMA_LOG_AIC_kheda_tc, ARIMA_LOG_BIC_kheda_tc, ARIMA_REG_kheda_tc)

final_anand_pv<-data.frame(anand_2020_ts$anand_pv_ts,TC_Reg_anand_pv, TC_Reg_trend_anand_pv, HW_anand_pv, ARIMA_AIC_anand_pv, ARIMA_BIC_anand_pv, ARIMA_LOG_AIC_anand_pv, ARIMA_LOG_BIC_anand_pv, ARIMA_REG_anand_pv)
final_anand_pf<-data.frame(anand_2020_ts$anand_pf_ts,TC_Reg_anand_pf, TC_Reg_trend_anand_pf, HW_anand_pf, ARIMA_AIC_anand_pf, ARIMA_BIC_anand_pf, ARIMA_LOG_AIC_anand_pf, ARIMA_LOG_BIC_anand_pf, ARIMA_REG_anand_pf)
final_anand_tc<-data.frame(anand_2020_ts$anand_tc_ts,TC_Reg_anand_tc, TC_Reg_trend_anand_tc, HW_anand_tc, ARIMA_AIC_anand_tc, ARIMA_BIC_anand_tc, ARIMA_LOG_AIC_anand_tc, ARIMA_LOG_BIC_anand_tc, ARIMA_REG_anand_tc)

# final_dahod_pv<-data.frame(dahod_2020_ts$dahod_pv_ts,TC_Reg_dahod_pv, TC_Reg_trend_dahod_pv, HW_dahod_pv, ARIMA_AIC_dahod_pv, ARIMA_BIC_dahod_pv, ARIMA_LOG_AIC_dahod_pv, ARIMA_LOG_BIC_dahod_pv, ARIMA_REG_dahod_pv)
# final_dahod_pf<-data.frame(dahod_2020_ts$dahod_pf_ts,TC_Reg_dahod_pf, TC_Reg_trend_dahod_pf, HW_dahod_pf, ARIMA_AIC_dahod_pf, ARIMA_BIC_dahod_pf, ARIMA_LOG_AIC_dahod_pf, ARIMA_LOG_BIC_dahod_pf, ARIMA_REG_dahod_pf)
# final_dahod_tc<-data.frame(dahod_2020_ts$dahod_tc_ts,TC_Reg_dahod_tc, TC_Reg_trend_dahod_tc, HW_dahod_tc, ARIMA_AIC_dahod_tc, ARIMA_BIC_dahod_tc, ARIMA_LOG_AIC_dahod_tc, ARIMA_LOG_BIC_dahod_tc, ARIMA_REG_dahod_tc)



View(final_kheda_pv)
View(final_kheda_pf)
View(final_kheda_tc)


View(final_anand_pv)
View(final_anand_pf)
View(final_anand_tc)

View(final_dahod_pv)
View(final_dahod_pf)
View(final_dahod_tc)


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   Graphs #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
kheda<-subset(data, District=="KHEDA") 
dahod<-subset(data, District=="DAHOD" )
anand<-subset(data, District=="ANAND" )

names(kheda)

kheda_tot_ts<-ts(kheda[,"Malaria.cases"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_pf_ts<-ts(kheda[,"pf"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_pv_ts<-ts(kheda[,"pv"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_humidity_ts<-ts(kheda[,"humidity"], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_rainfall.mm._ts<-ts(kheda[,"rainfall.mm."], start=c(2008,1), end=c(2019,12), frequency=12)
kheda_min_temp_ts<-ts(kheda[,"min_temp"], start=c(2008,1), end=c(2019,12), frequency=12)

ggseasonplot(kheda_tot_ts, year.labels=F, xlab="Months", ylab="Malaria Cases",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme(  axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(kheda_pv_ts, year.labels=F, xlab="Months", ylab="Pv Cases",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme( legend.position="NULL", axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(kheda_pf_ts, year.labels=F  , xlab="Months", ylab="Pf Cases",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme( legend.position="NULL", axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(kheda_rainfall.mm._ts, year.labels=F  , xlab="Months", ylab="Rainfall (MM)",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme( legend.position="NULL",axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(kheda_humidity_ts, year.labels=F  , xlab="Months", ylab="Humidity (%)",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme(legend.position="NULL", axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(kheda_min_temp_ts, year.labels=F  , xlab="Months", ylab="Min. Temp. (C)",  main="",polar = TRUE, col=rainbow(13))+theme_minimal()+geom_line( size=1)+ theme_minimal()+ theme(legend.position="NULL", axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))







anand_tot_ts<-ts(anand[,"Malaria.cases"], start=c(2008,1), end=c(2019,12), frequency=12)

anand_pf_ts<-ts(anand[,"pf"], start=c(2008,1), end=c(2019,12), frequency=12)
anand_pv_ts<-ts(anand[,"pv"], start=c(2008,1), end=c(2019,12), frequency=12)
anand_humidity_ts<-ts(anand[,"humidity"], start=c(2008,1), end=c(2019,12), frequency=12)
anand_rainfall.mm._ts<-ts(anand[,"rainfall.mm."], start=c(2008,1), end=c(2019,12), frequency=12)
anand_min_temp_ts<-ts(anand[,"min_temp"], start=c(2008,1), end=c(2019,12), frequency=12)

ggseasonplot(anand_tot_ts, year.labels=F, xlab="Months", ylab="Malaria Cases",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme(  axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(anand_pv_ts, year.labels=F, xlab="Months", ylab="Pv Cases",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme(  axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(anand_pf_ts, year.labels=F  , xlab="Months", ylab="Pf Cases",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme( legend.position="NULL", axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(anand_rainfall.mm._ts, year.labels=F  , xlab="Months", ylab="Rainfall (MM)",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme( legend.position="NULL",axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(anand_humidity_ts, year.labels=F  , xlab="Months", ylab="Humidity (%)",  main="",polar = TRUE, col=rainbow(13))+geom_line( size=1)+ theme_minimal()+ theme(legend.position="NULL", axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))
ggseasonplot(anand_min_temp_ts, year.labels=F  , xlab="Months", ylab="Min. Temp. (C)",  main="",polar = TRUE, col=rainbow(13))+theme_minimal()+geom_line( size=1)+ theme_minimal()+ theme(legend.position="NULL", axis.text= element_text(size=12, face="bold", family = 2),axis.title = element_text(face="bold", size=15, family=3),axis.title.x=element_blank(), panel.grid.major = element_line(size = .5, linetype = "dashed",colour = "black"))




















###############################GARCH MODEL
#################################################################################
library(zoo)

######for vatality
Box.test(model_ARIMA_kheda_aic$residuals^2, lag=20, type="Ljung-Box")

kheda_garch_spec<-ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(1,1,1)[12]))
kheda_garch_fit<-ugarchfit(spec=kheda_garch_spec, data=kheda_2016_ts$kheda_tc_ts)

kheda_garch_fit

Box.test(model_ARIMA_anand_aic$residuals^2, lag=20, type="Ljung-Box")
Box.test(model_ARIMA_dahod_aic$residuals^2, lag=20, type="Ljung-Box")











ctrl<-list(tol=1e-7,delta=1e-9)
kheda_garch_roll<-ugarchroll(kheda_garch_spec,kheda_2016_ts$kheda_tc_ts, n.start=12,refit.every = 1, refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE, solver.control = ctrl, fit.control = list(scale=1))

report(kheda_garch_roll, type="VaR", VaR.alpha=.01, conf.level=0.99)

kheda_garch_fcst<-ugarchforecast(kheda_garch_fit, n.ahead = 12)

kheda_garch_fcst1<-ugarchboot(kheda_garch_fit, n.ahead = 12, method=c("Partial", "Full")[1])


kheda_garch_fcst@forecast$seriesFor
kheda_garch_fcst@model$fmodel


plot(kheda_garch_fcst1,which=2)

kheda_garch_fcst$
  
  
  help(ugarchspec)

************New GARCH

m1.mean.model <- auto.arima(kheda_2016_ts$kheda_tc_ts, allowmean=F )
ar.comp <- arimaorder(m1.mean.model)[1]
ma.comp <- arimaorder(m1.mean.model)[3]

ar.comp
ma.comp  








##############Decomposition

##############Classical (Additive)
tc_kheda_ts %>% decompose(type="additive") %>%
  autoplot()

##############Classical (Multiplicative)
tc_kheda_ts_mul<-tc_kheda_ts+1
tc_kheda_ts_mul %>% decompose(type="multiplicative") %>%
  autoplot()

tc_kheda_ts_mul_dcom<-decompose(tc_kheda_ts_mul, type="multiplicative")
tc_kheda_ts_add_dcom<-decompose(tc_kheda_ts, type="additive")
tc_kheda_ts_add_dcom<-decompose(tc_kheda_ts, type="additive")
tc_kheda_ts_add_dcom<-x11(tc_kheda_ts)

fit<-tc_kheda_ts %>% seas(x11="")


par(mfrow=c(3,1))
plot(trendcycle(fit), col="red")
plot(seasadj(fit), col="blue")
plot(tc_kheda_ts_mul_dcom$trend,  col="pink")
plot(tc_kheda_ts_add_dcom$trend, col="black", lty=3)

##############Classical (X11)
library(seasonal)

fit<-tc_kheda_ts %>% seas(x11="")



########Forecasting with decomposition

fit<-stl(tc_kheda_ts, t.wondow=12, s.window="periodic", robust=T)
