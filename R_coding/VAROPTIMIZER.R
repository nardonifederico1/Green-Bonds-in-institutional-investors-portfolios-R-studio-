#VAR OPTIMIZER (errore finale)

##############################################
## LIBRERIE (giusto)
##############################################
library(lubridate)
library(dplyr)
library(tidyr)
library(nloptr)
library(stats)
library(ggplot2)
library(ggfortify)
library(readxl)
library(readr)
library(tidyquant)
library(writexl)
library(zoo)  # per LOCF
library(PortfolioAnalytics)
library(PerformanceAnalytics)  # per VaR/ES
library(DEoptim)               # se usi optimize_method = "DEoptim"
library(xts)
library(quantmod)

##############################################
## VaR 
##############################################
percentile <- 0.95

# PORTAFOGLIO GB
VaR_asset_histGB <- VaR(R = daily_returnsGB, p= percentile, method="historical",
                      portfolio_method = "single")
print(VaR_asset_histGB)
#modo alternativo: 
#quant_asset_histGB <- sapply(mydataGB, quantile, probs=1-percentile, type=7)
# print(quant_asset_histGB)

# PORTAFOGLIO CB
VaR_asset_histCB <- VaR(R = daily_returnsCB, p=percentile, method="historical",
                        portfolio_method = "single")
print(VaR_asset_histCB)
#modo alternativo :
#quant_asset_histCB <- sapply(mydataCB, quantile, probs=1-percentile, type=7)
#print(quant_asset_histCB)

# Creare la specificazione dell'oggetto del portafogli oche sarà fillata dai pesi 
#
WconsGB <- portfolio.spec(assets = colnames(mydataGB))
WconsCB <- portfolio.spec(assets = colnames(mydataCB))
# 
# Add long_only and weight_sum = 1 constraints
#

# 0<w<1
WconsGB <- add.constraint(portfolio = WconsGB, type='box', min=0, max=1) 
WconsCB <- add.constraint(portfolio = WconsCB, type='box', min=0, max=1)

#aiuta convergenza algoritmica non mettere 1. w1,w2,wn =1
WconsGB <- add.constraint( portfolio=WconsGB, type = "weight_sum",
                         min_sum=0.99, max_sum=1.01)
WconsCB <- add.constraint( portfolio=WconsCB, type = "weight_sum",
                           min_sum=0.99, max_sum=1.01)


# min VaR usando i ritorni storici 
# portfolio_method ="component" dice di calcolare il peso 

ObjSpec_histVaRGB$assets

ObjSpec_histVaRGB = add.objective(portfolio = WconsGB, type = "risk", 
                                  name = "VaR", 
                                  arguments=list(p=percentile, method="historical",
                                                 portfolio_method="component"),
                                  enabled=TRUE)
opt_resultVaRGB <-  optimize.portfolio(R =daily_returnsGB, portfolio=ObjSpec_histVaRGB, 
                                       search_size = 2000, trace = TRUE)
print(opt_resultVaRGB)
colnames(daily_returnsGB)









#setdiff(colnames(daily_returnsGB), ObjSpec_histVaRGB$assets)
#setdiff(ObjSpec_histVaRGB$assets, colnames(daily_returnsGB))

# confronta i nomi asset (senza la colonna 'date')
#identical(colnames(daily_returnsGB)[colnames(daily_returnsGB)!="date"],
          names(ObjSpec_histVaRGB$assets))

setdiff(colnames(daily_returnsGB)[colnames(daily_returnsGB)!="date"],
        names(ObjSpec_histVaRGB$assets))

setdiff(names(ObjSpec_histVaRGB$assets),
        colnames(daily_returnsGB)[colnames(daily_returnsGB)!="date"])




