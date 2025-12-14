#VAR OPTIMIZER ESPERIMENTO DEBUGGING CON DIMENSIONE CAMPIONARIA GRANDE

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
library(xts)
library(quantmod)
library(DEoptim)               # se usi optimize_method = "DEoptim"

percentile <- 0.95 #livello di confidenze 95% VaR

yep <-  c("AAPL", "MSFT", "AMZN")
start_date <- "2001-05-31"
end_date <- "2025-07-31"

#
# get Adjusted Close prices from Yahoo Finance
# 
pricesVAR2 <- xts()
for( tick in yep) {
  pricesVAR2 <- merge(pricesVAR2, getSymbols(Symbols=tick, from=start_date, 
                                           auto.assign=FALSE)[,paste(tick,"Adjusted",sep=".")])
}
colnames(pricesVAR2) <- yep

# transform index from POSIXct to Date class
#
index(pricesVAR2) <- as.Date(index(pricesVAR2))

# 1) Pulisco dagli NA 
pricesVAR2 <- na.omit(pricesVAR2)


# compute returns
#  
testData_return2 <- diff(pricesVAR2, arithmetic=FALSE, na.pad=FALSE) - 1
#

#  Compare VaR with quantile calculations for assets
#  when portfolio_method = "single" in VaR, the VaR for each column in R is calculated 
# questa è la VaR giornaliera
#
VaR_asset_hist2 <- VaR(R = testData_return2, p=percentile, method="historical",
                      portfolio_method = "single")
print(VaR_asset_hist2)

VaR_asset_hist_pct2 <- 100 * VaR_asset_hist2
print(apply(round(VaR_asset_hist_pct2, 2), 2, function(x) paste0(x, "%")))

#stesso calcolo ma utilizzando il quinto percentile. 

quant_asset_hist2 <- sapply(testData_return2, quantile, probs=1-percentile, type=7)
print(quant_asset_hist2)  

###################################################################################################
####### I GB hanno un VAr leggermente maggiore rispetto ai CB, quindi sembrano             ########
####### mostrare una buona resilienza ma minore erispetto ai CB analizzati. bisogna        ########
####### capire se ci sono vantaggi invece di sharpe ratio o rendimento e volatilità        ########
####### leggermente maggiori nel caso dei GB, perchè così si potrebbe spingere per quelli  ########
###################################################################################################

# creo pesi di specificazione con cui costruire il portafoglio 
#
Wcons2<- portfolio.spec(assets = colnames(testData_return2))
# 
# Add long_only and weight_sum = 1 constraints
#
Wcons2 <- add.constraint(portfolio = Wcons2, type='box', min=0, max=1)  
Wcons2 <- add.constraint( portfolio=Wcons2, type = "weight_sum",
                         min_sum=0.99, max_sum=1.01)          
#
# Set the objective to minimize VaR using historical returns
# portfolio_method ="component" tells VaR to use values of weights argument and calculate VaR for the portfolio
#
ObjSpec_hist2 = add.objective(portfolio = Wcons2, type = "risk", 
                             name = "VaR", 
                             arguments=list(p=percentile, method="historical",
                                            portfolio_method="single"),
                             enabled=TRUE)

## sol oquesto pezzettino è sbagliato

optVAR2 <-  optimize.portfolio(R =testData_return2, portfolio=ObjSpec_hist2, 
                              search_size = 20000, trace = TRUE)
print(optVAR2)
