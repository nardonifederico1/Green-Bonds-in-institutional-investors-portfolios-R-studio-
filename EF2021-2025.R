########################################################################
######### EFFICIENT FRONTIER TUTTO IL PERIODO (max sharpe) #############
########################################################################

library(fPortfolio)
library(timeSeries)
library(dplyr)

## === INPUT attesi ===
## mydataGB: data frame/matrix solo numerico (colonne = asset), senza NA
## daily_returnsGB$date: vettore Date, stessa lunghezza di mydataGB
## rf_sync$RF: risk-free giornaliero (serie), stesso calendario

stopifnot(nrow(mydataGB) == nrow(daily_returnsGB))  # per sicurezza

# 1) timeSeries per fPortfolio
R_GB <- timeSeries(as.matrix(mydataGB), charvec = daily_returnsGB$date)

# 2) Risk-free giornaliero come SCALARE, uso la media 
rf_daily <- mean(rf_sync$RF, na.rm = TRUE)

# 3) Specifica MV e vincoli (long-only)
spec <- portfolioSpec()
setRiskFreeRate(spec)   <- rf_daily
setNFrontierPoints(spec) <- 100
constraints <- "LongOnly"

# 4) Frontiera efficiente
front_GB <- portfolioFrontier(R_GB, spec = spec, constraints = constraints , include.mvl = TRUE, title = "EFFICIENT FRONTIER GB SHARPE")

# 5) Portafogli notevoli (
gmv_GB <- minvariancePortfolio(R_GB, spec = spec, constraints = constraints)
tan_GB <- tangencyPortfolio(  R_GB, spec = spec, constraints = constraints) 

# 6) Plot: frontiera + CML + punti chiave 
plot(front_GB)                                              # frontiera (nero=efficiente, grigio=inefficiente)
tangencyLines(front_GB, col = "red", lwd = 2)               # CML
minvariancePoints(front_GB, pch = 21, col = "blue",  bg = "blue",  cex = 1.3)  # GMV (min varianza)
tangencyPoints(front_GB,   pch = 21, col = "red",   bg = "red",   cex = 1.3)    # Tangenza (max Sharpe)
singleAssetPoints(front_GB, pch = 21, col = "orange", bg = "orange", cex = 1.1) # Singoli asset
equalWeightsPoints(front_GB, pch = 21, col = "darkgreen", bg = "darkgreen", cex = 1.1) # Equal-weight


w_gmvGB <- getWeights(gmv_GB) #portafoglio con varianza minima punto blu
w_tanGB <- getWeights(tan_GB) #portafoglio di tangenza con la CML
print(round(w_gmvGB, 4)); print(round(w_tanGB, 4))



#######################################################################
### PER CB 

stopifnot(nrow(mydataCB) == nrow(daily_returnsCB))  # sicurezza

# 1) timeSeries per fPortfolio
R_CB <- timeSeries(as.matrix(mydataCB), charvec = daily_returnsCB$date)

# 2) Risk-free giornaliero come SCALARE, uso la media 
rf_daily <- mean(rf_sync$RF, na.rm = TRUE)

# 3) Specifico MV e vincoli (long-only)
specCB <- portfolioSpec()
setRiskFreeRate(specCB)   <- rf_daily
setNFrontierPoints(specCB) <- 100
constraintsCB <- "LongOnly"

# 4) Frontiera efficiente
front_CB <- portfolioFrontier(R_CB, spec = specCB, constraints = constraintsCB , include.mvl = TRUE, title = "EFFICIENT FRONTIER CB SHARPE")

# 5) Portafogli notevoli 
gmv_CB <- minvariancePortfolio(R_CB, spec = specCB, constraints = constraintsCB)
tan_CB <- tangencyPortfolio(R_CB, spec = specCB, constraints = constraintsCB) 

# 6) Plot: frontiera + CML + punti chiave (colori semplici)
plot(front_CB)                                              # frontiera (nero=efficiente, grigio=inefficiente)
tangencyLines(front_CB, col = "red", lwd = 2)               # CML
minvariancePoints(front_CB, pch = 21, col = "blue",  bg = "blue",  cex = 1.3)  # GMV (min varianza)
tangencyPoints(front_CB,   pch = 21, col = "red",   bg = "red",   cex = 1.3)    # Tangenza (max Sharpe)
singleAssetPoints(front_CB, pch = 21, col = "orange", bg = "orange", cex = 1.1) # Singoli asset
equalWeightsPoints(front_CB, pch = 21, col = "darkgreen", bg = "darkgreen", cex = 1.1) # Equal-weight


w_gmvCB <- getWeights(gmv_CB) #portafoglio con varianza minima punto blu
w_tanCB <- getWeights(tan_CB) #portafoglio di tangenza con la CML
print(round(w_gmvCB, 4)); print(round(w_tanCB, 4))
