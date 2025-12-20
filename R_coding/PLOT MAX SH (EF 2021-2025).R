########################################################################
######### EFFICIENT FRONTIER PLOT 2021-2025(max sharpe) #############
########################################################################

library(fPortfolio)
library(timeSeries)
library(dplyr)
stopifnot(nrow(mydataGB) == nrow(daily_returnsGB))  

# 1) timeSeries for fPortfolio
R_GB <- timeSeries(as.matrix(mydataGB), charvec = daily_returnsGB$date)

# 2) Risk-free daily using the average
rf_daily <- mean(rf_sync$RF, na.rm = TRUE)

# 3) SPecific constraints
spec <- portfolioSpec()
setRiskFreeRate(spec)   <- rf_daily
setNFrontierPoints(spec) <- 100
constraints <- "LongOnly"

# 4) Efficient Frontier
front_GB <- portfolioFrontier(R_GB, spec = spec, constraints = constraints , include.mvl = TRUE, title = "EFFICIENT FRONTIER GB SHARPE")

# 5) Portfolios
gmv_GB <- minvariancePortfolio(R_GB, spec = spec, constraints = constraints)
tan_GB <- tangencyPortfolio(  R_GB, spec = spec, constraints = constraints) 

# 6) Plot: EF + CML + punti chiave 
plot(front_GB)                                              # efficient frontier 
tangencyLines(front_GB, col = "red", lwd = 2)               # CML
minvariancePoints(front_GB, pch = 21, col = "blue",  bg = "blue",  cex = 1.3)  # GMV (min variance)
tangencyPoints(front_GB,   pch = 21, col = "red",   bg = "red",   cex = 1.3)    # Tangency (max Sharpe)
singleAssetPoints(front_GB, pch = 21, col = "orange", bg = "orange", cex = 1.1) # Single assets
equalWeightsPoints(front_GB, pch = 21, col = "darkgreen", bg = "darkgreen", cex = 1.1) # Equal-weight


w_gmvGB <- getWeights(gmv_GB) #portafolio with minimum variance (blue point) 
w_tanGB <- getWeights(tan_GB) #portfolio tangent to CML
print(round(w_gmvGB, 4)); print(round(w_tanGB, 4))

#######################################################################
### FOR CB 
stopifnot(nrow(mydataCB) == nrow(daily_returnsCB))  
R_CB <- timeSeries(as.matrix(mydataCB), charvec = daily_returnsCB$date)
rf_daily <- mean(rf_sync$RF, na.rm = TRUE)
specCB <- portfolioSpec()
setRiskFreeRate(specCB)   <- rf_daily
setNFrontierPoints(specCB) <- 100
constraintsCB <- "LongOnly"
front_CB <- portfolioFrontier(R_CB, spec = specCB, constraints = constraintsCB , include.mvl = TRUE, title = "EFFICIENT FRONTIER CB SHARPE")
gmv_CB <- minvariancePortfolio(R_CB, spec = specCB, constraints = constraintsCB)
tan_CB <- tangencyPortfolio(R_CB, spec = specCB, constraints = constraintsCB) 
plot(front_CB)                                              
tangencyLines(front_CB, col = "red", lwd = 2)               
minvariancePoints(front_CB, pch = 21, col = "blue",  bg = "blue",  cex = 1.3) 
tangencyPoints(front_CB,   pch = 21, col = "red",   bg = "red",   cex = 1.3)    
singleAssetPoints(front_CB, pch = 21, col = "orange", bg = "orange", cex = 1.1) 
equalWeightsPoints(front_CB, pch = 21, col = "darkgreen", bg = "darkgreen", cex = 1.1) 
w_gmvCB <- getWeights(gmv_CB) 
w_tanCB <- getWeights(tan_CB) 
print(round(w_gmvCB, 4)); print(round(w_tanCB, 4))
