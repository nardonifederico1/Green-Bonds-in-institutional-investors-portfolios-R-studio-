### VaR MIN Portfolio EF 2021-2025 (weight allocation)

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
library(zoo)  
library(PortfolioAnalytics)
library(PerformanceAnalytics)  # for VaR/ES
library(xts)
library(quantmod)
library(DEoptim)               

# VAROPTIMIZER (VaR min) NLOPTR

percentile <- 0.95 #confidence level 95% VaR

tickers <- symbols
start_date <- "2021-05-31"
end_date <- "2025-07-31"

#
# get Adjusted Close prices from Yahoo Finance
# 
pricesVAR <- xts()
for( tick in tickers) {
  pricesVAR <- merge(pricesVAR, getSymbols(Symbols=tick, from=start_date, 
                                           auto.assign=FALSE)[,paste(tick,"Adjusted",sep=".")])
}
colnames(pricesVAR) <- tickers

# transform index from POSIXct to Date class
#
index(pricesVAR) <- as.Date(index(pricesVAR))

# 1) Clean NA 
pricesVAR <- na.omit(pricesVAR)

# 2) Dataset size cleanGB vs cleanCB
diff_VAR <- setdiff(index(pricesVAR), prices_wideGB_sync$date)  


# 3) counting and printing dates
length(diff_VAR)   # should be  35 (1078-1043)


as.Date(diff_VAR, origin = "1970-01-01")


# Le date da rimuovere
dates_to_removeVAR <- as.Date(c( 
  "2025-07-31", "2025-08-01", "2025-08-04", "2025-08-05", "2025-08-06", "2025-08-07", "2025-08-08",
  "2025-08-11", "2025-08-12", "2025-08-13", "2025-08-14", "2025-08-18", "2025-08-19", "2025-08-20",
  "2025-08-21", "2025-08-22", "2025-08-26", "2025-08-27", "2025-08-28", "2025-08-29", "2025-09-01",
  "2025-09-02", "2025-09-03", "2025-09-04", "2025-09-05", "2025-09-08", "2025-09-09", "2025-09-10",
  "2025-09-11", "2025-09-12", "2025-09-15", "2025-09-16", "2025-09-17", "2025-09-18", "2025-09-19",
  "2025-09-22"
))

# delete GB_clean
prices_syncVAR <- pricesVAR[ !(index(pricesVAR) %in% dates_to_removeVAR), ]

# checking 
nrow(prices_syncVAR) # 1043
any(index(prices_syncVAR) %in% dates_to_removeVAR) # False = every unmatched date has been deleted





# compute returns
#  
testData_return <- diff(prices_syncVAR, arithmetic=FALSE, na.pad=FALSE) - 1

VaR_asset_hist <- VaR(R = testData_return, p=percentile, method="historical",
                      portfolio_method = "single")
print(VaR_asset_hist)

# same thing but in %
VaR_asset_hist_pct <- 100 * VaR_asset_hist
print(apply(round(VaR_asset_hist_pct, 2), 2, function(x) paste0(x, "%")))



###################################################################################################
####### GBs exhibit a slightly higher VaR compared to CBs, suggesting good resilience     ########
####### but marginally lower than that observed for the analyzed CB portfolios. It is     ########
####### therefore necessary to assess whether advantages emerge in terms of Sharpe ratio  ########
####### or higher returns and volatility in the case of GBs, which could justify a        ########
####### preference for these instruments.                                                  ########
###################################################################################################





##############################################
## DIMENSIONS
##############################################

n_assetGB <- ncol(mydataGB) 
n_assetCB <- ncol(mydataCB) 

library(nloptr)

# lists
weight_historyGB <- list()
weight_historyCB <- list()


##############################################
##  VaR  Function
##############################################

VaR_GB <- function(W, alpha = 0.95) {
  weightsGB <- as.numeric(W)
  port_returnsGB <- rowSums(sweep(mydataGB, 2, weightsGB, `*`))
  rf_values <- as.numeric(rf_sync$RF)
  excess <- port_returnsGB - rf_values
  losses <- -excess
  VaR <- as.numeric(quantile(losses, probs = alpha, na.rm = TRUE, names = FALSE))
  weight_historyGB <<- append(weight_historyGB, list(weightsGB))
  return(VaR)
}

VaR_CB <- function(W, alpha = 0.95) {
  weightsCB <- as.numeric(W)
  port_returnsCB <- rowSums(sweep(mydataCB, 2, weightsCB, `*`))
  rf_values <- as.numeric(rf_sync$RF)
  excess <- port_returnsCB - rf_values
  losses <- -excess
  VaR <- as.numeric(quantile(losses, probs = alpha, na.rm = TRUE, names = FALSE))
  weight_historyCB <<- append(weight_historyCB, list(weightsCB))
  return(VaR)
}

##############################################
## Constraints
##############################################
constraint_eq <- function(W) {
  return(sum(W) - 1)
}

lower_boundsGB <- rep(0, n_assetGB)  
upper_boundsGB <- rep(1, n_assetGB)  
lower_boundsCB <- rep(0, n_assetCB)  
upper_boundsCB <- rep(1, n_assetCB) 

initial_weightsGB <- rep(1 / n_assetGB, n_assetGB)
initial_weightsCB <- rep(1 / n_assetCB, n_assetCB)

##############################################
## OPTIMIZATION (Min VaR)
##############################################
opt_resultVaR_GB <- nloptr(
  x0 = initial_weightsGB, 
  eval_f = function(W) VaR_GB(W, alpha = 0.95), 
  lb = lower_boundsGB, 
  ub = upper_boundsGB,
  eval_g_eq = constraint_eq,
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

opt_resultVaR_CB <- nloptr(
  x0 = initial_weightsCB, 
  eval_f = function(W) VaR_CB(W, alpha = 0.95), 
  lb = lower_boundsCB, 
  ub = upper_boundsCB,
  eval_g_eq = constraint_eq,
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

##############################################
## RESULTS
##############################################
# no scientific notation
options(scipen = 999, digits = 10) 

W_GB      <- opt_resultVaR_GB$solution      
VaR_min_GB <- -opt_resultVaR_GB$objective      

W_CB      <- opt_resultVaR_CB$solution     
VaR_min_CB <- -opt_resultVaR_CB$objective       

cat("Weights ottimali GB portfolio:" , W_GB, "\n")
cat("VaR GB minimo alfa 0.95:", VaR_min_GB, "\n")


cat("Optimal weights CB portfolio:" , W_CB, "\n")
cat("VaR CB min alpha 0.95:", VaR_min_CB, "\n")

# OPPURE

# Definizione ticker
ETF_VaR_GB <- c("GRON.MI","IPRE.DE", "XSX6.MI")
ETF_VaR_CB <- c("IEAA.L","IPRE.DE", "XSX6.MI")


# Stampa GB
cat("GB Portfolio VaR 2021-25:\n")
for (i in seq_along(ETF_VaR_GB)) {
  cat(ETF_VaR_GB[i], ":", round(W_GB[i], 4), "\n")
}
cat("VaR min GB 2021-25:", VaR_min_GB, "\n\n")


cat("CB Portfolio VaR 2021-25:\n")
for (i in seq_along(ETF_VaR_CB)) {
  cat(ETF_VaR_CB[i], ":", round(W_CB[i], 4), "\n")
}
cat("VaR min CB 2021-25:", VaR_min_CB, "\n\n")



#########################################################################
##              VaR MONTHLY STRATEGY on BOTH PORTFOLIO                 ##
#########################################################################

### VaR MIN Portfolio Monthly 2021-2025 (weight allocation)
library(dplyr)
library(lubridate)
library(nloptr)

confidence <- 0.95

# 1) add the column 'month_start' 
daily_returnsGB <- daily_returnsGB %>%
  mutate(month_start = floor_date(date, "month"))
daily_returnsCB <- daily_returnsCB %>%
  mutate(month_start = floor_date(date, "month"))

# same procedure for rf_sync , to index by month
rf_sync <- rf_sync %>%
  mutate(month_start = month)

monthly_results_VaRGB <- list()
monthly_results_VaRCB <- list()

for (m_date in sort(unique(daily_returnsGB$month_start))) {
  
  window_GB <- daily_returnsGB %>%
    dplyr::filter(.data$month_start == m_date) %>%
    dplyr::arrange(.data$date)
  
  window_CB <- daily_returnsCB %>%
    dplyr::filter(.data$month_start == m_date) %>%
    dplyr::arrange(.data$date)
  
  
  X_GB <- window_GB %>% select(-date, -month, -month_start) %>% as.matrix()
  X_CB <- window_CB %>% select(-date, -month, -month_start) %>% as.matrix()
  if (nrow(X_GB) == 0 || nrow(X_CB) == 0) next
  
  # 3) RF for every month
  rf_vecCB <- rep(as.numeric(rf_sync$RF[rf_sync$month == m][1]), nrow(window_CB))
  rf_vecGB <- rep(as.numeric(rf_sync$RF[rf_sync$month == m][1]), nrow(window_CB))

  VaR_min_mGB <- function(W) {
    pr <- rowSums(sweep(X_GB, 2, as.numeric(W), `*`))
    losses <- -(pr - rf_vecGB)
    as.numeric(quantile(losses, probs = confidence, na.rm = TRUE, names = FALSE))
  }
  VaR_min_mCB <- function(W) {
    pr <- rowSums(sweep(X_CB, 2, as.numeric(W), `*`))
    losses <- -(pr - rf_vecCB)
    as.numeric(quantile(losses, probs = confidence, na.rm = TRUE, names = FALSE))
  }
  
  opt_VaR_mGB <- nloptr::nloptr(
    x0        = rep(1/ncol(X_GB), ncol(X_GB)),
    eval_f    = VaR_min_mGB,
    lb        = rep(0, ncol(X_GB)),
    ub        = rep(1, ncol(X_GB)),
    eval_g_eq = function(w) sum(w) - 1,
    opts      = list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-6)
  )
  opt_VaR_mCB <- nloptr::nloptr(
    x0        = rep(1/ncol(X_CB), ncol(X_CB)),
    eval_f    = VaR_min_mCB,
    lb        = rep(0, ncol(X_CB)),
    ub        = rep(1, ncol(X_CB)),
    eval_g_eq = function(w) sum(w) - 1,
    opts      = list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-6)
  )
  
  monthly_results_VaRGB <- append(monthly_results_VaRGB, list(data.frame(
    Month   = m_date,  
    XSX6.MI = opt_VaR_mGB$solution[ match("XSX6.MI", colnames(X_GB)) ],
    GRON.MI = opt_VaR_mGB$solution[ match("GRON.MI", colnames(X_GB)) ],
    IPRE.DE = opt_VaR_mGB$solution[ match("IPRE.DE", colnames(X_GB)) ],
    VaRm    = opt_VaR_mGB$objective
  )))
  monthly_results_VaRCB <- append(monthly_results_VaRCB, list(data.frame(
    Month   = m_date, 
    XSX6.MI = opt_VaR_mCB$solution[ match("XSX6.MI", colnames(X_CB)) ],
    IEAA.L  = opt_VaR_mCB$solution[ match("IEAA.L",  colnames(X_CB)) ],
    IPRE.DE = opt_VaR_mCB$solution[ match("IPRE.DE", colnames(X_CB)) ],
    VaRm    = opt_VaR_mCB$objective
  )))
}

monthly_results_VaRGB <- dplyr::bind_rows(monthly_results_VaRGB)
monthly_results_VaRCB <- dplyr::bind_rows(monthly_results_VaRCB)


monthly_results_VaRGB <- monthly_results_VaRGB %>%
  mutate(Month = as.Date(Month, origin = "1970-01-01"),
         Month = format(Month, "%Y-%m"))

monthly_results_VaRCB <- monthly_results_VaRCB %>%
  mutate(Month = as.Date(Month, origin = "1970-01-01"),
         Month = format(Month, "%Y-%m"))


monthly_results_VaRCB <- monthly_results_VaRCB %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
monthly_results_VaRGB <- monthly_results_VaRGB %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

View(monthly_results_VaRGB)
View(monthly_results_VaRCB)



#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################


#########################################################################
# MEAN RETURN AND VOLATILITY ADDED TO MONTHLY RESULTS
#########################################################################
#########
#GB
#########

# Monthly return GB
monthly_results_VaRGB$MeanRet <- sapply(monthly_results_VaRGB$Month, function(m) {
  X <- daily_returnsGB %>%
    filter(floor_date(date, "month") == as.Date(paste0(m, "-01"))) %>%
    select(-date, -month)
  w <- as.numeric(monthly_results_VaRGB[monthly_results_VaRGB$Month == m,
                                       c("XSX6.MI","GRON.MI","IPRE.DE")])
  
  # compute daily return of the portfolio and then the average
  port <- as.numeric(as.matrix(X) %*% w)
  mean(port, na.rm = TRUE)
})

monthly_results_VaRGB$Vol <- sapply(monthly_results_VaRGB$Month, function(m) {
  X <- daily_returnsGB %>%
    filter(floor_date(date, "month") == as.Date(paste0(m, "-01"))) %>%
    select(-date, -month) %>%
    as.matrix()
  w <- as.numeric(monthly_results_VaRGB[monthly_results_VaRGB$Month == m,
                                       c("XSX6.MI","GRON.MI","IPRE.DE")])
  sd(as.numeric(X %*% w), na.rm = TRUE)
})

# monthly return CB
monthly_results_VaRCB$MeanRet <- sapply(monthly_results_VaRCB$Month, function(m) {
  X <- daily_returnsCB %>%
    filter(floor_date(date, "month") == as.Date(paste0(m, "-01"))) %>%
    select(-date, -month)
  w <- as.numeric(monthly_results_VaRCB[monthly_results_VaRCB$Month == m,
                                        c("XSX6.MI","IEAA.L","IPRE.DE")])
  port <- as.numeric(as.matrix(X) %*% w)
  mean(port, na.rm = TRUE)
})

# Volatility Monthly basis CB
monthly_results_VaRCB$Vol <- sapply(monthly_results_VaRCB$Month, function(m) {
  X <- daily_returnsCB %>%
    filter(floor_date(date, "month") == as.Date(paste0(m, "-01"))) %>%
    select(-date, -month) %>%
    as.matrix()
  w <- as.numeric(monthly_results_VaRCB[monthly_results_VaRCB$Month == m,
                                        c("XSX6.MI","IEAA.L","IPRE.DE")])
  sd(as.numeric(X %*% w), na.rm = TRUE)
})


