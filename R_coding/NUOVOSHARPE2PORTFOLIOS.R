##############################################
## LIBRARIES
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
library(zoo)  


##############################################
## DATA
##############################################
symbols <- c("XSX6.MI","GRON.MI", "IEAA.L","IPRE.DE") 
start_date <- "2021-05-31"
end_date <- "2025-07-31"

prices <- tq_get(symbols,
                 from = start_date,
                 to = end_date,
                 get = "stock.prices")

PricesGB <- c("XSX6.MI", "GRON.MI", "IPRE.DE")
PricesCB <- c("XSX6.MI", "IEAA.L", "IPRE.DE")

Portfolio_pricesGB <- prices %>%
  filter(symbol %in% PricesGB) %>%
  select(date, symbol, adjusted) %>%
  arrange(symbol, date)

Portfolio_pricesCB <- prices %>%
  filter(symbol %in% PricesCB) %>%
  select(date, symbol, adjusted) %>%
  arrange(symbol, date)

prices_wideGB <- Portfolio_pricesGB %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  arrange(date)

prices_wideCB <- Portfolio_pricesCB %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  arrange(date)

##############################################
## ADJUSTING DATASETS and OMIT NA
##############################################

# 1) CLEANING NA 
prices_wideGB_clean <- na.omit(prices_wideGB)
prices_wideCB_clean <- na.omit(prices_wideCB)

# 2) DATA SIZE
diff_GB <- setdiff(prices_wideGB_clean$date, prices_wideCB_clean$date)  # in GB ma non in CB
diff_CB <- setdiff(prices_wideCB_clean$date, prices_wideGB_clean$date)  # in CB ma non in GB

# 3) DATA SIZE 2
length(diff_GB)   # should be 19 (1062-1043)
length(diff_CB)   # should be 0 (if CB_clean is now of the same  GB_clean size)

as.Date(diff_GB, origin = "1970-01-01")
as.Date(diff_CB, origin = "1970-01-01")


# Le date da rimuovere
dates_to_remove <- as.Date(c("2021-08-30","2021-12-27","2021-12-28","2022-01-03",
                             "2022-05-02","2022-06-02","2022-06-03","2022-08-29",
                             "2022-09-19","2022-12-27","2023-01-02","2023-05-08",
                             "2023-05-29","2023-08-28","2024-05-06","2024-05-27",
                             "2024-08-26","2025-05-05","2025-05-26"))

# Elimino date da GB_clean
prices_wideGB_sync <- prices_wideGB_clean %>%
  filter(!date %in% dates_to_remove)

# Controllo: nessuna di quelle date rimasta
any(prices_wideGB_sync$date %in% dates_to_remove)   # deve dare FALSE

#creo nuovo oggetto per evitare confusione di codice
prices_wideCB_sync <- prices_wideCB_clean


##############################################
## COMPUTE RETURNS (arithmetical instead of log ) 
##############################################

daily_returnsGB <- prices_wideGB_sync %>%
  mutate(across(-date, 
                ~ (./lag(.) - 1),
                .names = "{.col}")) %>%
  slice(-1)

daily_returnsCB <- prices_wideCB_sync %>%
  mutate(across(-date, 
                ~ (./lag(.) - 1),
                .names = "{.col}")) %>%
  slice(-1)

##############################################
## CREATING MY DATA ( delete dare column to have a  numeric dataset)
##############################################
mydataGB <- daily_returnsGB[, -which(names(daily_returnsGB) == "date")]


mydataCB <- daily_returnsCB[, -which(names(daily_returnsCB) == "date")]


##############################################
## DISTRIBUTION OF RETURNS
##############################################

## GREEN BOND
xGRON <- as.numeric(mydataGB$GRON.MI)
xGRON <- xGRON[is.finite(xGRON)]   

hGRON <- hist(
  xGRON,
  breaks = "FD",
  plot   = FALSE,
  right  = FALSE,
  include.lowest = TRUE
)

plot(hGRON, freq = FALSE, border = FALSE,
     col = rgb(0.3,0.8,0.8,0.6),
     main = "Distribution returns GREEN BOND (GRON.MI)",
     xlab = "Return", ylab = "probability for each bin")
lines(density(xGRON), lwd = 2)


## STOCKS (STOXX 600)
xXSX6 <- as.numeric(mydataGB$XSX6.MI)
xXSX6 <- xXSX6[is.finite(xXSX6)]

hXSX6 <- hist(
  xXSX6,
  breaks = "FD",
  plot   = FALSE,
  right  = FALSE,
  include.lowest = TRUE
)

plot(hXSX6, freq = FALSE, border = FALSE,
     col = rgb(0.3,0.8,0.8,0.6),
     main = "Distribution returns AZIONARIO (XSX6.MI)",
     xlab = "Return", ylab = "probability for each bin")
lines(density(xXSX6), lwd = 2)


## REAL ESTATE
xIPRE <- as.numeric(mydataGB$IPRE.DE)
xIPRE <- xIPRE[is.finite(xIPRE)]

hIPRE <- hist(
  xIPRE,
  breaks = "FD",
  plot   = FALSE,
  right  = FALSE,
  include.lowest = TRUE
)

plot(hIPRE, freq = FALSE, border = FALSE,
     col = rgb(0.3,0.8,0.8,0.6),
     main = "Distribution returns REAL ESTATE (IPRE.DE)",
     xlab = "Return", ylab = "probability for each bin")
lines(density(xIPRE), lwd = 2)

## CORPORATE BONDS
xCB <- as.numeric(mydataCB$IEAA.L)
xCB <- xCB[is.finite(xCB)]

hCB <- hist(
  xCB,
  breaks = "FD",
  plot   = FALSE,
  right  = FALSE,
  include.lowest = TRUE
)

plot(hCB, freq = FALSE, border = FALSE,
     col = rgb(0.3,0.8,0.8,0.6),
     main = "Distribution returns CORPORATE BONDS (IEAA.L)",
     xlab = "Return", ylab = "probability for each bin")
lines(density(xCB), lwd = 2)


##############################################
## COVARIANCE
##############################################


COVARIANCEGB <- cov(mydataGB)
print(COVARIANZAGB)
  
COVARIANCECB <- cov(mydataCB)
print(COVARIANCECB)

##############################################
## RISK-FREE 
##############################################
KennethFrench <- read_xlsx("rf.xlsx")
colnames(KennethFrench) <- c("date_raw", "Mkt_RF", "SMB", "HML", "RF")
rf <- KennethFrench[, c("date_raw", "RF")]
rf$RF <- as.numeric(rf$RF) / 100


daily_returnsGB$date <- as.Date(daily_returnsGB$date)
daily_returnsCB$date <- as.Date(daily_returnsCB$date)
rf$date_raw  <- as.Date(rf$date_raw)


diff_rf_vs_GB <- setdiff(rf$date_raw, daily_returnsGB$date)
diff_rf_vs_CB <- setdiff(rf$date_raw, daily_returnsCB$date)

as.Date(diff_rf_vs_GB, origin = "1970-01-01")
as.Date(diff_rf_vs_CB, origin = "1970-01-01")

# Dates to delete
dates_to_removeRF <- as.Date(c(
  "2021-06-01","2021-08-30","2021-12-24","2021-12-27","2021-12-28","2021-12-31","2022-01-03",
  "2022-04-15","2022-04-18","2022-05-02","2022-06-02","2022-06-03","2022-08-15","2022-08-29",
  "2022-09-19","2022-12-26","2022-12-27","2023-01-02","2023-04-07","2023-04-10","2023-05-01",
  "2023-05-08","2023-05-29","2023-08-15","2023-08-28","2023-12-25","2023-12-26","2024-01-01",
  "2024-03-29","2024-04-01","2024-05-01","2024-05-06","2024-05-27","2024-08-15","2024-08-26",
  "2024-12-24","2024-12-25","2024-12-26","2024-12-31","2025-01-01","2025-04-18","2025-04-21",
  "2025-05-01","2025-05-05","2025-05-26","2025-07-05","2025-07-06","2025-07-12","2025-07-13",
  "2025-07-19","2025-07-20","2025-07-26","2025-07-27","2025-07-31"
))

# Delete dates for Rf
rf_sync <- rf %>%
  filter(!date_raw %in% dates_to_removeRF)

# checking results 
any(rf_sync$date_raw %in% dates_to_removeRF)   # should be FALSE
nrow(rf_sync)                                  # should be 1042 


##############################################
## DIMENSIONS
##############################################

n_assetGB <- ncol(mydataGB) 
n_assetCB <- ncol(mydataCB) 

library(nloptr)

# Creating lists to save the weights
weight_historyGB <- list()
weight_historyCB <- list()


##############################################
##  SHARPE RATIO FUNCTION
##############################################


sharpe_ratioGB <- function(w) {
  weightsGB <- as.numeric(w)
  port_returnsGB <- rowSums( sweep(mydataGB, 2, weightsGB, `*`) )
  rf_values <- as.numeric(rf_sync$RF)  
  sharpeGB <- mean(port_returnsGB - rf_values) / sd(port_returnsGB)
  weight_historyGB <<- append(weight_historyGB, list(weightsGB))
  return(-sharpeGB)
}

sharpe_ratioCB <- function(w) {
  weightsCB <- as.numeric(w)
  port_returnsCB <- rowSums( sweep(mydataCB, 2, weightsCB, `*`) )
  rf_values <- as.numeric(rf_sync$RF)  
  sharpeCB <- mean(port_returnsCB - rf_values) / sd(port_returnsCB)
  weight_historyCB <<- append(weight_historyCB, list(weightsCB))
  return(-sharpeCB)
}

##############################################
## CONSTRAINTS
##############################################
constraint_eq <- function(w) {
  return(sum(w) - 1)
}

lower_boundsGB <- rep(0, n_assetGB)  
upper_boundsGB <- rep(1, n_assetGB)  
lower_boundsCB <- rep(0, n_assetCB)  
upper_boundsCB <- rep(1, n_assetCB) 

initial_weightsGB <- rep(1 / n_assetGB, n_assetGB)
initial_weightsCB <- rep(1 / n_assetCB, n_assetCB)

##############################################
## OPTIMIZATION
##############################################
opt_resultGB <- nloptr(
  x0 = initial_weightsGB, 
  eval_f = sharpe_ratioGB, 
  lb = lower_boundsGB, 
  ub = upper_boundsGB,
  eval_g_eq = constraint_eq,
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

opt_resultCB <- nloptr(
  x0 = initial_weightsCB,
  eval_f = sharpe_ratioCB,
  lb = lower_boundsCB,
  ub = upper_boundsCB,
  eval_g_eq = constraint_eq,
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

##############################################
## RESULTS
##############################################
# No scientific notation  
options(scipen = 999, digits = 10)  


w_GB      <- opt_resultGB$solution         
sharpe_GB <- -opt_resultGB$objective       
w_CB      <- opt_resultCB$solution
sharpe_CB <- -opt_resultCB$objective

cat("Weights GB portfolio:" , w_GB, "\n")
cat("Sharpe GB:", sharpe_GB, "\n")
cat("Weights CB portfolio:" , w_CB, "\n")
cat("Sharpe CB:", sharpe_CB, "\n")



# OR

# Defining tickers
ETF_GB <- c("GRON.MI","IPRE.DE", "XSX6.MI")
ETF_CB <- c("IEAA.L","IPRE.DE", "XSX6.MI")

# Print GB
cat("GB Portfolio:\n")
for (i in seq_along(ETF_GB)) {
  cat(ETF_GB[i], ":", round(w_GB[i], 4), "\n")
}
cat("Sharpe GB:", sharpe_GB, "\n\n")

# Print CB
cat("CB Portfolio:\n")
for (i in seq_along(ETF_CB)) {
  cat(ETF_CB[i], ":", round(w_CB[i], 4), "\n")
}
cat("Sharpe CB:", sharpe_CB, "\n")




##########################################################################################################
##We have a corner solution, the reason behind it is linked with the overperformance of stick markets ####
## in the time frame analyzed and the objective of Sharpe ratio: minimizing the risk maximizing return.###
##########################################################################################################


##############################################
## MONTHLY STRATEGY REALLOCATION
##############################################



#group returns by month
daily_returnsGB$month <- floor_date(daily_returnsGB$date, "month")
daily_returnsCB$month <- floor_date(daily_returnsCB$date, "month")

rf_sync$date_raw  <- as.Date(rf_sync$date_raw)   
rf_sync$month <- floor_date(rf_sync$date_raw, "month")
monthly_resultsGB <- list()
monthly_resultsCB <- list()


for (m in unique(daily_returnsGB$month)) {
  
  monthly_dataGB <- daily_returnsGB %>%
    filter(month == m) %>%
    select(-date, -month)
  
  mydata_mGB <- monthly_dataGB
  rf_m <- rep(as.numeric(rf_sync$RF[rf_sync$month == m][1]), nrow(mydata_mGB))
  
  sharpe_ratio_mGB <- function(w) {
    weightsGB <- as.numeric(w)
    port_returnsGB <- rowSums(sweep(mydata_mGB, 2, weightsGB, `*`))
    srGB <- (mean(port_returnsGB) - rf_m) / sd(port_returnsGB)
    return(-srGB)
  }
  
  opt_m <- nloptr(
    x0 = rep(1/ncol(mydata_mGB), ncol(mydata_mGB)),
    eval_f = sharpe_ratio_mGB,
    lb = rep(0, ncol(mydata_mGB)),
    ub = rep(1, ncol(mydata_mGB)),
    eval_g_eq = function(w) sum(w) - 1,
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
  )
  
  monthly_resultsGB[[length(monthly_resultsGB) + 1]] <- data.frame(
    Month = as.Date(m),  
    XSX6.MI = opt_m$solution[1],
    GRON.MI = opt_m$solution[2],
    IPRE.DE  = opt_m$solution[3],
    Sharpe  = -opt_m$objective
  )
}


# Merge into a single dataset wide
monthly_results_dfGB <- bind_rows(monthly_resultsGB)

# Formatting by "YYYY-MM"
monthly_results_dfGB <- monthly_results_dfGB %>%
  mutate(Month = format(Month, "%Y-%m"))

# Round results 
monthly_results_dfGB <- monthly_results_dfGB %>%
  mutate(across(c(XSX6.MI, GRON.MI, IPRE.DE, Sharpe),
                ~ round(., 4)))

# Visualize final table
View(monthly_results_dfGB)
print(monthly_results_dfGB)

############ WITH CB PORTFOLIO#########
for (m in unique(daily_returnsCB$month)) {
  
  monthly_dataCB <- daily_returnsCB %>%
    filter(month == m) %>%
    select(-date, -month)
  
  mydata_mCB <- monthly_dataCB
  rf_mCB <- rep(as.numeric(rf_sync$RF[rf_sync$month == m][1]), nrow(mydata_mCB))
  
  sharpe_ratio_mCB <- function(w) {
    weightsCB <- as.numeric(w)
    port_returnsCB <- rowSums(sweep(mydata_mCB, 2, weightsCB, `*`))
    srCB <- (mean(port_returnsCB) - rf_m) / sd(port_returnsCB)
    return(-srCB)
  }
  
  opt_mCB <- nloptr(
    x0 = rep(1/ncol(mydata_mCB), ncol(mydata_mCB)),
    eval_f = sharpe_ratio_mCB,
    lb = rep(0, ncol(mydata_mCB)),
    ub = rep(1, ncol(mydata_mCB)),
    eval_g_eq = function(w) sum(w) - 1,
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
  )
  
  monthly_resultsCB[[length(monthly_resultsCB) + 1]] <- data.frame(
    Month = as.Date(m),  
    XSX6.MI = opt_mCB$solution[1],
    IEAA.L = opt_mCB$solution[2],
    IPRE.DE  = opt_mCB$solution[3],
    Sharpe  = -opt_mCB$objective
  )
}

# Merge in a single dataset wide
monthly_results_dfCB <- bind_rows(monthly_resultsCB)

# Format as "YYYY-MM"
monthly_results_dfCB <- monthly_results_dfCB %>%
  mutate(Month = format(Month, "%Y-%m"))

# Round weights
monthly_results_dfCB <- monthly_results_dfCB %>%
  mutate(across(c(XSX6.MI, IEAA.L, IPRE.DE, Sharpe),
                ~ round(., 4)))

# View final summary table
View(monthly_results_dfCB)
print(monthly_results_dfCB)

##Look into details the Sharpe ratio results on a monthly basis ##


monthly_results_dfGB$Vol <- sapply(monthly_results_dfGB$Month, function(m) {
  X <- daily_returnsGB %>%
    filter(floor_date(date, "month") == as.Date(paste0(m, "-01"))) %>%
    select(-date, -month) %>%
    as.matrix()
  w <- as.numeric(monthly_results_dfGB[monthly_results_dfGB$Month == m, c("XSX6.MI","GRON.MI","IPRE.DE")])
  sd(as.numeric(X %*% w), na.rm = TRUE)
})

monthly_results_dfGB$MeanRet <- sapply(monthly_results_dfGB$Month, function(m) {
  X <- daily_returnsGB %>%
    filter(floor_date(date, "month") == as.Date(paste0(m, "-01"))) %>%
    select(-date, -month)
  w <- as.numeric(monthly_results_dfGB[monthly_results_dfGB$Month == m,
                                       c("XSX6.MI","GRON.MI","IPRE.DE")])
  port <- as.numeric(as.matrix(X) %*% w)
  mean(port, na.rm = TRUE)
})
monthly_results_dfCB$Vol <- sapply(monthly_results_dfCB$Month, function(m) {
  X <- daily_returnsCB %>%
    filter(floor_date(date, "month") == as.Date(paste0(m, "-01"))) %>%
    select(-date, -month) %>%
    as.matrix()
  w <- as.numeric(monthly_results_dfCB[monthly_results_dfCB$Month == m,
                                       c("XSX6.MI","IEAA.L","IPRE.DE")])
  
  sd(as.numeric(X %*% w), na.rm = TRUE)
})
monthly_results_dfCB$MeanRet <- sapply(monthly_results_dfCB$Month, function(m) {
  X <- daily_returnsCB %>%
    filter(floor_date(date, "month") == as.Date(paste0(m, "-01"))) %>%
    select(-date, -month)
  w <- as.numeric(monthly_results_dfCB[monthly_results_dfCB$Month == m,
                                       c("XSX6.MI","IEAA.L","IPRE.DE")])
  port <- as.numeric(as.matrix(X) %*% w)
  mean(port, na.rm = TRUE)
})


