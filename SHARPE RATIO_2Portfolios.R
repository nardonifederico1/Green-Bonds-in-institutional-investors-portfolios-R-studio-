##############################################
## LIBRERIE
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

##############################################
## IMPORTO DATI
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
## SINCRONIZZAZIONE DATE E GESTIONE NA
##############################################
#in questo modo Gb ha le stesse osservazioni di CB
all_dates <- full_join(
  prices_wideGB %>% select(date),
  prices_wideCB %>% select(date),
  by = "date"
) %>% arrange(date)

prices_wideGB_sync <- all_dates %>%
  left_join(prices_wideGB, by = "date") %>%
  arrange(date) %>%
  mutate(across(-date, ~ na.locf(.x, na.rm = FALSE)))

prices_wideGB_sync <- prices_wideGB_sync[-1,]

prices_wideCB_sync <- all_dates %>%
  left_join(prices_wideCB, by = "date") %>%
  arrange(date) %>%
  mutate(across(-date, ~ na.locf(.x, na.rm = FALSE)))

prices_wideCB_sync <- prices_wideCB_sync[-1,]

##############################################
## CALCOLO RETURNS
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
## CREAZIONE MATRICI
##############################################
mydataGB <- daily_returnsGB[, -which(names(daily_returnsGB) == "date")]
mydataGB <- as.matrix(mydataGB)
mode(mydataGB) <- "numeric"

mydataCB <- daily_returnsCB[, -which(names(daily_returnsCB) == "date")]
mydataCB <- as.matrix(mydataCB)
mode(mydataCB) <- "numeric"

#calcolo covarianza

COVARIANZAGB <- cov(mydataGB, use = "pairwise.complete.obs")
print(COVARIANZAGB)

COVARIANZACB <- cov(mydataCB, use = "pairwise.complete.obs")
print(COVARIANZACB)

##############################################
## RISK-FREE
##############################################
rf <- read_xlsx("rf.xlsx")
colnames(rf) <- c("date_raw", "Mkt_RF", "SMB", "HML", "RF")
#rf$date <- ymd(as.character(rf$date_raw))
rf <- rf[, c("date_raw", "RF")]
rf$RF <- as.numeric(rf$RF) / 100



#ora trovo date in comune rf con i mydataGB e mydataCB

library(dplyr)
library(lubridate)
library(readxl)

# Leggo il file RF
rf <- read_xlsx("rf.xlsx", skip = 1)
colnames(rf) <- c("date_raw", "Mkt_RF", "SMB", "HML", "RF")

# Converto la colonna date
rf$date <- ymd(as.character(rf$date_raw))
rf$RF <- as.numeric(rf$RF) / 100

# Dataset portfolio (assumo che abbiano una colonna date)
dates_GB <- daily_returnsGB$date
dates_CB <- daily_returnsCB$date

# Filtro RF per date comuni con GB
rf_GB <- rf %>% filter(date %in% dates_GB)

# Filtro RF per date comuni con CB
rf_CB <- rf %>% filter(date %in% dates_CB)



library(dplyr)
library(zoo)

##############################################
## DIMENSIONI
##############################################

n_assetGB <- ncol(mydataGB) 
n_assetCB <- ncol(mydataCB) 

library(nloptr)

# Inizializzo liste per salvare i pesi provati
weight_historyGB <- list()
weight_historyCB <- list()


##############################################
## FUNZIONI SHARPE
##############################################

sharpe_ratioGB <- function(weights) {
  weightsGB <- as.numeric(weights)
  port_returnsGB <- mydataGB %*% weightsGB
  rf_values <- as.numeric(rf_GB$RF)  
  sharpeGB <- mean(port_returnsGB - rf_values) / sd(port_returnsGB)
  weight_historyGB <<- append(weight_historyGB, list(weightsGB))
  return(-sharpeGB)
}

sharpe_ratioCB <- function(weights) {
  weightsCB <- as.numeric(weights)
  port_returnsCB <- mydataCB %*% weightsCB
  rf_values <- as.numeric(rf_CB$RF)  
  sharpeCB <- mean(port_returnsCB - rf_values) / sd(port_returnsCB)
  weight_historyCB <<- append(weight_historyCB, list(weightsCB))
  return(-sharpeCB)
}

##############################################
## VINCOLI
##############################################
constraint_eq <- function(weights) {
  return(sum(weights) - 1)
}

lower_boundsGB <- rep(0, n_assetGB)  
upper_boundsGB <- rep(1, n_assetGB)  
lower_boundsCB <- rep(0, n_assetCB)  
upper_boundsCB <- rep(1, n_assetCB) 

initial_weightsGB <- rep(1 / n_assetGB, n_assetGB)
initial_weightsCB <- rep(1 / n_assetCB, n_assetCB)

##############################################
## OTTIMIZZAZIONE
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
## RISULTATI
##############################################
# Disattiva notazione scientifica
options(scipen = 999, digits = 10)  # niente notazione scientifica, 10 cifre totali


stampa_pesi_finali <- function(opt_solution, asset_names) {
  pesi_finali <- opt_solution
  df <- data.frame(
    Asset = asset_names,
    Peso = format(round(pesi_finali, 10), nsmall = 10)
  )
  cat("\n📊 Pesi Ottimali Finali:\n")
  print(df, row.names = FALSE)
}

# Uso per GB
stampa_pesi_finali(opt_resultGB$solution, c("XSX6.MI", "GRON.MI", "IPRE.DE"))

# Uso per CB
stampa_pesi_finali(opt_resultCB$solution, c("XSX6.MI", "IEAA.L", "IPRE.DE"))









#########################################################
########DIVERSI DA QUESTI CON WEIGHT_HISTORY#############


#stampa_pesi_finali <- function(weight_history, asset_names) {
  # Prende l’ultima combinazione (ultima lista salvata)
  #pesi_finali <- unlist(weight_history[[length(weight_history)]])
  
  # Crea data frame con nomi asset e pesi
  #df <- data.frame(
    #Asset = asset_names,
    #Peso = format(round(pesi_finali, 10), nsmall = 10)
  )
  
  # Stampa
  #cat("\n Pesi Ottimali Finali:\n")
  #print(df, row.names = FALSE)
}

# Esempio per GB
#stampa_pesi_finali(weight_historyGB, c("XSX6.MI", "GRON.MI", "IPRE.DE"))

# Esempio per CB
#stampa_pesi_finali(weight_historyCB, c("XSX6.MI", "IEAA.L", "IPRE.DE"))

########################################
#stesso gioco ma solo depiction mensile#

#raggruppa i return in base al mese
daily_returnsGB$month <- floor_date(daily_returnsGB$date, "month")
daily_returnsCB$month <- floor_date(daily_returnsCB$date, "month")

#tiene lista dello storico
monthly_resultsGB <- list()
monthly_resultsCB <- list()

for (m in unique(daily_returnsGB$month)) {
  
  monthly_dataGB <- daily_returnsGB %>%
    filter(month == m) %>%
    select(-date, -month)
  
  mydata_mGB <- as.matrix(monthly_dataGB)
  mydata_mGB <- mydata_mGB[-1,]
  mode(mydata_mGB) <- "numeric"
  
  sharpe_ratio_mGB <- function(weights) {
    port_returnsGB <- mydata_mGB %*% weights
    sr <- (mean(port_returnsGB) - rf_GB$RF) / sd(port_returnsGB)
    return(-sr)
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
    Month = as.Date(m),  # 🔹 Manteniamo Date qui
    XSX6.MI = opt_m$solution[1],
    GRON.MI = opt_m$solution[2],
    IPRE.DE  = opt_m$solution[3],
    Sharpe  = -opt_m$objective
  )
}

# Combino in unico dataset wide
monthly_results_dfGB <- bind_rows(monthly_resultsGB)

# 🔹 Ora formatto in "YYYY-MM"
monthly_results_dfGB <- monthly_results_dfGB %>%
  mutate(Month = format(Month, "%Y-%m"))

# 🔹 Arrotondo i pesi e lo Sharpe
monthly_results_dfGB <- monthly_results_dfGB %>%
  mutate(across(c(XSX6.MI, GRON.MI, IPRE.DE, Sharpe),
                ~ round(., 4)))

# Visualizzo tabella finale
View(monthly_results_dfGB)
print(monthly_results_dfGB)

############ CON IL PORTAFOGLIO CB#########
for (m in unique(daily_returnsCB$month)) {
  
  monthly_dataCB <- daily_returnsCB %>%
    filter(month == m) %>%
    select(-date, -month)
  
  mydata_mCB <- as.matrix(monthly_dataCB)
  mydata_mCB <- mydata_mCB[-1,]
  mode(mydata_mCB) <- "numeric"
  
  sharpe_ratio_mCB <- function(weights) {
    port_returnsCB <- mydata_mCB %*% weights
    sr <- (mean(port_returnsCB) - rf_CB$RF) / sd(port_returnsCB)
    return(-sr)
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
    Month = as.Date(m),  # 🔹 Manteniamo Date qui
    XSX6.MI = opt_m$solution[1],
    IEAA.L = opt_m$solution[2],
    IPRE.DE  = opt_m$solution[3],
    Sharpe  = -opt_m$objective
  )
}

# Combino in unico dataset wide
monthly_results_dfCB <- bind_rows(monthly_resultsCB)

# 🔹 Ora formatto in "YYYY-MM"
monthly_results_dfCB <- monthly_results_dfCB %>%
  mutate(Month = format(Month, "%Y-%m"))

# 🔹 Arrotondo i pesi e lo Sharpe
monthly_results_dfCB <- monthly_results_dfCB %>%
  mutate(across(c(XSX6.MI, IEAA.L, IPRE.DE, Sharpe),
                ~ round(., 4)))

# Visualizzo tabella finale
View(monthly_results_dfCB)
print(monthly_results_dfCB)

#per avere solo tabella uguale ma con dev. standard 
monthly_resultsCB <- list()



###########################

########################################
#DA CORREGGERE########################
for (m in unique(returns_wideNA$month)) {
  
  monthly_data <- returns_wideNA %>%
    filter(month == m) %>%
    select(-date, -month)
  
  mydata_m <- as.matrix(monthly_data)
  mode(mydata_m) <- "numeric"
  
  sharpe_ratio_m <- function(weights) {
    port_returns <- mydata_m %*% weights
    sr <- (mean(port_returns) - rf_daily) / sd(port_returns)
    return(-sr)
  }
  
  opt_m <- nloptr(
    x0 = rep(1/ncol(mydata_m), ncol(mydata_m)),
    eval_f = sharpe_ratio_m,
    lb = rep(0, ncol(mydata_m)),
    ub = rep(1, ncol(mydata_m)),
    eval_g_eq = function(w) sum(w) - 1,
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
  )
  
  # Calcolo la deviazione standard del portafoglio ottimizzato
  portfolio_returns_m <- mydata_m %*% opt_m$solution
  portfolio_sd_m <- sd(portfolio_returns_m)
  
  monthly_results[[length(monthly_results) + 1]] <- data.frame(
    Month   = as.Date(m),
    XSX6.MI = opt_m$solution[1],
    GRON.MI = opt_m$solution[2],
    IEAA.L  = opt_m$solution[3],
    Vol     = portfolio_sd_m  # deviazione standard mensile
  )
}

# Combino i risultati
monthly_vol_df <- bind_rows(monthly_results)

# Formatto in YYYY-MM
monthly_vol_df <- monthly_vol_df %>%
  mutate(Month = format(Month, "%Y-%m"))

# Arrotondo
monthly_vol_df <- monthly_vol_df %>%
  mutate(across(c(XSX6.MI, GRON.MI, IEAA.L, Vol),
                ~ round(., 4)))

# Visualizzo tabella
View(monthly_vol_df)
print(monthly_vol_df)

########################################
#stesso gioco ma solo depiction mensile#

library(lubridate)
library(dplyr)

#raggruppa i return in base al mese
returns_wideNA$month <- floor_date(returns_wideNA$date, "month")

#tiene lista dello storico
monthly_results <- list()

for (m in unique(returns_wideNA$month)) {
  
  monthly_data <- returns_wideNA %>%
    filter(month == m) %>%
    select(-date, -month)
  
  mydata_m <- as.matrix(monthly_data)
  mode(mydata_m) <- "numeric"
  
  sharpe_ratio_m <- function(weights) {
    port_returns <- mydata_m %*% weights
    sr <- (mean(port_returns) - rf_daily) / sd(port_returns)
    return(-sr)
  }
  
  opt_m <- nloptr(
    x0 = rep(1/ncol(mydata_m), ncol(mydata_m)),
    eval_f = sharpe_ratio_m,
    lb = rep(0, ncol(mydata_m)),
    ub = rep(1, ncol(mydata_m)),
    eval_g_eq = function(w) sum(w) - 1,
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
  )
  
  monthly_results[[length(monthly_results) + 1]] <- data.frame(
    Month = as.Date(m),  # 🔹 Manteniamo Date qui
    XSX6.MI = opt_m$solution[1],
    GRON.MI = opt_m$solution[2],
    IEAA.L  = opt_m$solution[3],
    Sharpe  = -opt_m$objective
  )
}

# Combino in unico dataset wide
monthly_results_df <- bind_rows(monthly_results)

# 🔹 Ora formatto in "YYYY-MM"
monthly_results_df <- monthly_results_df %>%
  mutate(Month = format(Month, "%Y-%m"))

# 🔹 Arrotondo i pesi e lo Sharpe
monthly_results_df <- monthly_results_df %>%
  mutate(across(c(XSX6.MI, GRON.MI, IEAA.L, Sharpe),
                ~ round(., 4)))

# Visualizzo tabella finale
View(monthly_results_df)
print(monthly_results_df)


#per avere solo tabella uguale ma con dev. standard 
monthly_results <- list()

for (m in unique(returns_wideNA$month)) {
  
  monthly_data <- returns_wideNA %>%
    filter(month == m) %>%
    select(-date, -month)
  
  mydata_m <- as.matrix(monthly_data)
  mode(mydata_m) <- "numeric"
  
  sharpe_ratio_m <- function(weights) {
    port_returns <- mydata_m %*% weights
    sr <- (mean(port_returns) - rf_daily) / sd(port_returns)
    return(-sr)
  }
  
  opt_m <- nloptr(
    x0 = rep(1/ncol(mydata_m), ncol(mydata_m)),
    eval_f = sharpe_ratio_m,
    lb = rep(0, ncol(mydata_m)),
    ub = rep(1, ncol(mydata_m)),
    eval_g_eq = function(w) sum(w) - 1,
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
  )
  # Calcolo la deviazione standard del portafoglio ottimizzato
  portfolio_returns_m <- mydata_m %*% opt_m$solution
  portfolio_sd_m <- sd(portfolio_returns_m)
  
  monthly_results[[length(monthly_results) + 1]] <- data.frame(
    Month   = as.Date(m),
    XSX6.MI = opt_m$solution[1],
    GRON.MI = opt_m$solution[2],
    IEAA.L  = opt_m$solution[3],
    Vol     = portfolio_sd_m  # deviazione standard mensile
  )
}

# Combino i risultati
monthly_vol_df <- bind_rows(monthly_results)

# Formatto in YYYY-MM
monthly_vol_df <- monthly_vol_df %>%
  mutate(Month = format(Month, "%Y-%m"))

# Arrotondo
monthly_vol_df <- monthly_vol_df %>%
  mutate(across(c(XSX6.MI, GRON.MI, IEAA.L, Vol),
                ~ round(., 4)))

# Visualizzo tabella
View(monthly_vol_df)
print(monthly_vol_df)

########################################
#stesso gioco ma solo depiction mensile#

library(lubridate)
library(dplyr)

# Aggiungo colonna con anno-mese
returns_wideNA <- returns_wideNA %>%
  mutate(month = floor_date(date, "month"))

# Calcolo return medio e deviazione standard per ciascun asset
monthly_stats <- returns_wideNA %>%
  group_by(month) %>%
  summarise(
    XSX6_mean = mean(XSX6.MI, na.rm = TRUE),
    XSX6_sd   = sd(XSX6.MI, na.rm = TRUE),
    GRON_mean = mean(GRON.MI, na.rm = TRUE),
    GRON_sd   = sd(GRON.MI, na.rm = TRUE),
    IEAA_mean = mean(IEAA.L, na.rm = TRUE),
    IEAA_sd   = sd(IEAA.L, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(month = format(month, "%Y-%m"))  # Formattiamo mese leggibile
# Visualizzo
View(monthly_stats)
print(monthly_stats)

