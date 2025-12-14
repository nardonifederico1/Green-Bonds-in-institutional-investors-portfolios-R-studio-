###########################################################
## Efficient frontier con CVaR EF2021-2025 e poi mensile ##
###########################################################

# compute returns
#  
testData_return <- diff(prices_syncVAR, arithmetic=FALSE, na.pad=FALSE) - 1

percentile <- 0.95

#  Compare VaR with quantile calculations for assets
#  when portfolio_method = "single" in VaR, the VaR for each column in R is calculated 
# questa è la VaR giornaliera per ogni asset
#
CVaR_asset_hist <- ES(R = testData_return, p=percentile, method="historical",
                      portfolio_method = "single")
print(CVaR_asset_hist)

# stessa cosa ma in percentuale

CVaR_asset_hist_pct <- 100 * CVaR_asset_hist
print(apply(round(CVaR_asset_hist_pct, 2), 2, function(x) paste0(x, "%")))

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
## FUNZIONI VaR 
##############################################

CVaR_GB <- function(Q, alpha = 0.95) {
  weightsGBCVar <- as.numeric(Q)
  port_returnsGBCVar <- rowSums(sweep(mydataGB, 2, weightsGBCVar, `*`))
  rf_values <- as.numeric(rf_sync$RF)
  excess <- port_returnsGBCVar - rf_values
  losses <- -excess
  CVaR <- -as.numeric(ES(losses, p = alpha, method = "historical"))
  weight_historyGB <<- append(weight_historyGB, list(weightsGBCVar))
  return(CVaR)
}

CVaR_CB <- function(Q, alpha = 0.95) {
  weightsCBCVar <- as.numeric(Q)
  port_returnsCBCVar <- rowSums(sweep(mydataCB, 2, weightsCBCVar, `*`))
  rf_values <- as.numeric(rf_sync$RF)
  excess <- port_returnsCBCVar - rf_values
  losses <- -excess
  CVaR <- -as.numeric(ES(losses, p = alpha, method = "historical"))
  weight_historyCB <<- append(weight_historyCB, list(weightsCBCVar))
  return(CVaR)
}

##############################################
## VINCOLI 
##############################################
constraint_eqCVar <- function(Q) {
  return(sum(Q) - 1)
}

lower_boundsGB <- rep(0, n_assetGB)  
upper_boundsGB <- rep(1, n_assetGB)  
lower_boundsCB <- rep(0, n_assetCB)  
upper_boundsCB <- rep(1, n_assetCB) 

initial_weightsGB <- rep(1 / n_assetGB, n_assetGB)
initial_weightsCB <- rep(1 / n_assetCB, n_assetCB)
##############################################
## OTTIMIZZAZIONE (Minimizzazione VaR)
##############################################
opt_resultCVaR_GB <- nloptr(
  x0 = initial_weightsGB, 
  eval_f = function(Q) CVaR_GB(Q, alpha = 0.95), 
  lb = lower_boundsGB, 
  ub = upper_boundsGB,
  eval_g_eq = constraint_eqCVar,
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

opt_resultCVaR_CB <- nloptr(
  x0 = initial_weightsCB, 
  eval_f = function(Q) CVaR_CB(Q, alpha = 0.95), 
  lb = lower_boundsCB, 
  ub = upper_boundsCB,
  eval_g_eq = constraint_eqCVar,
  opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
)

##############################################
## RISULTATI
##############################################
# Disattiva notazione scientifica
options(scipen = 999, digits = 10)  # niente notazione scientifica, 10 cifre totali


W_GB_CVaR      <- opt_resultCVaR_GB$solution      # pesi ottimali (vettore)
CVaR_min_GB <- opt_resultCVaR_GB$objective       # CVaR minimizzato portafoglio GB

W_CB_CVaR      <- opt_resultCVaR_CB$solution      # pesi ottimali (vettore)
CVaR_min_CB <- opt_resultCVaR_CB$objective       # CVaR minimizzato portafoglio CB

cat("Weights ottimali GB portfolio:" , W_GB_CVaR, "\n")
cat("CVaR GB minimo alfa 0.95:", CVaR_min_GB, "\n")


cat("Weights ottimali CB portfolio:" , W_CB_CVaR, "\n")
cat("CVaR CB minimo alfa 0.95:", CVaR_min_CB, "\n")

# OPPURE

# Definizione ticker
ETF_CVaR_GB <- c("GRON.MI","IPRE.DE","XSX6.MI")
ETF_CVaR_CB <- c("IEAA.L", "IPRE.DE","XSX6.MI")


# Stampa GB
cat("GB Portfolio CVaR 2021-25:\n")
for (i in seq_along(ETF_CVaR_GB)) {
  cat(ETF_CVaR_GB[i], ":", round(W_GB_CVaR[i], 4), "\n")
}
cat("CVaR min GB 2021-25:", CVaR_min_GB, "\n\n")


cat("CB Portfolio CVaR 2021-25:\n")
for (i in seq_along(ETF_CVaR_CB)) {
  cat(ETF_CVaR_CB[i], ":", round(W_CB_CVaR[i], 4), "\n")
}
cat("CVaR min CB 2021-25:", CVaR_min_CB, "\n\n")

#########################################################################
##              CVaR MENSILE IN ENTRAMBI PORTAFOGLI                     ##
#########################################################################

### CVaR MIN Portfolio MENSILE 2021-2025 (weight allocation)
library(dplyr)
library(lubridate)
library(nloptr)
library(PerformanceAnalytics)  # per ES (CVaR)

confidence <- 0.95

# 1) mese reale per i due dataset
daily_returnsGB <- daily_returnsGB %>%
  mutate(month_start = floor_date(date, "month"))
daily_returnsCB <- daily_returnsCB %>%
  mutate(month_start = floor_date(date, "month"))

# RF indicizzato per mese reale (uso la colonna già presente in rf_sync)
rf_sync <- rf_sync %>% mutate(month_start = month)


monthly_results_CVaRGB <- list()
monthly_results_CVaRCB <- list()

# 2) Itera sui mesi reali
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
  
  # 3) RF medio del mese (scalare replicato per il numero di giorni del mese)
  rf_m <- rf_sync %>%
    dplyr::filter(.data$month_start == m_date) %>%   # Date vs Date → OK
    dplyr::summarise(val = mean(RF, na.rm = TRUE)) %>%
    dplyr::pull(val) %>%
    as.numeric()
  
  if (!is.finite(rf_m)) next
  
  
  rf_vecGB <- rep(rf_m, nrow(window_GB))
  rf_vecCB <- rep(rf_m, nrow(window_CB))
  
  # ---- Obiettivi: MINIMIZZA CVaR (ES storico) ----
  CVaR_min_mGB <- function(W) {
    pr <- rowSums(sweep(X_GB, 2, as.numeric(W), `*`))
    losses <- -(pr - rf_vecGB)                 # coda a sinistra → perdite positive
    -as.numeric(ES(losses, p = confidence, method = "historical"))
  }
  CVaR_min_mCB <- function(W) {
    pr <- rowSums(sweep(X_CB, 2, as.numeric(W), `*`))
    losses <- -(pr - rf_vecCB)
    -as.numeric(ES(losses, p = confidence, method = "historical"))
  }
  # ------------------------------------------------
  
  opt_CVaR_mGB <- nloptr::nloptr(
    x0        = rep(1/ncol(X_GB), ncol(X_GB)),
    eval_f    = CVaR_min_mGB,
    lb        = rep(0, ncol(X_GB)),
    ub        = rep(1, ncol(X_GB)),
    eval_g_eq = function(w) sum(w) - 1,
    opts      = list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-6)
  )
  opt_CVaR_mCB <- nloptr::nloptr(
    x0        = rep(1/ncol(X_CB), ncol(X_CB)),
    eval_f    = CVaR_min_mCB,
    lb        = rep(0, ncol(X_CB)),
    ub        = rep(1, ncol(X_CB)),
    eval_g_eq = function(w) sum(w) - 1,
    opts      = list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-6)
  )
  
  monthly_results_CVaRGB <- append(monthly_results_CVaRGB, list(data.frame(
    Month   = m_date,  # vera Date
    XSX6.MI = opt_CVaR_mGB$solution[ match("XSX6.MI", colnames(X_GB)) ],
    GRON.MI = opt_CVaR_mGB$solution[ match("GRON.MI", colnames(X_GB)) ],
    IPRE.DE = opt_CVaR_mGB$solution[ match("IPRE.DE", colnames(X_GB)) ],
    CVaRm   = opt_CVaR_mGB$objective
  )))
  
  monthly_results_CVaRCB <- append(monthly_results_CVaRCB, list(data.frame(
    Month   = m_date,  # vera Date
    XSX6.MI = opt_CVaR_mCB$solution[ match("XSX6.MI", colnames(X_CB)) ],
    IEAA.L  = opt_CVaR_mCB$solution[ match("IEAA.L",  colnames(X_CB)) ],
    IPRE.DE = opt_CVaR_mCB$solution[ match("IPRE.DE", colnames(X_CB)) ],
    CVaRm   = opt_CVaR_mCB$objective
  )))
}


monthly_results_CVaRGB <- dplyr::bind_rows(monthly_results_CVaRGB) %>%
  dplyr::mutate(
    Month = as.Date(Month, origin = "1970-01-01"),
    Month = format(Month, "%Y-%m")
  )

monthly_results_CVaRCB <- dplyr::bind_rows(monthly_results_CVaRCB) %>%
  dplyr::mutate(
    Month = as.Date(Month, origin = "1970-01-01"),
    Month = format(Month, "%Y-%m")
  )


# poi arrotondi i numerici
monthly_results_CVaRGB <- monthly_results_CVaRGB %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4)))
monthly_results_CVaRCB <- monthly_results_CVaRCB %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4)))


View(monthly_results_CVaRGB)
View(monthly_results_CVaRCB)




################################################
# ---- GB: Vol e MeanRet con pesi ottimali mese per mese ----
monthly_results_CVaRGB$Vol <- sapply(monthly_results_CVaRGB$Month, function(m_chr) {
  m_start <- as.Date(paste0(m_chr, "-01"))  # Month è "YYYY-MM" dopo il format
  X <- daily_returnsGB %>%
    dplyr::filter(lubridate::floor_date(date, "month") == m_start) %>%
    dplyr::arrange(date) %>%
    dplyr::select(XSX6.MI, GRON.MI, IPRE.DE) %>%
    as.matrix()
  if (nrow(X) == 0) return(NA_real_)
  w <- as.numeric(
    monthly_results_CVaRGB[monthly_results_CVaRGB$Month == m_chr,
                           c("XSX6.MI","GRON.MI","IPRE.DE")]
  )
  if (length(w) != ncol(X) || anyNA(w)) return(NA_real_)
  port <- as.numeric(X %*% w)
  stats::sd(port, na.rm = TRUE)
})

monthly_results_CVaRGB$MeanRet <- sapply(monthly_results_CVaRGB$Month, function(m_chr) {
  m_start <- as.Date(paste0(m_chr, "-01"))
  X <- daily_returnsGB %>%
    dplyr::filter(lubridate::floor_date(date, "month") == m_start) %>%
    dplyr::arrange(date) %>%
    dplyr::select(XSX6.MI, GRON.MI, IPRE.DE) %>%
    as.matrix()
  if (nrow(X) == 0) return(NA_real_)
  w <- as.numeric(
    monthly_results_CVaRGB[monthly_results_CVaRGB$Month == m_chr,
                           c("XSX6.MI","GRON.MI","IPRE.DE")]
  )
  if (length(w) != ncol(X) || anyNA(w)) return(NA_real_)
  port <- as.numeric(X %*% w)
  mean(port, na.rm = TRUE)
})

# ---- CB: Vol e MeanRet ----
monthly_results_CVaRCB$Vol <- sapply(monthly_results_CVaRCB$Month, function(m_chr) {
  m_start <- as.Date(paste0(m_chr, "-01"))
  X <- daily_returnsCB %>%
    dplyr::filter(lubridate::floor_date(date, "month") == m_start) %>%
    dplyr::arrange(date) %>%
    dplyr::select(XSX6.MI, IEAA.L, IPRE.DE) %>%
    as.matrix()
  if (nrow(X) == 0) return(NA_real_)
  w <- as.numeric(
    monthly_results_CVaRCB[monthly_results_CVaRCB$Month == m_chr,
                           c("XSX6.MI","IEAA.L","IPRE.DE")]
  )
  if (length(w) != ncol(X) || anyNA(w)) return(NA_real_)
  port <- as.numeric(X %*% w)
  stats::sd(port, na.rm = TRUE)
})

monthly_results_CVaRCB$MeanRet <- sapply(monthly_results_CVaRCB$Month, function(m_chr) {
  m_start <- as.Date(paste0(m_chr, "-01"))
  X <- daily_returnsCB %>%
    dplyr::filter(lubridate::floor_date(date, "month") == m_start) %>%
    dplyr::arrange(date) %>%
    dplyr::select(XSX6.MI, IEAA.L, IPRE.DE) %>%
    as.matrix()
  if (nrow(X) == 0) return(NA_real_)
  w <- as.numeric(
    monthly_results_CVaRCB[monthly_results_CVaRCB$Month == m_chr,
                           c("XSX6.MI","IEAA.L","IPRE.DE")]
  )
  if (length(w) != ncol(X) || anyNA(w)) return(NA_real_)
  port <- as.numeric(X %*% w)
  mean(port, na.rm = TRUE)
})

# arrotonda
monthly_results_CVaRGB <- monthly_results_CVaRGB %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4)))
monthly_results_CVaRCB <- monthly_results_CVaRCB %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4)))




################################################################
######### EFFICIENT FRONTIER TUTTO IL PERIODO CVaR #############
################################################################

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

