#######################################################################################
#### CALCOLO RENDIMENTI CON PORTAFOGLIO CON PESI OTTIMALI in caso VaR e SHARPE ########
#######################################################################################
########## è sbagliato perchè poi i rendiemnti non coincidon ocon quelli calcolati in precedenza 
## per l osharpe ratio, qualcosa è toppato 

#########
### GB ##
#########

library(dplyr)
library(lubridate)

# df_daily: tibble con colonne: date + asset...
# weights_tbl: tibble con colonne: Month (YYYY-MM) + asset...
# assets: vettore di nomi colonna (in df_daily e in weights_tbl)
# out_name: nome colonna del risultato (es. "VaR" o "MaxSharpe")

compute_monthly_profit <- function(df_daily, weights_tbl, assets, out_name = "Ret") {
  daily <- df_daily %>%
    mutate(Month = format(floor_date(date, "month"), "%Y-%m")) %>%
    select(Month, all_of(assets))
  
  w_tbl <- weights_tbl %>%
    mutate(Month = as.character(Month)) %>%     # già "YYYY-MM" nelle tue tabelle
    select(Month, all_of(assets))
  
  # join per mese (tiene solo mesi per cui hai pesi)
  joined <- inner_join(daily, w_tbl, by = "Month", suffix = c(".r", ".w"))
  
  # calcolo del portafoglio giornaliero per riga: r_p = X * w
  # costruisco le matrici rispettando gli stessi asset nell'ordine di 'assets'
  X_mat <- as.matrix(joined %>% select(paste0(assets, ".r")))
  W_mat <- as.matrix(joined %>% select(paste0(assets, ".w")))
  
  # se i pesi sono uguali per tutte le righe del mese, W_mat avrà lo stesso vettore ripetuto;
  # calcolo il prodotto riga per riga
  port_daily <- rowSums(X_mat * W_mat)
  
  # attacco e poi aggrego a livello mensile con compounding
  out <- joined %>%
    mutate(port_daily = port_daily) %>%
    group_by(Month) %>%
    summarise("{out_name}" := prod(1 + port_daily) - 1, .groups = "drop")
  
  out
}

# dati giornalieri e set di asset per GB
assets_GB <- c("GRON.MI", "IPRE.DE", "XSX6.MI")

# Tabella VaR (pesi per mese) – la tua: monthly_results_VaRGB (Month, GRON.MI, IPRE.DE, XSX6.MI, VaRm)
gb_var_monthly <- compute_monthly_profit(
  df_daily   = daily_returnsGB %>% select(date, all_of(assets_GB)),
  weights_tbl= monthly_results_VaRGB,                # già con Month = "YYYY-MM"
  assets     = assets_GB,
  out_name   = "Ret_VaR"
)

# Tabella Max-Sharpe (pesi per mese) – la tua: monthly_results_dfGB (Month, XSX6.MI, GRON.MI, IPRE.DE, Sharpe)
# Riordino le colonne per farle combaciare con 'assets_GB'
weights_sharpe_GB <- monthly_results_dfGB %>%
  select(Month, all_of(assets_GB))

gb_sharpe_monthly <- compute_monthly_profit(
  df_daily   = daily_returnsGB %>% select(date, all_of(assets_GB)),
  weights_tbl= weights_sharpe_GB,
  assets     = assets_GB,
  out_name   = "Ret_MaxSharpe"
)

# Confronto unico GB
gb_compare <- full_join(gb_var_monthly, gb_sharpe_monthly, by = "Month") %>%
  arrange(Month)

print(gb_compare)
View(gb_compare)


#########
### CB ##
#########


assets_CB <- c("IEAA.L", "IPRE.DE", "XSX6.MI")

# VaR (la tua: monthly_results_VaRCB con Month, IEAA.L, IPRE.DE, XSX6.MI, VaRm)
cb_var_monthly <- compute_monthly_profit(
  df_daily   = daily_returnsCB %>% select(date, all_of(assets_CB)),
  weights_tbl= monthly_results_VaRCB,
  assets     = assets_CB,
  out_name   = "Ret_VaR"
)

# Max-Sharpe (la tua: monthly_results_dfCB con Month, XSX6.MI, IEAA.L, IPRE.DE, Sharpe)
weights_sharpe_CB <- monthly_results_dfCB %>%
  select(Month, all_of(assets_CB))

cb_sharpe_monthly <- compute_monthly_profit(
  df_daily   = daily_returnsCB %>% select(date, all_of(assets_CB)),
  weights_tbl= weights_sharpe_CB,
  assets     = assets_CB,
  out_name   = "Ret_MaxSharpe"
)

# Confronto unico CB però trai suo istessi rendimenti Var e SHARPE
cb_compare <- full_join(cb_var_monthly, cb_sharpe_monthly, by = "Month") %>%
  arrange(Month)

print(cb_compare)
View(cb_compare)




#GRAFICI DI CONFRONTO
library(PerformanceAnalytics)
library(xts)

# Convertiamo le tabelle in xts
gb_xts <- xts(gb_compare[, -1], order.by = as.Date(paste0(gb_compare$Month, "-01")))
cb_xts <- xts(cb_compare[, -1], order.by = as.Date(paste0(cb_compare$Month, "-01")))

# Rinomina colonne per chiarezza nel grafico
colnames(gb_xts) <- c("GB VaR", "GB MaxSharpe")
colnames(cb_xts) <- c("CB VaR", "CB MaxSharpe")

# Combina GB e CB
ret_xts <- merge(gb_xts, cb_xts)

# Grafico performance cumulata
charts.PerformanceSummary(ret_xts,
                          main = "GB vs CB — Monthly Returns",
                          wealth.index = TRUE,
                          legend.loc = "topright",
                          colorset = c("black", "red", "darkgreen", "blue"))




#Step 1: crea colonna 'month' in formato YYYY-MM per i daily returns
daily_returnsGB <- daily_returnsGB %>%
  mutate(month = format(floor_date(date, "month"), "%Y-%m"))

# Step 2: crea colonna 'month' anche nei pesi
weights_GB <- monthly_results_dfGB %>%
  mutate(month = Month) %>%
  select(month, XSX6.MI, GRON.MI, IPRE.DE)

# Step 3: crea il dataset esteso replicando i pesi su tutti i giorni
weights_GB_extended <- daily_returnsGB %>%
  select(date, month) %>%
  left_join(weights_GB, by = "month")

# Step 1: unisci i daily returns con i pesi giornalieri
gb_combined <- daily_returnsGB %>%
  select(date, all_of(c("XSX6.MI", "GRON.MI", "IPRE.DE"))) %>%
  left_join(weights_GB_extended, by = "date")

gb_combined <- gb_combined %>%
  rename(
    `return XSX6.MI` = `XSX6.MI.x`,
    `return GRON.MI` = `GRON.MI.x`,
    `return IPRE.DE` = `IPRE.DE.x`,
    `weight XSX6.MI` = `XSX6.MI.y`,
    `weight GRON.MI` = `GRON.MI.y`,
    `weight IPRE.DE` = `IPRE.DE.y`
  )


# Step 2: calcola il rendimento del portafoglio giornaliero
gb_combined <- gb_combined %>%
  mutate(
    portf_return_GB = `return XSX6.MI` * `weight XSX6.MI` +
      `return GRON.MI` * `weight GRON.MI` +
      `return IPRE.DE` * `weight IPRE.DE`
  )



############################################
# Step 1: crea colonna 'month' nei daily returns CB
daily_returnsCB <- daily_returnsCB %>%
  mutate(month = format(floor_date(date, "month"), "%Y-%m"))

# Step 2: crea colonna 'month' anche nei pesi CB
weights_CB <- monthly_results_dfCB %>%
  mutate(month = Month) %>%
  select(month, XSX6.MI, IEAA.L, IPRE.DE)

# Step 3: estendi i pesi su tutti i giorni (CB)
weights_CB_extended <- daily_returnsCB %>%
  select(date, month) %>%
  left_join(weights_CB, by = "month")

# Step 1: unisci i daily returns CB con i pesi giornalieri CB
cb_combined <- daily_returnsCB %>%
  select(date, all_of(c("XSX6.MI", "IEAA.L", "IPRE.DE"))) %>%
  left_join(weights_CB_extended, by = "date")

# Step 2: rinomina (rendimenti = return …, pesi = weight …)
cb_combined <- cb_combined %>%
  rename(
    `return XSX6.MI` = `XSX6.MI.x`,
    `return IEAA.L`  = `IEAA.L.x`,
    `return IPRE.DE` = `IPRE.DE.x`,
    `weight XSX6.MI` = `XSX6.MI.y`,
    `weight IEAA.L`  = `IEAA.L.y`,
    `weight IPRE.DE` = `IPRE.DE.y`
  )

# Step 3: calcola il rendimento giornaliero del portafoglio CB
cb_combined <- cb_combined %>%
  mutate(
    portf_return_CB = `return XSX6.MI` * `weight XSX6.MI` +
      `return IEAA.L`  * `weight IEAA.L`  +
      `return IPRE.DE` * `weight IPRE.DE`
  )

library(xts)
library(PerformanceAnalytics)

gb_ret_xts <- xts(gb_combined$portf_return_GB, order.by = gb_combined$date)
cb_ret_xts <- xts(cb_combined$portf_return_CB, order.by = cb_combined$date)
colnames(gb_ret_xts) <- "GB daily returns-monthly Sharpe strategy"
colnames(cb_ret_xts) <- "CB daily returns-monthly Sharpe strategy"

ret_all <- merge(gb_ret_xts, cb_ret_xts)
ret_all <- na.omit(ret_all)  # nel caso ci siano date non allineate

charts.PerformanceSummary(ret_all,
                          main = "GB vs CB — Daily (Sharpe weights)",
                          wealth.index = TRUE,
                          legend.loc = "topright"
)

