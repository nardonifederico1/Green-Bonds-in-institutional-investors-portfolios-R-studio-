#######################################################################################
#### COMPUTE RETURNS FOR EVERY SCENARIO OF MIN or MAX ########
#######################################################################################
library(dplyr)
library(lubridate)
compute_monthly_profit <- function(df_daily, weights_tbl, assets, out_name = "Ret") {
  daily <- df_daily %>%
    mutate(Month = format(floor_date(date, "month"), "%Y-%m")) %>%
    select(Month, all_of(assets))
  
  w_tbl <- weights_tbl %>%
    mutate(Month = as.character(Month)) %>%     
    select(Month, all_of(assets))
  joined <- inner_join(daily, w_tbl, by = "Month", suffix = c(".r", ".w"))
  X_mat <- as.matrix(joined %>% select(paste0(assets, ".r")))
  W_mat <- as.matrix(joined %>% select(paste0(assets, ".w")))
  port_daily <- rowSums(X_mat * W_mat)
  out <- joined %>%
    mutate(port_daily = port_daily) %>%
    group_by(Month) %>%
    summarise("{out_name}" := prod(1 + port_daily) - 1, .groups = "drop")
  
  out
}
assets_GB <- c("GRON.MI", "IPRE.DE", "XSX6.MI")
gb_var_monthly <- compute_monthly_profit(
  df_daily   = daily_returnsGB %>% select(date, all_of(assets_GB)),
  weights_tbl= monthly_results_VaRGB,               
  assets     = assets_GB,
  out_name   = "Ret_VaR"
)
weights_sharpe_GB <- monthly_results_dfGB %>%
  select(Month, all_of(assets_GB))

gb_sharpe_monthly <- compute_monthly_profit(
  df_daily   = daily_returnsGB %>% select(date, all_of(assets_GB)),
  weights_tbl= weights_sharpe_GB,
  assets     = assets_GB,
  out_name   = "Ret_MaxSharpe"
)

gb_compare <- full_join(gb_var_monthly, gb_sharpe_monthly, by = "Month") %>%
  arrange(Month)

print(gb_compare)
View(gb_compare)


#########
### CB ##
#########


assets_CB <- c("IEAA.L", "IPRE.DE", "XSX6.MI")
cb_var_monthly <- compute_monthly_profit(
  df_daily   = daily_returnsCB %>% select(date, all_of(assets_CB)),
  weights_tbl= monthly_results_VaRCB,
  assets     = assets_CB,
  out_name   = "Ret_VaR"
)
weights_sharpe_CB <- monthly_results_dfCB %>%
  select(Month, all_of(assets_CB))

cb_sharpe_monthly <- compute_monthly_profit(
  df_daily   = daily_returnsCB %>% select(date, all_of(assets_CB)),
  weights_tbl= weights_sharpe_CB,
  assets     = assets_CB,
  out_name   = "Ret_MaxSharpe"
)

cb_compare <- full_join(cb_var_monthly, cb_sharpe_monthly, by = "Month") %>%
  arrange(Month)

print(cb_compare)
View(cb_compare)


#CONFRONTATION GRAPHS
library(PerformanceAnalytics)
library(xts)

# Converting into in xts
gb_xts <- xts(gb_compare[, -1], order.by = as.Date(paste0(gb_compare$Month, "-01")))
cb_xts <- xts(cb_compare[, -1], order.by = as.Date(paste0(cb_compare$Month, "-01")))

# Rename columns
colnames(gb_xts) <- c("GB VaR", "GB MaxSharpe")
colnames(cb_xts) <- c("CB VaR", "CB MaxSharpe")

# Combine GB and CB
ret_xts <- merge(gb_xts, cb_xts)

# Graph of cumulative performance
charts.PerformanceSummary(ret_xts,
                          main = "GB vs CB — Monthly Returns",
                          wealth.index = TRUE,
                          legend.loc = "topright",
                          colorset = c("black", "red", "darkgreen", "blue"))

daily_returnsGB <- daily_returnsGB %>%
  mutate(month = format(floor_date(date, "month"), "%Y-%m"))
weights_GB <- monthly_results_dfGB %>%
  mutate(month = Month) %>%
  select(month, XSX6.MI, GRON.MI, IPRE.DE)
weights_GB_extended <- daily_returnsGB %>%
  select(date, month) %>%
  left_join(weights_GB, by = "month")
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

gb_combined <- gb_combined %>%
  mutate(
    portf_return_GB = `return XSX6.MI` * `weight XSX6.MI` +
      `return GRON.MI` * `weight GRON.MI` +
      `return IPRE.DE` * `weight IPRE.DE`
  )



############################################
# FOR CB
daily_returnsCB <- daily_returnsCB %>%
  mutate(month = format(floor_date(date, "month"), "%Y-%m"))

# Step 2: create column "Month" 
weights_CB <- monthly_results_dfCB %>%
  mutate(month = Month) %>%
  select(month, XSX6.MI, IEAA.L, IPRE.DE)

# Step 3: include all CB dates
weights_CB_extended <- daily_returnsCB %>%
  select(date, month) %>%
  left_join(weights_CB, by = "month")

# Step 4: merge weights with returns 
cb_combined <- daily_returnsCB %>%
  select(date, all_of(c("XSX6.MI", "IEAA.L", "IPRE.DE"))) %>%
  left_join(weights_CB_extended, by = "date")

# Step 5: rename
cb_combined <- cb_combined %>%
  rename(
    `return XSX6.MI` = `XSX6.MI.x`,
    `return IEAA.L`  = `IEAA.L.x`,
    `return IPRE.DE` = `IPRE.DE.x`,
    `weight XSX6.MI` = `XSX6.MI.y`,
    `weight IEAA.L`  = `IEAA.L.y`,
    `weight IPRE.DE` = `IPRE.DE.y`
  )

# Step 6: compute returns CB
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
ret_all <- na.omit(ret_all)  
charts.PerformanceSummary(ret_all,
                          main = "GB vs CB — Daily (Sharpe weights)",
                          wealth.index = TRUE,
                          legend.loc = "topright"
)

