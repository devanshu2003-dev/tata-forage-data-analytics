setwd("D:/project_csv/quantium")

library(data.table)
library(ggplot2)
library(lubridate)

data <- fread("QVI_data.csv")

data[, DATE := as.Date(DATE)]
data[, YEARMONTH := year(DATE)*100 + month(DATE)]

measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),
  nCustomers = uniqueN(LYLTY_CARD_NBR),
  nTxnPerCust = .N / uniqueN(LYLTY_CARD_NBR)
), by = .(STORE_NBR, YEARMONTH)]

analyze_store <- function(trial_store, control_store) {
  
  trial_data <- measureOverTime[STORE_NBR == trial_store]
  control_data <- measureOverTime[STORE_NBR == control_store]
  
  comparison <- merge(trial_data, control_data, by="YEARMONTH",
                      suffixes=c("_trial","_control"))
  
  comparison[, period := ifelse(YEARMONTH < 201902, "Pre", "Trial")]
  
  print(paste("Store", trial_store, "vs Control", control_store))
  
  print(comparison[, .(
    trial_avg = mean(totSales_trial),
    control_avg = mean(totSales_control)
  ), by = period])
  
  plot <- ggplot(comparison, aes(x=YEARMONTH)) +
    geom_line(aes(y=totSales_trial, color="Trial")) +
    geom_line(aes(y=totSales_control, color="Control")) +
    labs(title=paste("Trial vs Control Store", trial_store),
         x="Month", y="Total Sales")
  
  print(plot)
  
  ggsave(paste0("store_", trial_store, ".png"), plot=plot, width=8, height=5)
}

analyze_store(77, 233)
analyze_store(86, 155)
analyze_store(88, 237)