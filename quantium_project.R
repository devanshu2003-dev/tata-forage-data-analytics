setwd("D:/project_csv/quantium")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("stringr")
install.packages("readxl")
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(readxl)
transactions <- read_excel("QVI_transaction_data.xlsx")
customers <- read_csv("QVI_purchase_behaviour.csv")
head(transactions)
str(transactions)
data <- transactions %>%
  left_join(customers, by = "LYLTY_CARD_NBR")
head(data)
str(data)
colSums(is.na(data))
data <- data %>% filter(!is.na(PROD_NAME))
data$PACK_SIZE <- as.numeric(gsub("[^0-9]" , "" , data$PROD_NAME))
data$BRAND <- word(data$PROD_NAME, 1)
head(data[,c("PROD_NAME" , "PACK_SIZE" , "BRAND")])

sales_segement <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(TOTAL_SALES = sum(TOT_SALES), .groups = "drop")
transactions_segment <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(TOTAL_TRANSACTIONS = n(), .groups = "drop")


avg_spend <- data %>%
  group_by(LIFESTAGE, PREMIUM_CUSTOMER) %>%
  summarise(AVG_SPEND = mean(TOT_SALES), .groups = "drop")

sales_segement
transactions_segment
avg_spend


sales_plot = ggplot(sales_segement, aes(x=LIFESTAGE, y=TOTAL_SALES, fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(
    title = "Total Sales by Customer Segment",
    x = "Customer Life Stage",
    y = "Total Sales"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))



transaction_plot = ggplot(transactions_segment, aes(x=LIFESTAGE, y=TOTAL_TRANSACTIONS, fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(
    title = "Transactions by Customer Segment",
    x = "Customer Life Stage",
    y = "Number of Transactions"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

avg_plot = ggplot(avg_spend, aes(x=LIFESTAGE, y=AVG_SPEND, fill=PREMIUM_CUSTOMER)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  labs(
    title = "Average Spend per Transaction",
    x = "Customer Life Stage",
    y = "Average Spend"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

data_plot = ggplot(data, aes(x=PACK_SIZE)) +
  geom_histogram(binwidth=10) +
  theme_minimal() +
  labs(
    title = "Pack Size Distribution",
    x = "Pack Size (grams)",
    y = "Frequency"
  )
ggsave("sales_chart.png", plot = sales_plot)
ggsave("transaction_chart.png", plot = transaction_plot)
ggsave("avg_chart.png", plot = avg_plot)
ggsave("data_chart.png", plot = data_plot)
