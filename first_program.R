# Importing data set

sarah_powers_data_hbap <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/Harvard/LPA/Week 4/sarah_powers_data_hbap.csv")

# Reading head 

head(sarah_powers_data_hbap)

# Creating new variable to idetify sales and non sales people

sarah_powers_data_hbap$sales_non_sales <- ifelse(sarah_powers_data_hbap$Sales_dept == 0, 'Non_Sales', 'Sales')

# Counting and plotting bar chart to identify sales vs non sales people

counts <- table(sarah_powers_data_hbap$sales_non_sales)

barplot(counts, main = "Sales vs Non Sales People", xlab = "Number of People")

library(dplyr)

# To get stats on total email count

group_by(sarah_powers_data_hbap,sales_non_sales) %>% 
  summarise(
    count = n(),
    mean = mean(emailcount, na.rm = TRUE),
    median = median(emailcount, na.r=TRUE)
  )
