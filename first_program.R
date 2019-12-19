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
    total_email_count_mean = mean(emailcount, na.rm = TRUE),
    total_email_count_median = median(emailcount, na.r=TRUE)
  )

# To get stats on total meeting count

group_by(sarah_powers_data_hbap,sales_non_sales) %>% 
  summarise(
    count = n(),
    total_meeting_count_mean = mean(meetingcount, na.rm = TRUE),
    total_meeting_count_median = median(meetingcount, na.r=TRUE)
  )

# on meeting quotas

# To get stats on total meeting count

group_by(sarah_powers_data_hbap,sales_non_sales) %>% 
  summarise(
    count = n(),
    total_quota_count_mean = mean(attainquota, na.rm = TRUE),
    total_quota_count_median = median(attainquota, na.r=TRUE)
  )


# getting only sales people

only_sales <- subset(sarah_powers_data_hbap, sarah_powers_data_hbap$sales_non_sales=="Sales")

# remove three columns 

only_sales <- subset(only_sales, select = -c(sales_non_sales,Sales_dept,pid))

# Remove all the columns pertaining to department level information

only_sales <- subset(only_sales, select = -c(GA_dept,HR_dept,Mkting_dept,Ops_dept,PM_dept,RD_dept))

# to plot means

library(gplots)

# Voila... seems like a lot of binary looking like variables are not binary at all

summary(only_sales)

# Find type of all variale

sapply(only_sales,class)

# Changing all level variables to factor

names <- c('Exc_level','JrIc_level','Mgr_level','SrExec_level','SrIc_level','Sprt_level','Dir_level')

only_sales[,names] <- lapply(only_sales[,names],factor)

# changing all region variables to factor

names <- c('C_region','E_region','N_region','S_region','SW_region','W_region')

only_sales[,names] <- lapply(only_sales[,names], factor)

# Removing few more columns as those are redundant

only_sales <- subset(only_sales, select = -c(level,level_num,func_dept,region))


# to identify relationship between two continous variable i.e. attainquota and email count

scatter.smooth(x=only_sales$attainquota, y = only_sales$emailcount)

# similar for meeting count

scatter.smooth(x=only_sales$attainquota, y = only_sales$meetingcount)

# checking outliers for both total email count and total meeting count

par(mfrow=c(1,2))

boxplot(only_sales$emailcount, main="Total Email Count", boxplot.stats(only_sales$emailcount)$out)

boxplot(only_sales$meetingcount, main = "Total Meeting Count", boxplot.stats(only_sales$meetingcount)$out)

boxplot(only_sales$centrality, main = "Centrality" , boxplot.stats(only_sales$centrality)$out)


# drawing normal density plots to see the 

library(e1071)

par(mfrow=c(1,2))

plot(density(only_sales$emailcount), main = "Density of Email Count", ylab = "Frequency", sub=paste("Skewness:", round(e1071::skewness(only_sales$emailcount),2)))

plot(density(only_sales$meetingcount), main = "Density of Meeting count", ylab = "Frequency", sub=paste("Skewness:",round(e1071::skewness(only_sales$meetingcount),2)))









