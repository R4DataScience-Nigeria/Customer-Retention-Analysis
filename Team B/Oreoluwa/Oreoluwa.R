library(here)
library(readr)
library(naniar)
install.packages("naniar")
library(naniar)
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
df <- read_rds(here("Team A", "clean_churn.rds"))
view(df)
head(df)
summary(df)
#Check dimension of data

dim(df)
#checking data types of variables

glimpse(df)

str(df)
#Checking for unique values in acct_id

df %>%
  
  count(acct_id) %>%
  
  slice_head(n = 10) #To check first 10 observations (rows)

#Checking credit_val
df |>
  count(credit_val,x21)

#Dropping credit_val and x21 columns

df_2 <- df %>%
  select(-credit_val, -x21) 
  View(df_2)
  
  # Univariate Analysis
  
  # Distribution of 'years' variable
  ggplot(df_2, aes(x = years)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    ggtitle("Distribution of Years with the Bank") +
    xlab("Years") +
    ylab("Count")
  
  # Barchart of balance
  ggplot(df_2, aes(y = bal)) + 
    geom_bar(fill="blue") +
    ggtitle("Distribution of Account Balance")

  
  # Proportion of customers using digital banking services
  
  prop.table(table(df$mobile_app))
  prop.table(table(df$internet_banking))
  prop.table(table(df$ussd_banking))
   
  
  #Multivariate Analysis
  
  
  # Impact of transaction volumes and values on churn
  transaction_churn <- df_2 %>%
    select(churn, credit_vol, debit_vol, debit_val) %>%
    pivot_longer(cols = -churn, names_to = "TransactionType", values_to = "Value") %>%
    group_by(TransactionType) %>%
    summarise(ChurnRate = mean(churn == "churned", na.rm = TRUE))
  
  ggplot(transaction_churn, aes(x = TransactionType, y = ChurnRate)) +
    geom_bar(stat = "identity", fill = "purple") +
    ggtitle("Churn Rate by Transaction Volumes and Values") +
    xlab("Transaction Type") +
    ylab("Churn Rate")  
  
  # Convert categorical variables into binary (0 for "N", 1 for "Y")
  df_2 <- df_2 %>%
    mutate(
      mobile_app = ifelse(mobile_app == "Y", 1, 0),
      internet_banking = ifelse(internet_banking == "Y", 1, 0),
      ussd_banking = ifelse(ussd_banking == "Y", 1, 0),
      churn = ifelse(churn == "Churned", 1, 0))
  
  # Chi-Square Test: Is there an association between churn and internet banking?
  chisq.test(table(df_2$churn, df_2$internet_banking)
  
             