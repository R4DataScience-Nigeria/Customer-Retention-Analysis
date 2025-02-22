library(here)
library(readr)
library(tidyverse)

# Read the data frame from the RDS file
df <- read_rds(here("Team A", "clean_churn.rds"))

#View(df)

#Check dimension of data

dim(df)

#checking data types of variables

glimpse(df)

str(df)

#Checking credit_val

df |>
  count(credit_val,x21)


#Checking for unique values in acct_id

df %>%
  count(acct_id) %>%
  slice_head(n = 10) #To check first 10 observations (rows)


df %>% #Checking for duplicated values in acct_id column
  group_by(acct_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%  # Arrange in descending order
  slice_head(n = 50)     # Select the top 20 rows

#ggplot(aes(y = acct_id, x = n))+
#geom_col()


df %>% 
  filter(acct_id == "Account_2727") %>% #checking if Account_2727 is duplicated
  View()

#sum(duplicated(df) == TRUE) #Checking for duplicates


#Dropping credit_val and x21 columns

df_2 <- df %>% 
  select(-credit_val, -x21) %>%
  View()


#Establishing some Metrics

# a) Customer Churn Rate (CCR)= No of Churned cust/Total No of churned Cust

# total_No_cust <- 499152
# No_churned_cust <-
