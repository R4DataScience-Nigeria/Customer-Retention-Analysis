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


df_2 <- df %>% 
  select(-credit_val, -x21)
  View(df_2)
  
View(df_2)

#univariate and multivariate analysis
#what percentage of customers have churned?

table(df_2$churn)  # Frequency count
prop.table(table(df_2$churn)) * 100  # Percentage

#How are customers categorized based on risk levels?
table(df_2$risk)  # Count of each risk category
prop.table(table(df_2$risk)) * 100  # Percentage breakdown

#How many customers use digital banking services?
table(df_2$mobile_app)
table(df_2$internet_banking)
table(df_2$ussd_banking)
prop.table(table(df_2$mobile_app)) * 100
prop.table(table(df_2$internet_banking)) * 100
prop.table(table(df_2$ussd_banking)) * 100


df_2 %>%
  count(churn) %>%
  slice_tail(n = 10)

df_2 %>%
  count(risk) %>%
  slice_tail(n = 10)


#Churn Rate by Risk Level by percent

churn_rate_by_risk <- df_2 %>% 
  group_by(risk) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            not_churn_rate = sum(churn == "not churned") /n())
churn_rate_by_risk

#checking the counts 
df_2 %>% 
  group_by(risk, churn) %>% 
  summarise(count = n())


#checking the churn rate by mobile app, ussd, internet banking

df_2 %>% 
  group_by(churn,mobile_app) %>% 
  summarise(count = n())
  

df_2 %>% 
  group_by(churn, internet_banking) %>% 
  summarise(count = n())

df_2 %>% 
  group_by(churn, ussd_banking) %>% 
  summarise(count = n())

churn_rate_by_mobile_app <- df_2 %>% 
  group_by(mobile_app) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            not_churn_rate = sum(churn == "not churned") /n())
churn_rate_by_mobile_app

churn_rate_by_internet_banking <- df_2 %>% 
  group_by(internet_banking) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            not_churn_rate = sum(churn == "not churned") /n())
churn_rate_by_internet_banking


churn_rate_by_ussd_banking <- df_2 %>% 
  group_by(ussd_banking) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            not_churn_rate = sum(churn == "not churned") /n())
churn_rate_by_ussd_banking

df_2 %>% 
  group_by(churn, credit_card) %>% 
  summarise(count = n())
print(n = ....)

#Analyzing the correlation between credit and debit volumes

cor(df_2$credit_vol, df_2$debit_vol, use = "complete.obs")
aggregate(credit_vol ~ churn, data = df_2, mean)
aggregate(debit_vol ~ churn, data = df_2, mean)

# Credit Volume
ggplot(df_2, aes(x = churn, y = credit_vol)) + 
  stat_summary(fun = mean, geom = "bar") + 
  labs(title = "Average Credit Volume by Churn Status", x = "Churn Status", y = "Average Credit Volume")

# Debit Volume
ggplot(df_2, aes(x = churn, y = debit_vol)) + 
  stat_summary(fun = mean, geom = "bar") + 
  labs(title = "Average Debit Volume by Churn Status", x = "Churn Status", y = "Average Debit Volume")

cor(df_2$debit_val, df_2$debit_vol, use = "complete.obs")
aggregate(debit_val ~ churn, data = df_2, mean)
aggregate(debit_vol ~ churn, data =df_2, mean)

