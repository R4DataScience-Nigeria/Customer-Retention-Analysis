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
  select(-credit_val, -x21)
  View(df_2)

# UNIVARIATE & MULTIVARIATE ANALYSIS---- By answering some research questions

#UNIVARIATE ANALYSIS
  
#Checking relationship between years and churn status
  
#Ensure churn is a numeric variable (0 for not churned, 1 for churned)
df_2$churn <- ifelse(df_2$churn == "churned", 1, 0)  
str(df_2$churn)
#Separate churned and non-churned customers
churned <- df_2 %>% filter(churn == 1)   # Churned customers
not_churned <- df_2 %>% filter(churn == 0)  # Non-churned customers
  
# Group by years and summarize counts
churn_summary <- df_2 %>%
group_by(years) %>%
summarize(
churned_count = sum(churn),   # Total churned customers per year
not_churned_count = sum(1 - churn),  # Total non-churned customers per year
total_customers = n(),  
churn_rate = churned_count / total_customers  # Churn rate per year
)
print(churn_summary)

#What percentage of customers have churned?
  
table(df_2$churn)  # Frequency count
prop.table(table(df_2$churn)) * 100  # Percentage

#About 52% of customers churned, based on the data set.

#How are customers categorized based on risk levels? In percentage
table(df_2$risk)  # Count of each risk category
prop.table(table(df_2$risk)) * 100  # Percentage breakdown

#Checking the distribution of customer churn rate in risk column
df_2 %>% 
  group_by(churn) %>% 
  summarise(count_low = sum(risk == "LOW"),
            count_medium = sum(risk == "MEDIUM"),
            count_high = sum(risk == "HIGH"))

ggplot(df_2, aes(x = risk)) +
  geom_bar( aes(fill = churn), alpha = 0.5, color = "black",
            position = "dodge") +
  labs(title = "Distribution of customer churn rate by risk", 
       x = "Risk", y="Frequency")
#Based on customer risk, the Medium risk customers have high tendency to churn compared the other risk categories (high and low risk customers)


#checking the counts 
df_2 %>% 
  group_by(risk, churn) %>% 
  summarise(count = n())

#churn rate by risk
churn_rate_by_risk <- df_2 %>% 
  group_by(risk) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            Not_churn_rate = sum(churn == "not churned") / n())
churn_rate_by_risk
 #plot to visualize risk rate of customers

#How many customers use digital banking services?
table(df_2$mobile_app)
table(df_2$internet_banking)
table(df_2$ussd_banking)
prop.table(table(df_2$mobile_app)) * 100
prop.table(table(df_2$internet_banking)) * 100
prop.table(table(df_2$ussd_banking)) * 100

# Plot visualize the use of digital services

#Do customers who make use of mobile app have higher churn rate 
ggplot(df_2, aes(x = mobile_app)) +
  geom_bar(aes(fill = churn), alpha = 0.5, color = "black",
           position = "dodge") +
  labs(title = "Churn rate by Mobile app", x = "Mobile App", y = "frequency")
churn_rate_by_MA <- df_2 %>% 
  group_by(mobile_app) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            Not_churn_rate = sum(churn == "not churned") / n())
churn_rate_by_MA
#from the fig, customers who do not make use of the mobile app have low churn rate while the customers who make use of the mobile app has a high churn rate.
#About 76% of customers who make use of the mobile app churned. We could say there is an issue with the product(Mobile app) or the set of customers introduced to the product were not used to it and found it boring


#Do customers who make use of internet_banking have higher churn rate
df_2 %>% 
group_by(churn, internet_banking) %>% 
summarise(count = n())
#plotting
ggplot(df_2, aes(x = internet_banking)) +
  geom_bar(aes(fill = churn), alpha = 0.5, color = "purple",
           position = "dodge") +
  labs(title = "Churn rate by Internet_banking", x = "Internet_Bnaking", y = "frequency")
churn_rate_by_IB <- df_2 %>% 
  group_by(internet_banking) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            Not_churn_rate = sum(churn == "not churned") / n())
churn_rate_by_IB
#from the fig, customers who do not make use of the internet_baking have low churn rate while the customers who make use of the internet_banking has a high churn rate.
#About 80% of customers who make use of the internet_banking churned. We could say there is an issue with the product(Internet_banking) or the set of customers introduced to the product were not used to it and found it boring

# Chi-Square Test: Is there an association between churn and internet banking?
chisq.test(table(df_2$churn, df_2$internet_banking))
#There is a significant r/ship between  churn and Internet banking

#Do customers who make use of ussd_banking have higher churn rate
churn_rate_by_ussd_banking <- df_2 %>% 
  group_by(ussd_banking) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            not_churn_rate = sum(churn == "not churned") /n())
churn_rate_by_ussd_banking
ggplot(df_2, aes(x = ussd_banking)) +
  geom_bar(aes(fill = churn), alpha = 0.5, color = "purple",
           position = "dodge") +
  labs(title = "Churn rate by ussd_banking", x = "ussd_Banking", y = "frequency")
churn_rate_by_UB <- df_2 %>% 
  group_by(ussd_banking) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            Not_churn_rate = sum(churn == "not churned") / n())
churn_rate_by_UB
#About 91% would churn for customers using ussd_banking, while those not using the ussd_banking would most likely not churn
#The ussd_banking, has influence on the churn rate.The higher the number of people using ussd_banking the higher the churn rate.

#Do customers who make use of credit_card have higher churn rate
df_2 %>% 
  group_by(churn, credit_card) %>% 
  summarise(count = n())
ggplot(df_2, aes(x = credit_card)) +
  geom_bar(aes(fill = churn), alpha = 0.5, color = "purple",
           position = "dodge") +
  labs(title = "Churn rate by credit_card", x = "credit_card", y = "frequency")
churn_rate_by_CCD <- df_2 %>% 
  group_by(credit_card) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            Not_churn_rate = sum(churn == "not churned") / n())
churn_rate_by_CCD
#The popular products are product 5,7 and 8. for the first 4 products() there is no difference between the churn rate and non-churn. The highest churn rate was recorded in product 7 followed by product 5 and 8. They are the only products that affect churn rate
#The first 4 product does not have any effect/influence on customer churn. We can not use it to predict our customers churn

#MULTIVARIATE ANALYSIS
#Analyzing the correlation between credit and debit volumes

cor(df_2$credit_vol, df_2$debit_vol, use = "complete.obs")
aggregate(credit_vol ~ churn, data = df_2, mean)
aggregate(debit_vol ~ churn, data = df_2, mean)
#plot to visualize the correlation btw credit & debit volumes

# Credit Volume
ggplot(df_2, aes(x = churn, y = credit_vol)) + 
  stat_summary(fun = mean, geom = "bar") + 
  labs(title = "Average Credit Volume by Churn Status", x = "Churn Status", y = "Average Credit Volume")

# Debit Volume
ggplot(df_2, aes(x = churn, y = debit_vol)) + 
  stat_summary(fun = mean, geom = "bar") + 
  labs(title = "Average Debit Volume by Churn Status", x = "Churn Status", y = "Average Debit Volume")

cor(df_2$debit_val, df_2$debit_vol, use = "complete.obs") #There is a very strong correlation between the debit value and debit volume
aggregate(debit_val ~ churn, data = df_2, mean)  
aggregate(debit_vol ~ churn, data =df_2, mean)

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

#Checking relationship between years and churn status
# Ensure churn is a numeric variable (0 for not churned, 1 for churned)
df_2$churn <- ifelse(df_2$churn == "churned", 1, 0)  
str(df_2$churn)
# Separate churned and non-churned customers
churned <- df_2 %>% filter(churn == 1)   # Churned customers
not_churned <- df_2 %>% filter(churn == 0)  # Non-churned customers

# Group by years and summarize counts
churn_summary <- df_2 %>%
  group_by(years) %>%
  summarize(
    churned_count = sum(churn),   # Total churned customers per year
    not_churned_count = sum(1 - churn),  # Total non-churned customers per year
    total_customers = n(),  
    churn_rate = churned_count / total_customers  # Churn rate per year
  )
print(churn_summary)


# Plot churned vs. non-churned counts per year
ggplot(churn_summary, aes(x = years)) +
  geom_line(aes(y = churned_count, color = "Churned Customers"), size = 1) +
  geom_line(aes(y = not_churned_count, color = "Non-Churned Customers"), size = 1) +
  ggtitle("Churned vs. Non-Churned Customers by Year") +
  xlab("Years with Bank") +
  ylab("Number of Customers") +
  scale_color_manual(values = c("Churned Customers" = "red", "Non-Churned Customers" = "blue")) +
  theme_minimal()

#WHAT IS THE RELATIONSHIP BETWEEN CUSTOMER CHURN AND CURRENCE?
churn_currency_summary <- df_2 %>%
  group_by(currency) %>%
  summarize(
  churned_count = sum(churn, na.rm = TRUE),   # Total churned customers per currency
  not_churned_count = sum(1 - churn, na.rm = TRUE),  # Total non-churned customers per currency
  total_customers = n(),  
  churn_rate = churned_count / total_customers  # Churn rate per currency
  )

# Create bar chart of churned vs. non-churned customers by currency
ggplot(churn_currency_summary, aes(x = currency)) +
  geom_bar(aes(y = churned_count, fill = "Churned"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = not_churned_count, fill = "Not Churned"), stat = "identity", position = "dodge") +
  ggtitle("Customer Churn by Currency") +
  xlab("Currency") +
  ylab("Number of Customers") +
  scale_fill_manual(values = c("Churned" = "red", "Not Churned" = "blue")) +
  theme_minimal()

# Create bar chart of churned vs churn_rate
ggplot(churn_currency_summary, aes(x = currency, y = churn_rate)) +
  geom_col(fill = "red") +
  ggtitle("Churn Rate by Currency") +
  xlab("Currency") +
  ylab("Churn Rate (%)") +
  theme_minimal()

table(df_2$churn) 

prop.table(table(df_2$churn)) * 100  

# Distribution of 'years' variable (TENURE OF DISTRIBUTION)
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

library(ggplot2)
library(dplyr)

# Function to convert proportions to data frame
get_proportion_df <- function(column, name) {df %>%
    count(!!sym(column)) %>%
    mutate(percentage = (n / sum(n)) * 100) %>%
    rename(category = !!sym(column)) %>%
    mutate(channel = name)}

# Compute proportions for each banking channel
mobile_app_df <- get_proportion_df("mobile_app", "Mobile App")
internet_banking_df <- get_proportion_df("internet_banking", "Internet Banking")
ussd_banking_df <- get_proportion_df("ussd_banking", "USSD Banking")

# Combine all into one dataset
plot_data <- bind_rows(mobile_app_df, internet_banking_df, ussd_banking_df)

ggplot(plot_data, aes(x = category, y = percentage, fill = channel)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Usage Distribution of Banking Channels",
       x = "Usage (Yes/No)",
       y = "Percentage (%)") +
  theme_minimal() +
  scale_fill_manual(values = c("Mobile App" = "blue", "Internet Banking" = "green", "USSD Banking" = "red"))

# Function to calculate churn rate for a given column
get_churn_rate <- function(column, name) {df %>%
    group_by(!!sym(column)) %>%
    summarise(churn_rate = sum(churn) / n() * 100) %>%
    filter(!!sym(column) == 1) %>%  # Only take "Yes" (1) category
    mutate(channel = name) %>%
    select(channel, churn_rate)}



