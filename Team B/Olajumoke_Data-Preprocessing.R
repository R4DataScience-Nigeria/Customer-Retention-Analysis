library(tidyverse)
library(dplyr)

df <- clean_churn
#Check dimension of data
dim(df)

#checking data types of variables
glimpse(df)
str(df)

#Checking credit_val
#df |>
  #count(credit_val,x21)

#Checking for unique values in acct_id
df %>%
  count(acct_id) %>%
  slice_head(n = 10) #To check first 10 observations (rows)

df %>%
  count(acct_id) %>%
  slice_tail(n = 20)
  
df %>% #Checking for duplicated values in acct_id column
    group_by(acct_id) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>%  # Arrange in descending order
    slice_head(n = 50)     # Select the top 20 rows
    #ggplot(aes(y = acct_id, x = n))+
    #geom_col()

df %>% 
    filter(acct_id == "Account_2727") %>% #checking if Account_2727 is duplicated
    View(.)
  
sum(duplicated(df) == TRUE) #Checking for duplicates

#Dropping credit_val and x21 columns
df_2 <- df %>% 
  select(-credit_val, -x21)
  View(df_2)
    
#univariate and multivariate analysis
summary(df_2)
str(df_2)


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

# Print the summary
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


#churn vs mobile_app, internet_bank, ussd_bank users

# Summarize churn counts by currency
<<<<<<< HEAD
#churn_currency_summary <- df_2 %>%
  #group_by(currency, churn) %>%  # Group by currency and churn status
  #summarize(count = n(), .groups = 'drop')  # Count customers

#print(churn_mobile_summary)
=======
churn_currency_summary <- df_2 %>%
  group_by(currency, churn) %>%  # Group by currency and churn status
  summarize(count = n(), .groups = 'drop')  # Count customers
print(churn_mobile_summary)
>>>>>>> 86558da1aa222ef51cbf2e96f27df5e3b7b1dcd4


#WHAT IS THE RELATIONSHIP BETWEEN CUSTOMER CHURN AND CURRENCY?
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




