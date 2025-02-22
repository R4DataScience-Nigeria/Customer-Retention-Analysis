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
View()


#univariate and multivariate analysis
#checking the distribution of customer churn rate in risk column
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


#How does customer churn vary by years
churn_rate_by_years <- df_2 %>% 
  group_by(years) %>% 
  summarise(churn_rate = sum(churn == "churned") / n())
churn_rate_by_tenure
ggplot(df_2, aes(x = years)) +
  geom_histogram(aes(fill = churn), bins = 50, color = "black") +
  labs(title = "Varaition of Churn rate by years",
      x = "Years")

#churn rate by risk
churn_rate_by_risk <- df_2 %>% 
  group_by(risk) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            Not_churn_rate = sum(churn == "not churned") / n())
churn_rate_by_risk


#Do customers who make use of mobile app have higher churn rate 
ggplot(df_2, aes(x = mobile_app)) +
  geom_bar(aes(fill = churn), alpha = 0.5, color = "black",
           position = "dodge") +
  labs(title = "Churnrate by Mobile app", x = "Mobile App", y = "frequency")
churn_rate_by_MA <- df_2 %>% 
  group_by(mobile_app) %>% 
  summarise(churn_rate = sum(churn == "churned") / n(),
            Not_churn_rate = sum(churn == "not churned") / n())
churn_rate_by_MA

#