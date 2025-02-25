library(here)
library(reshape2)
library(tidyverse)
library(corrplot)
library(ggstatsplot)
library(scales)

# Read the data frame from the RDS file
df <- read_rds(here("Team A", "clean_churn.rds"))

#checking data types of variables
#View(df)
#glimpse(df)
#str(df)

# removing the credit_val and x21 (contains huge numbers of missing values)
new_df <- df |>
           select(-c(credit_val,x21)) |>
           rename(churn_status = "churn")


# data inspection
glimpse(new_df)

# Understanding the Scope of Churn
# What percentage of customers have churned?

churn_percentage <- new_df %>%
  summarise(not_churn_rate = mean(churn_status == "not churned") * 100,
            churn_rate = mean(churn_status == "churned") * 100)


 churn_by_risk <- new_df %>%
  group_by(risk) %>%
  summarise(not_churn_rate = mean(churn_status == "not churned") * 100,
            churn_rate = mean(churn_status == "churned") * 100)

# How are customers categorized based on risk levels
risk_levels <- new_df |>
              count(risk)

# Proportion of customers using digital banking services

  new_df |> 
  summarise(mobile_prop = mean(mobile_app == "Y") * 100,
            internet_prop = mean(internet_banking == "Y") * 100,
            ussd_prop = mean(ussd_banking == "Y") * 100 )
  
  # Proportion of customers not using digital banking services
  new_df |> 
    summarise(mobile_prop = mean(mobile_app == "N") * 100,
              internet_prop = mean(internet_banking == "N") * 100,
              ussd_prop = mean(ussd_banking == "N") * 100 )
  
  
  
# Checking the churn rate by credit card
  
  new_df |>
    group_by(credit_card) |>
    summarise(churn_rate = mean(churn_status == "churned") * 100) |>
    arrange(desc(churn_rate))
  
 
  #first 10 observations (rows)
  new_df |>
    count(acct_id) |>
    arrange(desc(n)) |>
    slice_head(n = 10)
    
  #last 10 observations (rows)
  new_df %>%
    count(acct_id) |>
    arrange(desc(n)) |>
    slice_tail(n = 10)

#---------UNIVARIATE & MULTIVARIATE ANALYSIS
#---- By answering some research questions

#UNIVARIATE ANALYSIS
  
#Checking relationship between years,ave, subsegment, credit_vol, debit_vol and debit_val
  
  # Assuming your dataframe is called 'new_df' and the six variables are years,ave, subsegment, credit_vol, debit_vol and debit_val
  variables <- new_df %>%
    select(years,ave, subsegment, credit_vol, debit_vol,debit_val)
  
  # Calculate the correlation matrix
  #correlation_matrix <- cor(variables, use = "complete.obs")
  ggcorrmat(variables, matrix.type = "full",
            title = "Correlational Plot",
              caption = "Source:")
  
  # Convert the correlation matrix to a long format for visualization
  correlation_long <- melt(correlation_matrix)

  # Impact of transaction volumes and values on churn
  transaction_impact_on_churn <- new_df %>%
    group_by(churn_status) %>%
    summarise(credit_volume = mean(credit_vol),
              debit_volume = mean(debit_vol),
              subsegment = mean(subsegment),
              ave = mean(ave),
              years = mean(years),
              debit_value = mean(debit_val))


#Based on customer risk, the Medium risk customers have high tendency to churn compared the other risk categories (high and low risk customers)

  freq1_df <- new_df %>%
    count(risk, churn_status) %>%
    rename(Frequency = n)

  new_df %>%
    count(currency, churn_status) %>%
    rename(Frequency = n)
  
  
  # Relationship between customer churn and currency
  currency_churn_relationship <- new_df %>%
    group_by(currency, churn_status) %>%
    summarise(count = n())
  
  
  
  
#------------------------------------------- VISUALIZATIOON
  
# Plot visualize the use of digital services
  
  # Temporal Trends & Currency Influence on Churn
  # How does customer churn vary by years?
  churn_by_year <- new_df %>%
    group_by(years) %>%
    summarise(churn_rate = mean(churn_status == "churned") * 100)
  
  # create visualization for churn distribution
  ggplot(new_df, aes(x = fct_relevel(risk,c("LOW","MEDIUM","HIGH")))) +
    geom_bar( aes(fill = churn_status), alpha = 0.5, color = "black",
              position = "dodge") +
    labs(title = "Customer's Risk Churn Rate", 
         x = "Customer's Risk Profile", y="Frequency of Churn Status", 
         caption = "Source")+
    scale_y_continuous(labels = comma,
                       breaks = seq(from = 0, 140000, by = 20000))+
    scale_fill_manual(values = c("churned"="black",
                                  "not churned"="blue"))+
    theme_minimal()+
    theme(axis.title = element_text(face = "bold.italic", 
                                    color = "gray20",size = 10),
          plot.title = element_text(face = "bold",
                                    size = 12,
                                    colour = "gray20"),
          plot.caption = element_text(face = "bold.italic",
                                      size = 8))
  
  churn_by_years <- new_df %>%
    group_by(years) %>%
    summarise(not_churn_rate = mean(churn_status == "not churned") * 100,
              churn_rate = mean(churn_status == "churned") * 100) |>
    pivot_longer(
      cols = 2:3,
      values_to = "values",
      names_to = "rate"
    )
  
  #I touched this
  ggplot(churn_by_years, aes(x = years, y = values, colour = rate)) +
    geom_line() +
    labs(title = "Customer Loyalty Over Time:", 
         subtitle = "A Deep Dive into the Effect of Years in the Bank on Churn",
         x = "Years with Bank", y = "Number of Customers",
         caption = "Source:", color = "Rate")+
    scale_color_manual(values = c("churn_rate"="black",
                                  "not_churn_rate"="blue"))+
    annotate("text",x = 1, y= 10, label = "Not Churn", color = "blue")+
    annotate("text", x = 0, y = 90, label = "Churn", color = "black")+
    scale_x_continuous(breaks = seq(from = 0, to = 13, by = 2))+
    scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10))+
    theme_minimal()+
    theme(legend.position = "top",axis.title = element_text(face = "bold.italic", 
                                    color = "gray20",size = 10),
          plot.title = element_text(face = "bold",
                                    size = 12,
                                    colour = "gray20"),
          plot.caption = element_text(face = "bold.italic",
                                      size = 8))
  
#Do customers who make use of mobile app have higher churn rate 
ggplot(new_df, aes(x = mobile_app)) +
  geom_bar(aes(fill = churn_status), alpha = 0.5, color = "black",
           position = "dodge") +
  labs(title = "Churn rate by Mobile app", x = "Mobile App", y = "frequency")+
  scale_fill_manual(values = c("churned"="black",
                                "not churned"="blue"))+
  theme_minimal()


# Where we STOP!!!!!!!!!!!! -----------------------------------------------

churn_rate_by_MA <- new_df %>% 
  group_by(mobile_app) %>% 
  summarise(churn_rate = sum(churn_status == "churned") / n(),
            Not_churn_rate = sum(churn_status == "not churned") / n())
churn_rate_by_MA
#from the fig, customers who do not make use of the mobile app have low churn rate while the customers who make use of the mobile app has a high churn rate.
#About 76% of customers who make use of the mobile app churned. We could say there is an issue with the product(Mobile app) or the set of customers introduced to the product were not used to it and found it boring


churn_rate_by_IB <- new_df %>% 
  group_by(internet_banking) %>% 
  summarise(churn_rate = sum(churn_status == "churned") / n(),
            Not_churn_rate = sum(churn_status == "not churned") / n())

churn_rate_by_IB


churn_rate_by_USSD <- new_df %>% 
  group_by(ussd_banking) %>% 
  summarise(churn_rate = sum(churn_status == "churned") / n(),
            Not_churn_rate = sum(churn_status == "not churned") / n())


churn_rate_by_USSD

#Do customers who make use of internet_banking have higher churn rate
#plotting
ggplot(new_df, aes(x = internet_banking)) +
  geom_bar(aes(fill = churn_status), position = "dodge") +
  labs(title = "Churn rate by Internet banking", 
       x = "Internet Banking", y = "Frequency")+
  scale_fill_manual(values = c("churned"="black",
                                "not churned"="blue"))+
  theme_minimal()+
  theme(legend.position = "top",axis.title = element_text(face = "bold.italic", 
                                                          color = "gray20",size = 10),
        plot.title = element_text(face = "bold",
                                  size = 12,
                                  colour = "gray20"),
        plot.caption = element_text(face = "bold.italic",
                                    size = 8))




ggplot(new_df, aes(x = ussd_banking)) +
  geom_bar(aes(fill = churn_status), position = "dodge") +
  labs(title = "Churn rate by USSD", 
       x = "USSD", y = "Frequency")+
  scale_fill_manual(values = c("churned"="black",
                               "not churned"="blue"))+
  theme_minimal()+
  theme(legend.position = "top",axis.title = element_text(face = "bold.italic", 
                                                          color = "gray20",size = 10),
        plot.title = element_text(face = "bold",
                                  size = 12,
                                  colour = "gray20"),
        plot.caption = element_text(face = "bold.italic",
                                    size = 8))

ggplot(new_df, aes(x = mobile_app)) +
  geom_bar(aes(fill = churn_status), position = "dodge") +
  labs(title = "Churn rate by Mobile Application", 
       x = "Mobile Application", y = "Frequency")+
  scale_fill_manual(values = c("churned"="black",
                               "not churned"="blue"))+
  theme_minimal()+
  theme(legend.position = "top",axis.title = element_text(face = "bold.italic", 
                                                          color = "gray20",size = 10),
        plot.title = element_text(face = "bold",
                                  size = 12,
                                  colour = "gray20"),
        plot.caption = element_text(face = "bold.italic",
                                    size = 8))


#from the fig, customers who do not make use of the internet_baking have low churn rate while the customers who make use of the internet_banking has a high churn rate.
#About 80% of customers who make use of the internet_banking churned. We could say there is an issue with the product(Internet_banking) or the set of customers introduced to the product were not used to it and found it boring

# Chi-Square Test: Is there an association between churn and internet banking?
#chisq.test(table(new_df$churn_status, new_df$internet_banking))
#There is a significant r/ship between  churn and Internet banking

#Do customers who make use of ussd_banking have higher churn rate
churn_rate_by_ussd_banking <- new_df %>% 
  group_by(ussd_banking) %>% 
  summarise(churn_rate = sum(churn_status == "churned") / n(),
            not_churn_rate = sum(churn_status == "not churned") /n())
churn_rate_by_ussd_banking

ggplot(new_df, aes(x = ussd_banking)) +
  geom_bar(aes(fill = churn_status), alpha = 0.5, position = "dodge") +
  labs(title = "Churn rate by ussd_banking", x = "ussd_Banking", y = "frequency")+
  scale_fill_manual(values = c("churned"="black",
                               "not churned"="blue"))+
  theme_minimal()+
  theme(legend.position = "top",axis.title = element_text(face = "bold.italic", 
                                                          color = "gray20",size = 10),
        plot.title = element_text(face = "bold",
                                  size = 12,
                                  colour = "gray20"),
        plot.caption = element_text(face = "bold.italic",
                                    size = 8))


#About 91% would churn for customers using ussd_banking, while those not using the ussd_banking would most likely not churn
#The ussd_banking, has influence on the churn rate.The higher the number of people using ussd_banking the higher the churn rate.

#Do customers who make use of credit_card have higher churn rate
#new_df %>% 
 #group_by(churn_status, credit_card) %>% 
  #count(credit_card) %>% 
  #gt::gt()

churn_rate_by_CCD <- new_df %>% 
  group_by(credit_card) %>% 
  summarise(churn_rate = sum(churn_status == "churned") / n(),
            Not_churn_rate = sum(churn_status == "not churned") / n()) %>% 
  gt()
churn_rate_by_CCD
#The popular products are product 5,7 and 8. for the first 4 products() there is no difference between the churn rate and non-churn. The highest churn rate was recorded in product 7 followed by product 5 and 8. They are the only products that affect churn rate
#The first 4 product does not have any effect/influence on customer churn. We can not use it to predict our customers churn

#MULTIVARIATE ANALYSIS
#Analyzing the correlation between credit and debit volumes


# Credit Volume
ggplot(new_df, aes(x = churn_status, y = credit_vol)) + 
  stat_summary(fun = mean, geom = "bar") + 
  labs(title = "Average Credit Volume by Churn Status", x = "Churn Status",
       y = "Average Credit Volume")


#Checking relationship between years and churn status
# Group by years and summarize counts
churn_summary <- new_df %>%
  group_by(years) %>%
  summarize(
    churned = sum(churn_status == "churned"),   # Total churned customers per year
    not_churned = sum(churn_status == "not churned"),  # Total non-churned customers per year
  churn_rate = mean(churn_status == "churned")
    )
print(churn_summary)


#WHAT IS THE RELATIONSHIP BETWEEN CUSTOMER CHURN AND CURRENCE?
churn_currency_summary <- new_df %>%
  group_by(currency) %>%
  summarize(
    churned = sum(churn_status == "churned"),   # Total churned customers per year
    not_churned = sum(churn_status == "not churned"),  # Total non-churned customers per year
    churn_rate = mean(churn_status == "churned")
  ) %>% 
  arrange(desc(churn_rate))

churn_currency_summary %>% 
  ggplot(aes(y = churn_rate,  x= fct_reorder(currency, churn_rate)))+
  geom_bar(stat = "identity", aes(fill= currency))+
labs(x = "Currency", y = "Churn Rate", title = "Churn Rate by Currency")+
  scale_fill_brewer(palette = "PuBu")+
  theme_minimal()+
  theme(legend.position = "",axis.title = element_text(face = "bold.italic", 
                                                          color = "gray20",size = 10),
        plot.title = element_text(face = "bold",
                                  size = 12,
                                  colour = "gray20"),
        plot.caption = element_text(face = "bold.italic",
                                    size = 8))



# Barchart of balance
ggplot(new_df, aes(x = bal)) + 
  geom_bar(fill="blue") +
  ggtitle("Distribution of Account Balance")+
  theme_minimal()+
  scale_y_continuous(labels = comma)+
  theme(legend.position = "",axis.title = element_text(face = "bold.italic", 
                                                       color = "gray20",size = 10),
        plot.title = element_text(face = "bold",
                                  size = 12,
                                  colour = "gray20"),
        plot.caption = element_text(face = "bold.italic",
                                    size = 8))



# Function to calculate churn rate for a given column
get_churn_rate <- function(column, name) {df %>%
    group_by(!!sym(column)) %>%
    summarise(churn_rate = sum(churn) / n() * 100) %>%
    filter(!!sym(column) == 1) %>%  # Only take "Yes" (1) category
    mutate(channel = name) %>%
    select(channel, churn_rate)}



