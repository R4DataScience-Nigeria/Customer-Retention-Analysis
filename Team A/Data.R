library(dplyr)
library(readr)
library(googledrive)
library(janitor)
library(stringr)

# Authenticate Google account
#drive_auth()
# Read the file directly from Google Drive
#file <- drive_get("Bank_customer_churn.txt")
#file_id <- as_id(file)
# Read the file directly into R
#path <- drive_download(file_id, path = tempfile(), overwrite = TRUE)
#txt_data <- read_delim(drive_download(file_id, path = tempfile(), overwrite = TRUE), delim = "/t", col_names = TRUE)



##Read in the data------------------------------------------------
churn_df <- read_table("Team A/Bank_customer_churn.txt")

#View(Bank_customer_churn)
#dim(Bank_customer_churn)

## Data Cleaning---------------------------------------------------

str(churn_df) # check data structure 

glimpse(churn_df) # Inspecting data structure


# 1. change column names to lower case
new_churn <- churn_df |>
         clean_names() |> # change names to lower case
         rename( #rename long column names
           years = "years_with_bank",
           risk = "risk_rating",
           scheme = "scheme_type",
           mobile_app = "mobile_app_adoption",
           internet_banking = "internet_banking_adoption",
           ussd_banking = "ussd_banking_adoption",
           termloan = "termloan_status",
           credit_vol = "last_12_months_credit_volume",
           debit_vol = "last_12_months_debit_volume",
           debit_val = "last_12_months_debit_value",
           credit_val = "last_12_months_credit_value",
         ) |>
  filter(!(ave %in% c("GBP", "JPY", "NGN", "SBA", "USD"))) |>
  mutate(across(c(ave, subsegment, debit_vol, debit_val, credit_val), 
                ~ parse_number(str_replace_all(., ",", "") %>%
                                 str_replace_all("-", "0")))) |>
  mutate(across(where(is.character), as.factor))



new_churn |>
  count(ave)




# 3. change column data type



# Randomly select 2/3 of the data for analysis
sample_churn <- bank_churn %>% sample_frac(size = 2/3)

# Save sample into csv format 
write.csv(sample_churn, "sample_churn.csv", row.names = FALSE)

cat("Sample data has been saved as 'sample_churn.csv'.")


library(readr)
sample_churn <- read_csv("~/sample_churn.csv")
View(sample_churn)
summary(sample_churn)
str(sample_churn)
sample_churn$AVE.BAL <- as.numeric(levels(sample_churn$AVE.BAL))[sample_churn$AVE.BAL]  
sample_churn$AVE.BAL <- as.numeric(sample_churn$AVE.BAL)  
sample_churn$AVE.BAL[is.na(sample_churn$AVE.BAL)] <- mean(sample_churn$AVE.BAL, na.rm = TRUE)  
sample_churn$CHURN <- as.factor(sample_churn$CHURN)
data_clean <- na.omit(sample_churn)
sample_churn$AVE.BAL[is.na(sample_churn$AVE.BAL)] <- mean(sample_churn$AVE.BAL, na.rm = TRUE)
sample_churn$CHURN <- as.factor(sample_churn$CHURN)
sample_churn$RISK_RATING <- as.character(sample_churn$Risk_RATING)
sample_churn$SCHEME_TYPE <- as.factor(sample_churn$SCHEME_TYPE)
unique(sample_churn$SCHEME_TYPE)
sample_churn$SCHEME_TYPE <- trimws(sample_churn$SCHEME_TYPE)
data_clean <- unique(sample_churn)
boxplot(sample_churn$AVE.BAL)
data_clean <- data[sample_churn$AVE.BAL < quantile(sample_churn$AVE.BAL, 0.99)]
sample_churn$AVE.BAL[sample_churn$AVE.BAL > quantile(sample_churn$AVE.BAL,0.99)]<- quantile(sample_churn$AVE.BAL,0.99)
sample_churn$CURRENCY <- tolower(sample_churn$CURRENCY)
sample_churn$CURRENCY <- trimws(sample_churn$CURRENCY)
colnames(sample_churn)<- c("Account_id","Years_With_Bank","Churn","Risk_Rating",
                           "CURRENCY","Average_Balance", "SCHEME_TYPE","Mobile_App
                   _Adoption","Internet_Banking_Adoption", 
                           "USSD_Banking_Adoption","Digital_loan","Unsecured_Loan",
                           "Termloan_Status","Credit_Card","Subsegment",
                           "Last_12_Months_Credit_Volume","Last_12_Months_Debit_Volume",
                           "Last_12_Months_Debit_Value","Last_12_Months_Credit_Value")
write.csv(data_clean,"cleaned_sample_churn.csv",row.names = FALSE)