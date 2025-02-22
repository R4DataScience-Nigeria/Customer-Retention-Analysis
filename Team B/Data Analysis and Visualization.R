library(here)
library(readr)
library(naniar)
library(dplyr)
library(tidyverse)
df <- read_rds(here("Team A", "clean_churn.rds"))

gg_miss_var(df)


df %>% 
  group_by(acct_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>%  # Arrange in descending order
  slice_head(n = 50) %>%     # Select the top 20 rows
ggplot(aes(y = acct_id, x = n))+
  geom_col()


df %>% 
  filter(acct_id == "Account_2727") %>% 
  View(.)

sum(duplicated(df) == TRUE)
