library(tidyverse)

mutate(across(where(is.character), as.factor))