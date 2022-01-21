library(tidyr)
library(dplyr)
billboard %>%
  slice(1:10)
billboard %>%
  pivot_longer(cols = starts_with("wk"), 
               names_to = "week",
               values_to = "ranking") %>%
  drop_na() %>%
  group_by(track)%>%
  slice(1:5) %>%
  ungroup() %>%
  slice(1:30)
billboard %>%
  filter(artist == "2Ge+her") %>%
  pivot_longer(cols = starts_with("wk"),
               names_to = "week",
               values_to = "ranking")
us_rent_income %>%
  pivot_wider(names_from = "variable", values_from = c("estimate", "moe"))
us_rent_income %>%
  pivot_wider(names_from = "variable", values_from =  c("estimate", "moe")) %>%
  select(locale = NAME, estimate_income, estimate_rent)%>%
  group_by(locale) %>%
  summarise(p_income_spent_on_rent = 12*estimate_rent / estimate_income) %>%
  arrange(p_income_spent_on_rent)
library(readr)
library(here)
conformity <- here("Documents", "GitHub", "into-the-tidyverse","Data", "JustCon5_TPP_Order1.csv") %>%
  read_csv() %>%
  select(sub_id = mTurkCode, starts_with("assault"), starts_with("theft")) %>%
  slice(-1) %>%
  type_convert()
conformity                     
conformity%>%
  pivot_longer(cols = -sub_id, names_to = "condition", values_to = "rating")
conformity%>%
  pivot_longer(cols = -sub_id, names_to = "condition", values_to = "rating")%>%
  separate(col = condition, into = c("crime_type", "crime_severity", "n_endorsing_punishment", "repetitio_number", "qualtrics_junk")) %>%
  select(-qualtrics_junk)

elections <- here("Documents", "GitHub", "into-the-tidyverse","Data","countypres_2000-2016.csv") %>%
  read_csv() %>%
  select(year, county, state, candidate, party, candidatevotes, totalvotes)
elections %>%
  unite(col = "location", county, state, sep = ", ")

banks <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "BankBranchesData.txt") %>%
  read_tsv()
banks

library(janitor)
banks %>%
  clean_names()
library(tidyverse)
library(here)
library(readr)
library(dplyr)

candy <- here("Documents", "Github", "into-the-tidyverse", "Data", "candyhierarchy2017.csv") %>%
  read_csv()%>%
  clean_names()%>%
  pivot_longer(cols= starts_with("Q6"),
               names_to = "Candy Type", values_to = "Rank", values_drop_na = TRUE)

#install.packages("writexl")

##  pivot_wider(names_from = "blank", values_from = c("item1", "item2")) %>%


library(janitor)
covid <- here("Documents","GitHub","into-the-tidyverse", "Data", "time_series_covid19_confirmed_US.csv") %>%  
  read_csv()%>%
  pivot_longer(cols = ends_with("/20"), names_to = "data", values_to = "covidcases")
  #clean_names()
#library("writexl")
#df <- data.frame(Name = c("Jon", "Bill", "Maria", "Ben", "Tina"),
 #                Age = c(23, 41, 32, 58, 26)
#write_xlsx(candy,"testing2.xlsx")
#install.packages("writexl")
