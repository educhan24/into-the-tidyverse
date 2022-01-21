custom_mean <-function(this_vector){
  sum(this_vector) / length(this_vector)
}
custom_mean(c(2,6))
covid <- here("Documents","GitHub","into-the-tidyverse", "Data", "time_series_covid19_confirmed_US.csv") %>%  read_csv()

(1:5) %>%
  sum()

library(here)
library(readr)
library(dplyr)

incarceration <- here("Documents", "GitHub","into-the-tidyverse","Data", "incarceration_trends.csv")%>%
  read_csv()

ca_jail <- incarceration %>%
  filter(year == 2018, state == "CA")%>%
  select(fips, total_pop, total_jail_pop)%>%
  mutate(prop_jail = total_jail_pop/total_pop)%>%
  slice(1:10)

elections <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "countypres_2000-2016.csv") %>%
  read_csv()%>%
  filter(state_po == "CA")%>%
  filter(party %in% c("democrat", "republican")) %>%
  mutate(lean_republican = candidatevotes / first(candidatevotes))%>%
  ungroup()%>%
  filter(party == "republican") %>%
  select(FIPS, lean_republican)%>%
  mutate(more_trump = lean_republican > 1)


ca_jail %>%
  left_join(elections, by=c("fips"="FIPS"))%>%
  summarise(mean(prop_jail, na.rm = TRUE), sd(prop_jail, na.rm = TRUE))
