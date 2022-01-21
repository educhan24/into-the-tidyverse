library(tidyverse)
library(lubridate)
library(janitor)
library(here)


covidcases <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "time_series_covid19_confirmed_US.csv")%>%
  read_csv() %>%
  select(-c(UID:Admin2, Country_Region:Combined_Key))%>%
  rename(state = Province_State)%>%
  pivot_longer(cols = -c(state), names_to = "date",
               values_to = "cases")%>%
  mutate(date = str_remove(date, "x"),
         date = mdy(date))%>%
  group_by(state) %>%
  summarise(cases = sum(cases))%>%
  ungroup()
covidcases

pop <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "nst-est2019-modified.csv")%>%
  read_csv()%>%
  drop_na() %>%
  select(state = State, population = "2019")%>%
  mutate(population = population/1000)%>%
  left_join(covidcases, by = "state")%>%
  mutate(cases_per_pop = cases /  population)%>%
  select(state, cases_per_pop)%>%
  ungroup()

incarceration <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "incarceration_trends.csv")%>%
  read_csv()%>%
  select(state, year, total_jail_pop)%>%
  drop_na() %>%
 # state.name[match("state", state.abb)]%>%
  filter(year=="2018")%>%
  group_by(state)%>%
  summarise(jail_per_cap = sum(total_jail_pop)/1000)

state_crosswalk <- tibble(state_name = state.name,
                          state_abb = state.abb)
state_crosswalk

incarceration %>%
  inner_join(state_crosswalk, by = c("state"="state_abb"))%>%
  inner_join(pop, by = c("state_name" = "state"))%>%
  select(state, jail_per_cap, cases_per_pop)%>%
  ggplot(mapping = aes(x=jail_per_cap, y=cases_per_pop, fill=state)) +
  geom_area() +
  theme(legend.position = "bottom")

