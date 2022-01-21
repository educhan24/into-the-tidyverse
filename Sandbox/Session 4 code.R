library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(dplyr)
covid <- here("Documents","GitHub","into-the-tidyverse", "Data", "time_series_covid19_confirmed_US.csv") %>%  
  read_csv() %>%
  clean_names() %>%
  select(-c(uid:fips, country_region:combined_key))%>%
  rename(county = admin2, state = province_state)%>%
  pivot_longer(cols = -c(county, state), names_to = "date",
               values_to = "cases")%>%
  mutate(date = str_remove(date, "x"),
         date = mdy(date))%>%
  group_by(state, date) %>%
  summarise(cases = sum(cases))%>%
  ungroup()

covid%>%
  slice_head(n = 10)
covid %>%
  ggplot(mapping = aes(x=date, y=cases))

covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) + 
  geom_line()
covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) + 
  geom_bar(stat = "identity")
covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) + 
  geom_area()
covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) + 
  geom_point()
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_boxplot() +
  ggtitle("covid-19 cases in the USA over time")
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_violin(scale = "width") +
  ggtitle("covid-19 cases in the USA over time")
covid %>%
  mutate(time = month(date, label = TRUE))%>%
  filter(time > "Jun")%>%
  ggplot(mapping = aes(x=time, y=cases))+
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize =5,
               binwidth = 1000) +
  ggtitle("covid-19 cases in US over time")
covid %>%
  mutate(time = month(date, label = TRUE)) %>%
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_point() +
  ggtitle("covid-19 cases in the USA over time")

covid%>%
  filter(date == as.Date("2020-09-24"))%>%
  ggplot(mapping = aes(x=cases))+
  geom_histogram() +
  ggtitle("Distribution of covid-19 cases on 9/24 in US")
covid%>%
  filter(date == as.Date("2020-09-24"))%>%
  ggplot(mapping = aes(x=cases))+
  geom_density() +
  ggtitle("Distribution of covid-19 cases on 9/24 in US")
covid%>%
  filter(date == as.Date("2020-09-24"))%>%
  ggplot(mapping = aes(x=cases))+
  geom_area(stat = "bin") +
  ggtitle("Distribution of covid-19 cases on 9/24 in US")
covid%>%
  filter(date == as.Date("2020-09-24"))%>%
  ggplot(mapping = aes(x=cases))+
  geom_freqpoly() +
  ggtitle("Distribution of covid-19 cases on 9/24 in US")
covid%>%
  filter(date == as.Date("2020-09-24"))%>%
  ggplot(mapping = aes(x=cases))+
  geom_dotplot() +
  ggtitle("Distribution of covid-19 cases on 9/24 in US")

covid%>%
  mutate(time = month(date, label = TRUE))%>%
  ggplot(mapping = aes(x=time, y=cases))+ 
  geom_bar(stat= "summary", fun = "mean")+
  ggtitle("covid 19 cases in us over time")
covid%>%
  mutate(time = month(date, label = TRUE))%>%
  ggplot(mapping = aes(x=time, y=cases))+ 
  geom_bar(stat= "summary", fun = "mean")+
  geom_dotplot(binaxis = "y", stackdir =  "center",
               dotsize = 2, binwidth = 1000)+
  ggtitle("covid 19 cases in us over time")
covid%>%
  mutate(time = month(date, label = TRUE))%>%
  ggplot(mapping = aes(x=time, y=cases))+ 
  geom_dotplot(binaxis = "y", stackdir =  "center",
               dotsize = 2, binwidth = 1000)+
  geom_bar(stat= "summary", fun = "mean")+
  ggtitle("covid 19 cases in us over time")

covid%>%
  ggplot(mapping = aes(x=date, y=cases, fill=state))+
  geom_area()+
  theme(legend.position = "bottom")

elections <- here("Documents", "GitHub", "into-the-tidyverse","Data","countypres_2000-2016.csv") %>%
  read_csv()%>%
  filter(year == 2016)%>%
  filter(party %in% c("democrat", "republican"))%>%
  group_by(state, candidate)%>%
  summarise(candidatevotes = sum(candidatevotes, na.rm=T))%>%
  group_by(state)%>%
  mutate(lean_democrat = candidatevotes/first(candidatevotes))%>%
  filter(candidate == "Hillary Clinton")%>%
  ungroup()%>%
  select(state, lean_democrat)
covid%>%
  inner_join(elections)%>%
  filter(state != "District of Columbia")%>%
  ggplot(mapping = aes(x=date, y=cases, color=lean_democrat,
                       group=state))+
  geom_line()
covid%>%
  filter(state %in% c("Tennessee", "California", "Rhode Island"))%>%
  mutate(date = month(date, label = TRUE))%>%
  group_by(state, date)%>%
  summarise(cases = sum(cases))%>%
  ungroup()%>%
  ggplot(mapping = aes(x=date, y=cases, fill=state))+
  geom_bar(stat = "summary", fun = "mean")+
  theme(legend.position = "bottom")
covid%>%
  filter(state %in% c("Tennessee", "California", "Rhode Island"))%>%
  mutate(date = month(date, label = TRUE))%>%
  group_by(state, date)%>%
  summarise(cases = sum(cases))%>%
  ungroup()%>%
  ggplot(mapping = aes(x=date, y=cases, color=state))+
  geom_bar(stat = "summary", fun = "mean")+
  theme(legend.position = "bottom")
regions <- here("Documents", "GitHub", "into-the-tidyverse","Data","state_region_division.csv")%>%
  read_csv()
covid%>%
  inner_join(regions)%>%
  group_by(region, date)%>%
  summarise(cases_mean = mean(cases, na.rm = TRUE),
            cases_sd = sd(cases, na.rm = TRUE),
            cases_n = n(),
            cases_se = cases_sd/cases_n)%>%
  ungroup()%>%
  ggplot(mapping = aes(x=date, y=cases_mean, color=region))+
  geom_line()
covid%>%
  inner_join(regions)%>%
  group_by(region, date)%>%
  summarise(cases_mean = mean(cases, na.rm = TRUE),
            cases_sd = sd(cases, na.rm = TRUE),
            cases_n = n(),
            cases_se = cases_sd/cases_n)%>%
  ungroup()%>%
  ggplot(mapping = aes(x=date, y=cases_mean, color=region))+
  geom_line(aes(color=region))
covid%>%
  inner_join(regions)%>%
  group_by(region, date)%>%
  summarise(cases_mean = mean(cases, na.rm = TRUE),
            cases_sd = sd(cases, na.rm = TRUE),
            cases_n = n(),
            cases_se = cases_sd/cases_n)%>%
  ungroup()%>%
  ggplot(mapping = aes(x=date, y=cases_mean, color=region))+
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se)) +
  geom_line()
covid%>%
  inner_join(regions)%>%
  group_by(region, date)%>%
  summarise(cases_mean = mean(cases, na.rm = TRUE),
            cases_sd = sd(cases, na.rm = TRUE),
            cases_n = n(),
            cases_se = cases_sd/cases_n)%>%
  ungroup()%>%
  ggplot(mapping = aes(x=date, y=cases_mean, color=region, fill=region))+
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se),
              alpha=0.25) +
  geom_line(size=1)
covid %>%
  inner_join(regions) %>%
  group_by(region, date) %>%
  summarise(cases_mean = mean(cases, na.rm = TRUE),
            cases_sd = sd(cases, na.rm = TRUE),
            cases_n = n(),
            cases_se = cases_sd / cases_n) %>%
  ungroup() %>%
  ggplot(mapping = aes(x=date, y=cases_mean, color=region, fill=region, linetype=region)) +
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se),
              alpha=0.25, linetype="solid") +
  geom_line(size=1)
covid%>%
  inner_join(regions)%>%
  group_by(region, date)%>%
  summarise(cases_mean = mean(cases, na.rm = TRUE),
            cases_sd = sd(cases, na.rm = TRUE),
            cases_n = n(),
            cases_se = cases_sd/cases_n)%>%
  ungroup()%>%
  ggplot(mapping = aes(x=date, y=cases_mean, color=region, fill=region,
                       linetype=region))+
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se),
              alpha=0.25) +
  geom_line(size=1)
covid%>%
  inner_join(regions)%>%
  group_by(region, date)%>%
  summarise(cases_mean = mean(cases, na.rm = TRUE),
            cases_sd = sd(cases, na.rm = TRUE),
            cases_n = n(),
            cases_se = cases_sd/cases_n)%>%
  ungroup()%>%
  ggplot(mapping = aes(x=date, y=cases_mean, color=region, fill=region,
                       linetype=region))+
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se),
              alpha=0.25, color=NA) +
  geom_line(size=1)



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



