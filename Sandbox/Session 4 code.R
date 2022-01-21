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



covidnew <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "nst-est2019-modified.csv")%>%
  read_csv()%>%
  select(state=State, population2019 = "2019")%>%
  mutate(population2019 = population2019/1000)

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

covid %>%
  inner_join(covidnew) %>%
  group_by(state)%>%
  mutate(covidrates_per_capita = cases /population2019)%>%
  ungroup()%>%
  ggplot(mapping = aes(x = state, y = covidrates_per_capita)) +
  geom_bar(stat="identity")+
  ggtitle("Covid-19 cases per USA state capita")
#or
covid %>%
  inner_join(covidnew) %>%
  group_by(state)%>%
  mutate(covidrates_per_capita = cases /population2019)%>%
  ungroup()%>%
  ggplot(mapping = aes(x = date, y = covidrates_per_capita, fill=state)) +
  geom_area()+
  theme(legend.position = "bottom") +
  ggtitle("Covid-19 cases per USA state capita")
#or
covid %>%
  inner_join(covidnew) %>%
  group_by(state)%>%
  mutate(covidrates_per_capita = cases /population2019)%>%
  ungroup()%>%
  ggplot(mapping = aes(x = date, y = covidrates_per_capita, color=state)) +
  geom_bar(stat = "summary", fun = "mean")+
  theme(legend.position = "bottom")
  

 # state.name[match(incarceration,state.abb)]%>%

#covid2 <- tibble(state_name = state.name,
                 #    state_abb = state.abb,
                    # state_region = state.region)

covid2 <- here("Documents","GitHub","into-the-tidyverse", "Data", "time_series_covid19_confirmed_US.csv") %>%  
  read_csv() %>%
  clean_names() %>%
  select(-c(uid:fips, country_region:combined_key))%>%
  rename(county = admin2, state = province_state)%>%
  pivot_longer(cols = -c(county, state), names_to = "date",
               values_to = "cases")%>%
  mutate(date = str_remove(date, "x"),
         date = mdy(date))%>%
  group_by(state) %>%
  summarise(cases = sum(cases))%>%
  ungroup()
covid2

covidnew <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "nst-est2019-modified.csv")%>%
  read_csv()%>%
  select(state=State, population2019 = "2019")%>%
  mutate(population2019 = population2019/1000)
covidnew

covid2 %>%
  inner_join(covidnew, by=state)%>%
  group_by(state)%>%
  mutate(covidrates_per_capita = cases / population2019)%>%
  drop_na() %>%
  select(state, covidrates_per_capita)%>%
  ungroup()

covid2 <- tibble(state_name = state.name,
                     state_abb = state.abb,
                     state_region = state.region)

incarceration <- here("Documents", "GitHub", "into-the-tidyverse","Data","incarceration_trends.csv")%>%
  read_csv()%>%
  group_by(state)%>%
  filter(year=="2018")%>%
  summarise(jail_per_cap= sum(total_jail_pop/(total_pop/1000), na.rm = TRUE))%>%
  left_join(covid2, by = c("state"="state_abb"))
incarceration

  select(state, jail_per_cap, covidrates_per_capita)%>%
  ungroup()
incarceration

incarceration%>%
  ggplot(mapping = aes(x=jail_per_cap, y=covidrates_per_capita,
                       group=state))+
  geom_line()
  

