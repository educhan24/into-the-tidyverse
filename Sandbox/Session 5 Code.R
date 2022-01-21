library(tidyverse)
library(lubridate)
library(janitor)
library(here)

covid <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "time_series_covid19_confirmed_US.csv")%>%
  read_csv() %>%
  clean_names() %>%
  select(-c(uid:fips, country_region:combined_key)) %>%
  rename(county = admin2, state = province_state) %>%
  pivot_longer(cols = -c(county, state), names_to = "date", values_to = "cases") %>%
  mutate(date = str_remove(date, "x"),
         date = mdy(date)) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases)) %>%
  ungroup()

elections <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "countypres_2000-2016.csv")%>%
  read_csv() %>%
  filter(year == 2016) %>%
  filter(party %in% c("democrat", "republican")) %>%
  group_by(state, candidate) %>%
  summarise(candidatevotes = sum(candidatevotes, na.rm=T)) %>%
  group_by(state) %>%
  mutate(lean_democrat = candidatevotes / first(candidatevotes)) %>%
  filter(candidate == "Hillary Clinton") %>%
  ungroup() %>%
  select(state, lean_democrat)

regions <-here("Documents", "GitHub", "into-the-tidyverse", "Data", "state_region_division.csv") %>%
  read_csv()

covid %>%
  ggplot(mapping = aes(x=date, y=cases)) +
  xlab("Time") +
  ylab("Raw case numbers")+
  ggtitle(label = "covid-19 cases over time", 
          subtitle = "(not normalized by state population)")
covid%>%
  ggplot(mapping = aes(x=date, y=cases, color=state)) +
  geom_line() +
  facet_wrap(~state) +
  theme(legend.position = "bottom")

covid %>%
  inner_join(elections)%>%
  mutate(ideology = if_else(
    lean_democrat > 1, 
    "Democratic-leaning", "Republican-leaning")) %>%
  inner_join(regions) %>%
  ggplot(mapping = aes(x=date, y=cases, group=state))+
  geom_line() +
  facet_wrap(~region + ideology, ncol=2)

covid %>%
  inner_join(elections)%>%
  mutate(ideology = if_else(
    lean_democrat > 1, 
    "Democratic-leaning", "Republican-leaning")) %>%
  inner_join(regions) %>%
  ggplot(mapping = aes(x=date, y=cases, group=state))+
  geom_line() +
  facet_grid(rows = vars(region), cols= vars(ideology))

covid %>%
  inner_join(elections)%>%
  mutate(ideology = if_else(
    lean_democrat > 1, 
    "Democratic-leaning", "Republican-leaning")) %>%
  inner_join(regions) %>%
  ggplot(mapping = aes(x=date, y=cases, group=state))+
  geom_line() +
  facet_grid(region ~ ideology)

covid%>%
  ggplot(aes(x=date, y=cases, group=state))+
  geom_line() +
  scale_y_continuous(name = "Raw covid-19 case counts")
covid%>%
  ggplot(aes(x=date, y=cases, group=state))+
  geom_line() +
  scale_y_log10(name = "Log-transformed covid-19 case counts")
covid%>%
  ggplot(aes(x=date, y=cases, group=state))+
  geom_line() +
  scale_y_sqrt(name = "Square-root-transformed covid19 cases")
covid%>%
  ggplot(aes(x=date, y=cases, group=state))+
  geom_line() +
  scale_y_binned(name = "Binned covid-19 case counts")
covid%>%
  ggplot(aes(x=date, y=cases, group=state))+
  geom_line() +
  scale_y_reverse(name = "Inexplicably plotted covid-19 case counts")

covid%>%
  inner_join(regions)%>%
  ggplot(mapping = aes(x=date, y=cases, color=region, group=state))+
  geom_line() +
  scale_color_discrete(name= "census-designated region:")
covid%>%
  inner_join(regions)%>%
  ggplot(mapping = aes(x=date, y=cases, color=region, group=state))+
  geom_line() +
  scale_color_viridis_d(name= "census-designated region:",
                        begin = 0.1, end = 0.8, option = "plasma")
covid%>%
  inner_join(regions)%>%
  ggplot(mapping = aes(x=date, y=cases, color=region, group=state))+
  geom_line() +
  scale_color_brewer(name= "census-designated region:", palette="GnBu")
covid%>%
  inner_join(regions)%>%
  ggplot(mapping = aes(x=date, y=cases, color=region, group=state))+
  geom_line() +
  scale_color_manual(name= "census-designated region:", 
                     values = c("#3182bd", "#de2d26", "#31a354", "#756bb1"))

covid%>%
  inner_join(elections)%>%
  filter(state != "District of Columbia")%>%
  ggplot(mapping = aes(x=date, y=cases, color=lean_democrat, group=state))+
  geom_line()+
  scale_color_continuous(name = "democrat voting ratio:")
covid%>%
  inner_join(regions)%>%
  ggplot(mapping = aes(x=date, y=cases, color=region, group=state))+
  geom_line()+
  scale_color_viridis_d(name = "Census-designated region:",
                        begin = 0.1, end =0.8, option = "plasma")+
  coord_cartesian(xlim=c(mdy("03/01/2020"), mdy("09/24/2020")))

covid%>%
  inner_join(regions)%>%
  ggplot(mapping = aes(x=date, y=cases, color=region, group=state))+
  geom_line()+
  scale_color_viridis_d(name = "Census-designated region:",
                        begin = 0.1, end =0.8, option = "plasma")+
  coord_fixed(ratio = 1/100000)
covid%>%
  filter(date == mdy("09/24/2020"))%>%
  filter(state %in% c("Tennessee", "California", "Rhode Island"))%>%
  ggplot(mapping = aes(x=date, y=cases, fill=state))+
  geom_bar(stat = "identity")
covid%>%
  filter(date == mdy("09/24/2020"))%>%
  filter(state %in% c("Tennessee", "California", "Rhode Island"))%>%
  ggplot(mapping = aes(x=date, y=cases, fill=state))+
  geom_bar(stat = "identity") +
  coord_polar(theta = "y")

covid_regions <- covid%>%
  inner_join(regions)
covid_regions %>%
  ggplot(mapping = aes(x=date, y=cases, group=state))+
  geom_line()+
  facet_grid(cols=vars(region))+
  theme_void()

covid_regions%>%
  ggplot(mapping = aes(x=date, y=cases, color=region, group=state))+
  ggtitle("covid-19 cases over time")+
  geom_line() +
  facet_grid(cols = vars(region))+
  theme_bw()+
  theme(panel.grid = element_blank(), legend.position = "bottom", plot.title=element_text(hjust = 0.5))
covid %>%
  filter(date == mdy("09/24/2020")) %>%
  filter(state %in% c("Tennessee", "California", "Rhode Island")) %>%
  ggplot(mapping = aes(x=date, y=cases, fill=state))+
  geom_bar(stat = "identity", position = position_dodge())

overlap <- tibble(
  x=rep(1, times=20),
  y=rep(1, times=20)
)
overlap%>%
  ggplot(aes(x, y))+
  geom_point(position = position_jitter(width=0.01, height=0))+
  coord_cartesian(xlim=c(0.95, 1.05))
my_plot <- covid_regions%>%
  ggplot(mapping = aes(x=date, y=cases, group=state))+
  ggtitle("covid-19 cases over time")+
  geom_line() + 
  facet_grid(cols=vars(region))
my_plot
theme_jae <-list(
  theme_bw(), theme(panel.grid = element_blank(),
                    plot.title = element_text(hjust = 0.5))
)
covid_regions %>%
  ggplot(mapping = aes(x=date, y=cases, group=state))+
  ggtitle("covid-19 cases over time")+
  geom_line() +
  facet_grid(cols=vars(region))+
  theme_jae
my_plot + theme_jae

ggsave(
  filename = here("Output", "ggsave_example.pdf"),
  plot= my_plot + theme_jae,
  width = 8,
  height = 4,
  units = "in",
  dpi = 300,
  useDingbats = FALSE
)

#### exercises

babynames <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "Popular_Baby_Names.csv")%>%
  read_csv()%>%
  mutate_each("Child's First Name", funs=toupper)%>%
  select(year = "Year of Birth", name = "Child's First Name", count = Count)%>%
  group_by(year, name)%>%
  summarise(count2= sum(count))%>%
  ungroup()
babynames
#could maybe use facet to make each graph a name
babynames%>%
  filter(name %in% c("IRIS", "KAYLA", "JAZMIN"))%>%
  ggplot(mapping = aes(x=year, y=count2, color = name)) +
  geom_line() +
  scale_color_viridis_d(name = "Names",
                        begin = 0.1, end = 0.8, option = "plasma")+
  ggtitle(label = "Name occurences over time")+
  scale_y_continuous(name = "Name occurences")

babynames2 <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "Popular_Baby_Names.csv")%>%
  read_csv()%>%
  mutate_each("Child's First Name", funs=toupper)%>%
  select(year = "Year of Birth", name = "Child's First Name", ethnicity = Ethnicity, ranking = Rank)%>%
  filter(year== 2014)%>%
  filter(ranking ==(1:3))%>%
  group_by(ranking)%>%
  arrange(ranking)%>%
  #summarise(count2= sum(count))%>%
  ungroup()
babynames2
babynames2%>%
  ggplot(mapping = aes(x=name, y=ranking, fill = ethnicity)) +
  geom_bar(stat = "summary", fun = "mean") +
 # scale_color_viridis_d(name = "Names",
                     #   begin = 0.1, end = 0.8, option = "plasma")+
  ggtitle(label = "Most commonly ranked names in 2014")+
  scale_y_continuous(name = "Ranking")

#how gender affects how people perceive the dress
#or trick or treating
candy <- here("Documents", "GitHub", "into-the-tidyverse", "Data", "candyhierarchy2017.csv")%>%
  read_csv()%>%
  select(gender = "Q2: GENDER", trick_or_treating =  "Q1: GOING OUT?", dress_color = "Q10: DRESS")%>%
  drop_na()%>%
  filter(dress_color %in% c("White and gold","Blue and black"))%>%
  group_by(dress_color, gender, trick_or_treating)%>%
  count(dress_color)%>%
  ggplot(mapping = aes(x=dress_color, y= n)) +
  geom_bar(stat = "identity", color = "black", fill = "orange") +
  facet_wrap(~gender + trick_or_treating)+
  theme_dark() +
  scale_y_continuous(name = "Number of instances")+
  ggtitle("How Gender & Trick-Or-Treating
          Status Relate to Color Perception of Dress")

candy

  