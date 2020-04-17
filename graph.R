library(tidyverse)
library(xts)
#library(tbl2xts)
setwd("~/Documents/GitHub/covid-19")

# Get latest data from https://coronavirus.data.gov.uk/ - no way to automate this that I can see
cases <- read_csv("data/coronavirus-cases.csv")
deaths <- read_csv("data/coronavirus-deaths.csv") # Note deaths only from late-March onwards

# Country level data (England only)
eng_cases <- cases %>% filter(`Area name`=="England") %>% 
  select(date=`Specimen date`, cases_daily=`Daily lab-confirmed cases`, cases_cum=`Cumulative lab-confirmed cases`)
eng_deaths <- deaths %>% filter(`Area name`=="England") %>% 
  select(date=`Reporting date`, deaths_daily=`Daily hospital deaths`, deaths_cum=`Cumulative hospital deaths`)
# Filter for >50 cumulative cases, also removes NAs, and make tidy
eng <- full_join(eng_cases, eng_deaths) %>% 
  filter(cases_cum>50) %>% 
  select (date, deaths=deaths_daily, cases=cases_daily) %>% 
  pivot_longer(-date, names_to = "type", values_to = "count")

ggplot(eng, aes(x = date, y = count)) + 
  geom_line(aes(color = type), size = 1) +
  scale_color_manual(values = c("black", "grey")) +
  scale_y_continuous(trans='log10') +
  scale_x_date(date_breaks = "week" , date_labels = "%d-%b") +
  labs(title="Daily COVID-19 cases and deaths in England", x="Date", y="Cases / deaths", color="Plot") +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")), color = "darkgreen", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-03-11"), y=1000, label = "Symptomatic self-isolation", color = "darkgreen", angle = 90) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-20")), color = "red", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-03-19"), y=1000, label = "Last night in the pubs", color = "red", angle = 90) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-23")), color = "darkgreen", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-03-22"), y=1000, label = "Pseudo-lockdown", color = "darkgreen", angle = 90) +
  geom_vline(xintercept=as.numeric(as.Date("2020-04-04")), color = "red", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-04-03"), y=1000, label = "Unseasonably warm weekend", color = "red", angle = 90) +
  geom_vline(xintercept=as.numeric(as.Date("2020-04-10")), color = "red", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-04-09"), y=1000, label = "Easter weekend", color = "red", angle = 90) +
  theme_minimal()

# Alternative source for deaths data https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
# Can't be bothered to go through all the hoops of readxl and automatic data extraction so here's a processed csv
data <- read_csv("data/processed.csv")
data$date = as.Date(data$date)
data$roll7c <- rollmean(data$cases, 7, fill=NA)
data$roll7d <- rollmean(data$deaths, 7, fill=NA)
data_roll <- filter(data, date<=as.Date("2020-04-14"))  %>% select(date, roll7c, roll7d) %>% 
  pivot_longer(-date, names_to = "type", values_to = "count")
data <- filter(data, date<=as.Date("2020-04-14"))  %>% select(date, cases, deaths) %>% 
  pivot_longer(-date, names_to = "type", values_to = "count")

ggplot(data, aes(x = date, y = count)) + 
  geom_point(aes(color = type), size = 1) +
  geom_line(data=data_roll, aes(color = type), size = 1)+
  scale_color_manual(values = c("black", "grey", "black", "grey")) +
  scale_y_continuous(trans='log10', limits=c(10,5000)) +
  scale_x_date(date_breaks = "weeks" , date_labels = "%d-%b") +
  labs(title="Daily COVID-19 new cases and deaths in England", x="Date", y="Cases / deaths", color="Plot") +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-12")), color = "darkgreen", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-03-11"), y=12, label = "Symptomatic self-isolation", color = "darkgreen", angle = 90, hjust=0) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-20")), color = "red", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-03-19"), y=12, label = "Last night in the pubs", color = "red", angle = 90, hjust=0) +
  geom_vline(xintercept=as.numeric(as.Date("2020-03-23")), color = "darkgreen", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-03-22"), y=12, label = "Pseudo-lockdown", color = "darkgreen", angle = 90, hjust=0) +
  geom_vline(xintercept=as.numeric(as.Date("2020-04-04")), color = "red", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-04-03"), y=12, label = "Unseasonably warm weekend", color = "red", angle = 90, hjust=0) +
  geom_vline(xintercept=as.numeric(as.Date("2020-04-10")), color = "red", linetype=3) +
  annotate(geom = "text", x=as.Date("2020-04-09"), y=12, label = "Easter weekend", color = "red", angle = 90, hjust=0) +
  theme_minimal()

ggsave("Rplot.png")

# Time series analysis
#eng$date = as.POSIXlt(eng$date)
#eng_ts <- tbl_xts(eng)
#autoplot(eng_ts, facet=F)
