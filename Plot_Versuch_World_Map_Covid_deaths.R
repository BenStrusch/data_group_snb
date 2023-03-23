#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
View(world)

ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") + theme_void()


confirmedraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
str(confirmedraw) # Check latest date at the end of data
deathsraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recoveredraw <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

View(deathsraw)
library(tidyr)
library(dplyr)

confirmed <- confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths <- deathsraw %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered <- recoveredraw %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))


country <- country[!(country$country=="Winter Olympics 2022" | country$country=="Summer Olympics 2020"),]
country <- full_join(confirmed, deaths) %>% full_join(recovered)

View(country)

str(country)
country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
str(country)
View(country)
country <- country %>% rename(country = Country.Region)
country <- country %>%
  group_by(country) %>%
  arrange(date)
View(country)

country <- country %>% group_by(country) %>% mutate(cumconfirmed=cumsum(confirmed), days = date - first(date) + 1)
View(country)

world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=sum(cumconfirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)

#switzerland
switzerland <- country %>% filter(country=="Switzerland")

summary(switzerland)

library(ggplot2)
ggplot(data=switzerland, aes(x=days, y=confirmed)) +
  geom_bar(stat="identity", width=0.1) +
  theme_minimal() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(world, aes(x=days, y=deaths)) +
  geom_bar(stat="identity", width=0.2) +
  theme_minimal() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Covid-19 Deaths per Day") +
  theme(plot.title = element_text(hjust = 0.5))

countrytotal <- country %>% group_by(country) %>% summarize(cumconfirmed=sum(confirmed), cumdeaths=sum(deaths), cumrecovered=sum(recovered))
#install.packages("tmap")
library(tmap)
data(World)
class(World)

countrytotal$country[!countrytotal$country %in% World$name]

world %>% select(-cumconfirmed) %>% gather("Type", "Cases", -c(date, days)) %>%
  ggplot(aes(x=days, y=Cases, colour=Type)) + geom_bar(stat="identity", width=0.2, fill="white") +
  theme_classic() +
  labs(title = "Covid-19 Global Cases", x= "Date", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.5))



p_cont <- ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy, color = hwy)) +
  theme_bw() + scale_color_distiller(palette = "Spectral") + labs(x = "Stringency", y = "Number of deaths", title="Covid Deaths vs  stringency measures", subtitle = "2021") + theme(legend.title = element_blank(), legend.position = "bottom")
p_cont

p_cont_2 <- ggplot(data = mpg) + 
  geom_jitter(mapping = aes(x = displ, y = hwy, size = hwy, color=hwy)) +
  theme_bw() + labs(x = "Stringency", y = "Number of deaths", title="Covid Deaths vs  stringency measures", subtitle = "2021") + theme(legend.title = element_blank(), legend.position = "bottom")
p_cont_2

stringency_x <- c(1,2,3,4,5,6,7,8,9,10)
deaths_y <- c(10, 9 , 8, 7, 6, 5, 4, 3, 2, 1)
happiness <- c(1,2,3,4,5,5,4,3,2,1)
gdp_ex <- c(1,2,3,4,5,5,4,3,2,1)
gdp_score <- gdp_ex * 10

string_low <- runif(25, min=0, max = 25)
deaths_new <- runif(50, min = 0, max = 10)
happiness_new <- runif(50, min=0, max=5)
gdp_sco <- runif(50,min=0, max=50)

plot_example <- data.frame(stringency_x, deaths_y, happiness, gdp_score)
plot_example

plot_example_2 <- data.frame(string_x, deaths_new, happiness_new, gdp_sco)
plot_example_2

p_cont_2 <- ggplot(data = plot_example) + geom_jitter(mapping = aes(x = stringency_x, y = deaths_y, size = happiness, color=gdp_score)) +
  theme_bw() + labs(x = "Stringency", y = "Number of deaths", title="Covid Deaths vs  stringency measures", subtitle = "2021") + theme(legend.position = "bottom")
p_cont_2

new_plot_2020 <- ggplot(data = plot_example_2) + geom_jitter(mapping = aes(x=string_x, y=deaths_new, size = happiness_new, color=gdp_sco)) +
  theme_bw() + labs(x = "Stringency", y = "Number of deaths", title="Covid Deaths vs  stringency measures", subtitle = "2021") + theme(legend.position = "bottom")
new_plot_2020

library(readr)
getwd()
setwd("C:/Users/benja/OneDrive/code/data_group_snb")
stringency_data <- read_lines("stringency_index_avg.txt", sep =";")
View(stringency_data)

stringency <- read_csv("OxCGRT_timeseries_all.csv")
View(stringency)
