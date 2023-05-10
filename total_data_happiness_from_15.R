library(tidyverse)
library(ggthemes)
library(distill)
library(rmarkdown)
library(lme4)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggrepel)


OxCGRTall <- read_excel("OxCGRT_timeseries_all.xlsx")
#head(OxCGRTall)
#colnames(OxCGRTall)

OxCGRT_country <- OxCGRTall %>%
  filter (jurisdiction == "NAT_TOTAL")
#head(OxCGRT_country)

OxCGRT <- OxCGRT_country %>%
  select (-region_code, -region_name, -jurisdiction, -c(contains("2023")))

Ox2020 <- OxCGRT %>%
  select (country_code, country_name, c(contains("2020")))
Ox2021 <- OxCGRT %>%
  select (country_code, country_name, c(contains("2021")))
Ox2022 <- OxCGRT %>%
  select (country_code, country_name, c(contains("2022")))
Ox2020_Str <- pivot_longer(Ox2020, cols = c(contains("2020")), names_to = "date", values_to = "stringency")
Ox2021_Str <- pivot_longer(Ox2021, cols = c(contains("2021")), names_to = "date", values_to = "stringency")
Ox2022_Str <- pivot_longer(Ox2022, cols = c(contains("2022")), names_to = "date", values_to = "stringency")

AvgStr2020 <- Ox2020_Str %>%
  group_by(country_name) %>%
  summarize(mean_Str2020 = mean(stringency))
AvgStr2021 <- Ox2021_Str %>%
  group_by(country_name) %>%
  summarize(mean_Str2021 = mean(stringency))
AvgStr2022 <- Ox2022_Str %>%
  group_by(country_name) %>%
  summarize(mean_Str2022 = mean(stringency))

Avg_Str_0 <- OxCGRT_country %>%
  select(country_code, country_name)
Avg_Str_22 <- left_join(AvgStr2022, Avg_Str_0)
Avg_Str_21 <- left_join(AvgStr2021, Avg_Str_22)
Avg_Str_all <- left_join(AvgStr2020, Avg_Str_21)

Avg_Str <- Avg_Str_all %>% drop_na()

Avg_Str_year <- Avg_Str %>%
  rename("2020" = "mean_Str2020", "2021" = "mean_Str2021", "2022" = "mean_Str2022")

Avg_Str_country <- Avg_Str_year %>%
  pivot_longer (c("2020", "2021", "2022"), names_to = "year", values_to = "stringency")


#Avg_Str_country %>% ggplot(aes(year, stringency, col = country_code)) + geom_point(show.legend = FALSE) + facet_wrap(~country_code)

happy_2015 <- read.csv("2015.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2016 <- read.csv("2016.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2017 <- read.csv("2017.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2018 <- read.csv("2018.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2019 <- read.csv("2019.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2020 <- read.csv("2020.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2021 <- read.csv("2021.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2022 <- read.csv("2022.csv", header = TRUE, sep = ",", na.string = "NA", dec = ",")


only_happy_2015 <- happy_2015%>%
  select(Country, Happiness.Score) %>%
  rename(country_name = Country, Happy2015 = Happiness.Score)%>%
  arrange(country_name)
allStr_h2015 <- merge(Avg_Str_all, only_happy_2015,by="country_name")

only_happy_2016 <- happy_2016%>%
  select(Country, Happiness.Score) %>%
  rename(country_name = Country, Happy2016 = Happiness.Score)%>%
  arrange(country_name)
allStr_h2016 <- merge(allStr_h2015, only_happy_2016,by="country_name")

only_happy_2017 <- happy_2017%>%
  select(Country, Happiness.Score) %>%
  rename(country_name = Country, Happy2017 = Happiness.Score)%>%
  arrange(country_name)
allStr_h2017 <- merge(allStr_h2016, only_happy_2017,by="country_name")

only_happy_2018 <- happy_2018%>%
  select(Country.or.region, Score) %>%
  rename(country_name = Country.or.region, Happy2018 = Score)%>%
  arrange(country_name)
allStr_h2018 <- merge(allStr_h2017, only_happy_2018,by="country_name")

only_happy_2019 <- happy_2019%>%
  select(Country.or.region, Score) %>%
  rename(country_name = Country.or.region, Happy2019 = Score)%>%
  arrange(country_name)
allStr_h2019 <- merge(allStr_h2018, only_happy_2019,by="country_name")
avg_pre_pandemic <- allStr_h2019 %>%
  mutate(total_pre_pandemic_happiness = Happy2015 + Happy2016 + Happy2017 + Happy2018 + Happy2019) %>%
  mutate(avg_pre_pandemic_happiness = total_pre_pandemic_happiness/5)

only_happy_2020 <- happy_2020%>%
  select(Country.name, Ladder.score) %>%
  rename(country_name = Country.name, Happy2020 = Ladder.score)%>%
  arrange(country_name)
allStr_h19_20 <- merge(avg_pre_pandemic, only_happy_2020,by="country_name")

only_happy_2021 <- happy_2021%>%
  select(Country.name, Ladder.score, Regional.indicator) %>%
  rename(country_name = Country.name, Happy2021 = Ladder.score)%>%
  arrange(country_name)
allStr_h19_20_21 <- merge(allStr_h19_20, only_happy_2021,by="country_name")

only_happy_2022 <- happy_2022%>%
  select(Country, Happiness.score) %>%
  rename(country_name = Country, Happy2022 = Happiness.score)%>%
  arrange(country_name)
allStr_allHap <- merge(allStr_h19_20_21, only_happy_2022,by="country_name")

allStr_allHap %>% ggplot(aes(x = mean_Str2020, y = Happy2020, col = 2)) + geom_point(show.legend = FALSE)
allStr_allHap %>% ggplot(aes(mean_Str2021, Happy2021, col = "RED")) + geom_point(show.legend = FALSE)
allStr_allHap %>% ggplot(aes(mean_Str2022, Happy2022, col = "GREEN")) + geom_point(show.legend = FALSE)

Happy_Country_Year_all <- allStr_allHap %>%
  #mutate(Happy2020=Happy2020-avg_pre_pandemic_happiness) %>%
  #mutate(Happy2021=Happy2021-avg_pre_pandemic_happiness) %>%
  #mutate(Happy2022=Happy2022-avg_pre_pandemic_happiness) %>%
  #mutate(avg_pre_pandemic_happiness=avg_pre_pandemic_happiness-avg_pre_pandemic_happiness) %>%
  rename("avg_pre_pandemic_happiness" = avg_pre_pandemic_happiness, "2015"= Happy2015, "2016"= Happy2016, "2017"= Happy2017, "2018"= Happy2018, "2019"= Happy2019, "2020" = Happy2020, "2021" = Happy2021, "2022" = Happy2022, "Region" = Regional.indicator) %>%
  select(country_name, country_code, "avg_pre_pandemic_happiness", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "Region") 

Happy_Country_Year_all <- Happy_Country_Year_all %>%
  pivot_longer(c("avg_pre_pandemic_happiness", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"), names_to = "year", values_to = "Happiness") #%>%filter (year != "avg_pre_pandemic_happiness")

Happy_Country_Year_all %>%
  group_by(country_code) %>%
  ggplot(aes(x = year, y = Happiness, col = 2)) +
  geom_point(show.legend = FALSE) #+ facet_wrap(~country_code)

Happy_Country_Year_all %>%
  group_by(country_code) %>%
  ggplot(aes(x = year, y = Happiness, col = 2)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~Region)

#lmer_model <-  lmer(Happiness ~ (year | country_code), data = Happy_Country_Year_all)


Stringency_Country_Year_all <- allStr_allHap %>%
  rename("2020" = mean_Str2020, "2021" = mean_Str2021, "2022" = mean_Str2022, "Region" = Regional.indicator) %>%
  select(country_name, country_code, "2020", "2021", "2022", "Region") %>%
  pivot_longer (c("2020", "2021", "2022"), names_to = "year", values_to = "Stringency")
#head(Stringency_Country_Year_all)
Stringency_Country_Year_all %>%
  group_by(country_code) %>%
  ggplot(aes(x = year, y = Stringency, col = 2)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~country_name)

Stringency_Country_Year_all %>%
  group_by(country_code) %>%
  ggplot(aes(x = year, y = Stringency, col = 2)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~Region)


covid_data <- read.csv("owid-covid-data.csv")
#covid_data_china <- covid_data %>% filter(location == "China")
#View(covid_data)


covid_data <- covid_data %>%
  select(c(location, total_cases_per_million, total_deaths_per_million, date)) #hosp_patients, total_boosters, icu_patients))


covid_data_grouped <- covid_data %>% 
  group_by(location) %>%
  arrange(date)
#str(covid_data_grouped$date)
covid_data_grouped$date <- as.Date(covid_data_grouped$date)
str(covid_data_grouped$date)
covid_data_grouped <- covid_data_grouped %>%
  group_by(location) %>%
  mutate(days = date - first(date) + 1)

covid_data_2020_total <- covid_data_grouped[covid_data_grouped$date == '2020-12-31', ]
covid_data_2021_total <- covid_data_grouped[covid_data_grouped$date == '2021-12-31', ]
covid_data_2022_total <- covid_data_grouped[covid_data_grouped$date == '2022-12-31', ]


covid_data_2019_total <- covid_data_2020_total %>%
  select(location)
covid_data_2019_total$cases_2019 <- 0
covid_data_2019_total$deaths_2019 <- 0

covid_data_2020_total <- covid_data_2020_total %>%
  rename(cases_2020 = total_cases_per_million, deaths_2020 = total_deaths_per_million) %>%
  select(-c(date, days))

covid_data_2021_total <- covid_data_2021_total %>%
  rename(cases_2021 = total_cases_per_million, deaths_2021 = total_deaths_per_million) %>%
  select(-c(date, days))

covid_data_2022_total <- covid_data_2022_total %>%
  rename(cases_2022 = total_cases_per_million, deaths_2022 = total_deaths_per_million) %>%
  select(-c(date, days))

#head(covid_data_2022_total)

covid_deaths_total <- left_join(covid_data_2019_total,covid_data_2020_total, by="location")
covid_deaths_total <- left_join(covid_deaths_total, covid_data_2021_total, by="location")
covid_deaths_total <- left_join(covid_deaths_total, covid_data_2022_total, by="location")


#View(covid_deaths_total)


covid_deaths_total <- covid_deaths_total %>%
  mutate(CasesPerMillion_2020 = cases_2020) %>%
  mutate(DeathsPerMillion_2020 = deaths_2020) %>%
  mutate(CasesPerMillion_2021 = cases_2021 - cases_2020) %>%
  mutate(DeathsPerMillion_2021 = deaths_2021 - deaths_2020) %>%
  mutate(CasesPerMillion_2022 = cases_2022 - cases_2021) %>%
  mutate(DeathsPerMillion_2022 = deaths_2022 - deaths_2021)

covid_deaths_total["CasesPerMillion_2021"][is.na(covid_deaths_total["CasesPerMillion_2021"])] <- 0
covid_deaths_total["CasesPerMillion_2022"][is.na(covid_deaths_total["CasesPerMillion_2022"])] <- 0
covid_deaths_total["DeathsPerMillion_2021"][is.na(covid_deaths_total["DeathsPerMillion_2021"])] <- 0
covid_deaths_total["DeathsPerMillion_2022"][is.na(covid_deaths_total["DeathsPerMillion_2022"])] <- 0

colnames(covid_deaths_total)
#install.packages("dbplyr")
library(dbplyr)

#covid_deaths_total <- covid_deaths_total %>% relocate(location, year.x, year.y, year)
#head(covid_deaths_total)

covid_deaths_cleaned <- covid_deaths_total %>%
  pivot_longer(
    cols = !location,
    names_to = c("type", "year"),
    names_sep = "_",
    values_to = "score"
  ) %>%
  pivot_wider(
    names_from = type, 
    values_from = score
  )

covid_cases_and_deaths <- covid_deaths_cleaned %>%
  rename(country_name = location)
#head(covid_deaths_cleaned)


total_data <- full_join(Stringency_Country_Year_all, covid_cases_and_deaths, by=c("country_name", "year"))
total_data <- full_join(Happy_Country_Year_all, total_data, by=c("country_name", "year"))
#head(total_data)


total_data <- total_data%>%
  rename("country_code" = country_code.x, "Region" = Region.x) %>%
  select(-country_code.y, -Region.y)

total_data["CasesPerMillion"][is.na(total_data["CasesPerMillion"])] <- 0
total_data["DeathsPerMillion"][is.na(total_data["DeathsPerMillion"])] <- 0
total_data["Stringency"][is.na(total_data["Stringency"])] <- 0
total_data["cases"][is.na(total_data["cases"])] <- 0
total_data["deaths"][is.na(total_data["deaths"])] <- 0


total_data <- total_data %>% drop_na(Happiness)

View(total_data)

plot1 <- total_data %>%
  ggplot(mapping=aes(x=year, y=Happiness)) +
  geom_col(aes(colour=country_code))
plot1

#total_data_just_pandemic<- total_data %>% filter (year != c("avg_pre_pandemic_happiness", "2015", "2016", "2017", "2018", "2019"))

hap_string <- lm(Happiness ~ Stringency, data = total_data)
summary(hap_string)

happiness_by_stringency <- total_data %>%
  ggplot(aes(x = Stringency, y = Happiness)) +
  geom_point() + #(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  labs(title="Differences in Stringency vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')
happiness_by_stringency
hap_death <- lm(Happiness ~ DeathsPerMillion, data = total_data)
summary(hap_death)

happiness_by_deaths <- total_data %>%
  ggplot(aes(x = DeathsPerMillion, y = Happiness))+ #, col=deathsdiff)) + 
  geom_point() + #(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  labs(title="Differences in Total Covid Deaths vs. Happiness", subtitle="total deaths per million per year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')
happiness_by_deaths

hap_string_death <- lm(Happiness ~ Stringency * DeathsPerMillion, data = total_data)
summary(total_data)
summary(hap_string_death)

hapsting_by_deaths <- total_data %>%
  ggplot(aes(x = Stringency, y = Happiness, col=DeathsPerMillion)) + 
  geom_point() + #(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  labs(title="Differences in Stringency vs. Happiness", subtitle="by total deaths per million per year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')
hapsting_by_deaths

hap_cases <- lm(Happiness ~ CasesPerMillion, data = total_data)
summary(hap_cases)

happiness_by_cases <- total_data %>%
  ggplot(aes(x = CasesPerMillion, y = Happiness)) + #, col=deathsdiff)) + 
  geom_point() + ##(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  labs(title="Differences in Casedifferences vs. Happiness", subtitle="Differences in total cases per million per year between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')
happiness_by_cases

hap_string_cases <- lm(Happiness ~ Stringency * CasesPerMillion, data = total_data)
summary(hap_string_cases)

hap_string_cases_plot <- total_data %>%
  ggplot(aes(x = Stringency, y = Happiness, col=CasesPerMillion)) + 
  geom_point() + ##(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  labs(title="Differences in Stringency vs. Happiness", subtitle="total cases per million per year: differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')

hap_string_cases_plot

year_2020 <- total_data %>%
  filter(year == "2020")
year_2021 <- total_data %>%
  filter(year == "2021")
year_2022 <- total_data %>%
  filter(year == "2022")

model_2020 <- lm(Happiness ~ Stringency, data=year_2020)
summary(model_2020)
model_2021 <- lm(Happiness ~ Stringency, data=year_2021)
summary(model_2021)
model_2022 <- lm(Happiness ~ Stringency, data=year_2022)
summary(model_2022)


plot_2020 <- year_2020 %>%
  ggplot(aes(x = Stringency, y = Happiness)) + #, col=casediff)) + 
  geom_point() + ##(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Deathrates vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')

plot_2021 <- year_2021 %>%
  ggplot(aes(x = Stringency, y = Happiness)) + #, col=casediff)) + 
  geom_point() + ##(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Deathrates vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')

plot_2022 <- year_2022 %>%
  ggplot(aes(x = Stringency, y = Happiness))+ #, col=casediff)) + 
  geom_point() + #(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Deathrates vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')

plot_2020
plot_2021
plot_2022

###########################################################################################################

#total_data %>%
  #ggplot(aes(x = Stringency, y = Happiness, label = country_code, colour = Region)) +
  #geom_label_repel(aes(label = country_code), size = 3, box.padding = 0, label.padding = 0, max.overlaps = getOption("ggrepel.max.overlaps", default = 60), segment.color = 'grey') +
  #geom_point() + facet_wrap(~year, scale = "fixed")

total_data %>%
  ggplot(aes(x = Stringency, y = Happiness, label = country_code, colour = Region)) +
  stat_smooth(method = "lm", se=FALSE) +
  geom_text_repel(aes(label = country_code), size = 3, 
                   segment.color = 'grey') +
  geom_point(aes(size = DeathsPerMillion)) +
  facet_wrap(~year, scale = "fixed") + 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "grey95"), 
        panel.grid.major = element_line(color = 'grey90', 
                                        linetype = 'dotted')) + 
  labs(title="Stringency vs. Happiness across different world regions between 2020-2022", 
       subtitle="accounting for death rates per year") # + theme_gray(base_size = 30)

total_data %>%
  ggplot(aes(x = Stringency, y = Happiness, label = country_code, colour = Region)) +
  geom_text_repel(aes(label = country_code), size = 3, 
                  segment.color = 'grey') +
  geom_point(aes(size = DeathsPerMillion))+
  theme_bw() +
  facet_wrap(~ Region * year) + 
  stat_smooth(method = "lm", se=FALSE) + 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "grey95"), 
        panel.grid.major = element_line(color = 'grey90', linetype = 'dotted')) + 
  labs(title="Stringency vs. Happiness across different world regions between 2020-2022", 
       subtitle="accounting for death rates per year") # + theme_gray(base_size = 30)



total_data$Continent <- total_data$Region

total_new <- + total_data %>%
  mutate(Continent = case_when(Region == "Sub-Saharan Africa" ~ "Africa", TRUE~Continent)) %>%
  mutate(Continent = case_when(Region == "Middle East and North Africa" ~ "Africa", TRUE ~Continent)) %>%
  mutate(Continent = case_when(Region == "South Asia" ~ "Asia", TRUE ~Continent))  %>%
  mutate(Continent = case_when(Region == "Central and Eastern Europe" ~ "Europe", TRUE ~Continent))  %>%
  mutate(Continent = case_when(Region == "Commonwealth of Independent States" ~ "Asia", TRUE ~Continent)) %>%
  mutate(Continent = case_when(Region == "East Asia" ~ "Asia", TRUE ~Continent)) %>%
  mutate(Continent = case_when(Region == "Latin America and Caribbean" ~ "South America", TRUE ~Continent)) %>%
  mutate(Continent = case_when(Region == "North America and ANZ" ~ "North America", TRUE ~Continent)) %>%
  mutate(Continent = case_when(Region == "Southeast Asia" ~ "Asia", TRUE ~Continent)) %>%
  mutate(Continent = case_when(Region == "Western Europe" ~ "Europe", TRUE ~Continent)) 

total_new %>%
  ggplot(aes(x = Stringency, y = Happiness, label = country_code, colour = Continent)) +
  geom_point(aes(size = deathsdiff))+
  theme_minimal()+
  facet_wrap(~ Continent * year, ncol = 3) + 
  stat_smooth(method = "lm", se=FALSE) + 
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "grey95"), 
        panel.grid.major = element_line(color = 'grey90', 
                                        linetype = 'dotted')) + 
  labs(title="Stringency vs. Happiness across continents between 2020-2022", 
       subtitle="accounting for death rates per year") #+ theme_gray(base_size = 30)