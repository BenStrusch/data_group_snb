library(tidyverse)
library(ggthemes)
library(distill)
library(rmarkdown)
library(lme4)
library(readxl)
library(dplyr)
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)

#read Oxford stringency alltimes data xls file
OxCGRTall <- read_excel("OxCGRT_timeseries_all.xlsx")
head(OxCGRTall)
colnames(OxCGRTall)

#filter for total country to get rid of states
OxCGRT_country <- OxCGRTall %>%
  filter (jurisdiction == "NAT_TOTAL")
head(OxCGRT_country)

#delete unnecessary columns
OxCGRT <- OxCGRT_country %>%
  select (-region_code, -region_name, -jurisdiction, -c(contains("2023")))

#df by year
Ox2020 <- OxCGRT %>%
  select (country_code, country_name, c(contains("2020")))
Ox2021 <- OxCGRT %>%
  select (country_code, country_name, c(contains("2021")))
Ox2022 <- OxCGRT %>%
  select (country_code, country_name, c(contains("2022")))


#transpose by year
Ox2020_Str <- pivot_longer(Ox2020, cols = c(contains("2020")), names_to = "date", values_to = "stringency")
Ox2021_Str <- pivot_longer(Ox2021, cols = c(contains("2021")), names_to = "date", values_to = "stringency")
Ox2022_Str <- pivot_longer(Ox2022, cols = c(contains("2022")), names_to = "date", values_to = "stringency")



#calculate mean stringency by year and country
AvgStr2020 <- Ox2020_Str %>%
  group_by(country_name) %>%
  summarize(mean_Str2020 = mean(stringency))
AvgStr2021 <- Ox2021_Str %>%
  group_by(country_name) %>%
  summarize(mean_Str2021 = mean(stringency))
AvgStr2022 <- Ox2022_Str %>%
  group_by(country_name) %>%
  summarize(mean_Str2022 = mean(stringency))

#combine to one table incl country_code
Avg_Str_0 <- OxCGRT_country %>%
  select(country_code, country_name)
Avg_Str_22 <- left_join(AvgStr2022, Avg_Str_0)
Avg_Str_21 <- left_join(AvgStr2021, Avg_Str_22)
Avg_Str_all <- left_join(AvgStr2020, Avg_Str_21)

head(Avg_Str_0) ### was wolltest Du damit machen?

#delete all countries with stringency NA
Avg_Str <- Avg_Str_all %>% drop_na()
head(Avg_Str)

#rename and add Stringency "0" for 2019
Avg_Str_year <- Avg_Str %>%
  rename("2020" = "mean_Str2020", "2021" = "mean_Str2021", "2022" = "mean_Str2022")
Avg_Str_year$"2019"[1:181] <- 0 ### keine Spalte mit 2019

#transpose
Avg_Str_country <- Avg_Str_year %>%
  pivot_longer (c("2019", "2020", "2021", "2022"), names_to = "year", values_to = "stringency")

#visualizing stringency per country 2019-2022
Avg_Str_country %>%
  ggplot(aes(year, stringency, col = country_code)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~country_code)

#load happiness scores
happy_2019 <- read.csv("2019.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2020 <- read.csv("2020.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2021 <- read.csv("2021.csv", header = TRUE, sep = ",", na.string = "NA", dec = ".")
happy_2022 <- read.csv("2022.csv", header = TRUE, sep = ",", na.string = "NA", dec = ",")


#combine happiness with Stringency
#Str all + 2019
only_happy_2019 <- happy_2019%>%
  select(Country.or.region, Score) %>%
  rename(country_name = Country.or.region, Happy2019 = Score)%>%
  arrange(country_name)
allStr_h2019 <- merge(Avg_Str_all, only_happy_2019,by="country_name")
#Str all + 2019 + 2020
only_happy_2020 <- happy_2020%>%
  select(Country.name, Ladder.score) %>%
  rename(country_name = Country.name, Happy2020 = Ladder.score)%>%
  arrange(country_name)
allStr_h19_20 <- merge(allStr_h2019, only_happy_2020,by="country_name")
#Str all + 2019 + 2020 + 2021
only_happy_2021 <- happy_2021%>%
  select(Country.name, Ladder.score) %>%
  rename(country_name = Country.name, Happy2021 = Ladder.score)%>%
  arrange(country_name)
allStr_h19_20_21 <- merge(allStr_h19_20, only_happy_2021,by="country_name")
#Str all + 2019 + 2020 + 2021 + 2022
only_happy_2022 <- happy_2022%>%
  select(Country, Happiness.score) %>%
  rename(country_name = Country, Happy2022 = Happiness.score)%>%
  arrange(country_name)
allStr_allHap <- merge(allStr_h19_20_21, only_happy_2022,by="country_name")
head(allStr_allHap)
#visualizing Stringency vs Happiness per year 2020-2022
allStr_allHap %>%
  ggplot(aes(x = mean_Str2020, y = Happy2020, col = 2)) +
  geom_point(show.legend = FALSE)
allStr_allHap %>%
  ggplot(aes(mean_Str2021, Happy2021, col = "RED")) +
  geom_point(show.legend = FALSE)
allStr_allHap %>%
  ggplot(aes(mean_Str2022, Happy2022, col = "GREEN")) +
  geom_point(show.legend = FALSE)

#visualizing Happiness per country over time 2019-2022
#transpose first
Happy_Country_Year_all <- allStr_allHap %>%
  rename("2019" = Happy2019, "2020" = Happy2020, "2021" = Happy2021, "2022" = Happy2022) %>%
  select(country_name, country_code, "2019", "2020", "2021", "2022") %>%
  pivot_longer(c("2019", "2020", "2021", "2022"), names_to = "year", values_to = "Happiness")

Happy_Country_Year_all %>%
  group_by(country_code) %>%
  ggplot(aes(x = year, y = Happiness, col = 2)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~country_name)

#visualizing Stringency per country over time 2020-2022
#transpose first
Stringency_Country_Year_all <- allStr_allHap %>%
  rename("2020" = mean_Str2020, "2021" = mean_Str2021, "2022" = mean_Str2022) %>%
  select(country_name, country_code, "2020", "2021", "2022") %>%
  pivot_longer (c("2020", "2021", "2022"), names_to = "year", values_to = "Stringency")
#head(Stringency_Country_Year_all)
Stringency_Country_Year_all %>%
  group_by(country_code) %>%
  ggplot(aes(x = year, y = Stringency, col = 2)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~country_name)

######################################################################
###################### Covid Death Data ##############################
######################################################################

getwd()
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

#View(covid_data_grouped)

covid_data_2020_total <- covid_data_grouped[covid_data_grouped$date == '2020-12-31', ]
covid_data_2021_total <- covid_data_grouped[covid_data_grouped$date == '2021-12-31', ]
covid_data_2022_total <- covid_data_grouped[covid_data_grouped$date == '2022-12-31', ]

#View(covid_data_2020_total)

#year_2020 <- 2020
#year_2021 <- 2021
#year_2022 <- 2022

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
  mutate(casediff_2020 = cases_2020) %>%
  mutate(deathsdiff_2020 = deaths_2020) %>%
  mutate(casediff_2021 = cases_2021 - cases_2020) %>%
  mutate(deathsdiff_2021 = deaths_2021 - deaths_2020) %>%
  mutate(casediff_2022 = cases_2022 - cases_2021) %>%
  mutate(deathsdiff_2022 = deaths_2022 - deaths_2021)

covid_deaths_total["casediff_2021"][is.na(covid_deaths_total["casediff_2021"])] <- 0
covid_deaths_total["casediff_2022"][is.na(covid_deaths_total["casediff_2022"])] <- 0
covid_deaths_total["deathsdiff_2021"][is.na(covid_deaths_total["deathsdiff_2021"])] <- 0
covid_deaths_total["deathsdiff_2022"][is.na(covid_deaths_total["deathsdiff_2022"])] <- 0

colnames(covid_deaths_total)
#install.packages("dbplyr")
#library(dbplyr)

#covid_deaths_total <- covid_deaths_total %>%
  #relocate(location, year.x, year.y, year)
head(covid_deaths_total)

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
#head(covid_deaths_cleaned)
  
#head(Stringency_Country_Year_all)
#head(covid_deaths_total)

covid_cases_and_deaths <- covid_deaths_cleaned %>%
  rename(country_name = location)
#head(covid_deaths_cleaned)

total_data <- full_join(Stringency_Country_Year_all, covid_cases_and_deaths, by=c("country_name", "year"))
total_data <- full_join(Happy_Country_Year_all, total_data, by=c("country_name", "year"))
#head(total_data)


total_data <- total_data[,-5]

View(total_data)

total_data <- total_data %>%
  rename(country_code = country_code.x)

total_data["casediff"][is.na(total_data["casediff"])] <- 0
total_data["deathsdiff"][is.na(total_data["deathsdiff"])] <- 0

View(total_data)
library(tidyr)

total_data_wo_na <- total_data %>%
  drop_na(Happiness)

View(total_data_wo_na)
total_data_wo_na_2019 <- total_data_wo_na %>%
  filter(year == c("2020", "2021", "2022"))

View(total_data_wo_na_2019)


happiness_by_stringency <- total_data_wo_na_2019 %>%
  ggplot(aes(x = Stringency, y = Happiness, col=deathsdiff)) + 
  geom_point() + #(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Stringency vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')

happiness_by_stringency

hap_string_death <- lm(Happiness ~ Stringency + deathsdiff, data = total_data_wo_na)
summary(hap_string_death)

happiness_by_deaths <- total_data_wo_na_2019 %>%
  ggplot(aes(x = deathsdiff, y = Happiness))+ #, col=deathsdiff)) + 
  geom_point() + #(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Total Covid Deaths vs. Happiness", subtitle="total deaths per year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')
happiness_by_deaths

year_2020 <- total_data_wo_na_2019 %>%
  filter(year == "2020")
year_2021 <- total_data_wo_na_2019 %>%
  filter(year == "2021")
year_2022 <- total_data_wo_na_2019 %>%
  filter(year == "2022")

model_2020 <- lm(Happiness ~ Stringency, data=year_2020)
summary(model_2020)
model_2021 <- lm(Happiness ~ Stringency, data=year_2021)
summary(model_2021)
model_2022 <- lm(Happiness ~ Stringency, data=year_2022)
summary(model_2022)


year_2022_desc <- year_2022 %>%
  arrange(desc(deathsdiff)) %>%
  filter(deathsdiff > 10)

year_2022_desc <- year_2022_desc %>%
  arrange(desc(casediff))
View(year_2022_desc)

plot_2020 <- year_2020 %>%
  ggplot(aes(x = Stringency, y = Happiness)) + #, col=casediff)) + 
  geom_point() + ##(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Deathrates vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')
plot_2020
plot_2021 <- year_2021 %>%
  ggplot(aes(x = Stringency, y = Happiness)) + #, col=casediff)) + 
  geom_point() + ##(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Deathrates vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')
plot_2021

plot_2022 <- year_2022 %>%
  ggplot(aes(x = Stringency, y = Happiness))+ #, col=casediff)) + 
  geom_point() + #(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Deathrates vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')

plot_2022

plot_2022_deaths <- year_2022 %>%
  ggplot(aes(x = deathsdiff, y = Happiness))+ #, col=casediff)) + 
  geom_point() + #(aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Deathrates vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')

plot_2022_deaths

plot_year_2022_desc <- year_2022_desc %>%
  ggplot(aes(x = Stringency, y = Happiness))+ #, col=casediff)) + 
  geom_point() +# (aes(size=deathsdiff)) +
  theme_minimal() +
  scale_colour_gradientn(colors=rainbow(7)) +
  #facet_wrap(~year, scales = "free") +
  labs(title="Differences in Deathrates vs. Happiness", subtitle="Differences between presented and previous year from 2020 - 2022") +
  theme(legend.position = "bottom", legend.text = element_text(size = 5)) +
  facet_wrap(~year, scale = "free") + stat_smooth(method = 'lm')

plot_year_2022_desc

model_2022 <- lm(Happiness ~ Stringency + deathsdiff, data=year_2022_desc)
summary(model_2022)

View(total_data)

View(china_and_switzerland)

model1_hapstring <- lm(Happiness ~ Stringency, data=total_data)
model2_hapcasediff <- lm(Happiness ~ casediff * deathsdiff, data=total_data)

summary(model1_hapstring)
summary(model2_hapcasediff)


#lmermodell <- lmer(Happiness ~ 1 + Stringency + (1 + Stringency|country_name), data=total_data_wo_na_2019)
#summary(lmermodell)

#############################################################
############# Resource for Covid Data #######################
#############################################################


#Edouard Mathieu, Hannah Ritchie, Lucas Rodés-Guirao, Cameron Appel, Charlie Giattino, Joe Hasell, Bobbie Macdonald, Saloni Dattani, Diana Beltekian, Esteban Ortiz-Ospina and Max Roser (2020) - "Coronavirus Pandemic (COVID-19)". Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/coronavirus' [Online Resource]
