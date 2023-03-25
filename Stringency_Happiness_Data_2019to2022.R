
library(tidyverse)
library(ggthemes)
library(distill)
library(rmarkdown)
library(lme4)
library(readxl)
library(dplyr)

#read Oxford stringency alltimes data xls file
OxCGRTall <- read_excel("OxCGRT_timeseries_all.xlsx")

#filter for total country to get rid of states
OxCGRT_country <- OxCGRTall %>%
  filter (jurisdiction == "NAT_TOTAL")

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

#delete all countries with stringency NA
Avg_Str <- Avg_Str_all %>% drop_na()

#rename and add Stringency "0" for 2019
Avg_Str_year <- Avg_Str %>%
  rename("2020" = "mean_Str2020", "2021" = "mean_Str2021", "2022" = "mean_Str2022")
Avg_Str_year$"2019"[1:181] <- 0

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
  select(ï..Country.name, Ladder.score) %>%
  rename(country_name = ï..Country.name, Happy2021 = Ladder.score)%>%
  arrange(country_name)
allStr_h19_20_21 <- merge(allStr_h19_20, only_happy_2021,by="country_name")
#Str all + 2019 + 2020 + 2021 + 2022
only_happy_2022 <- happy_2022%>%
  select(Country, Happiness.score) %>%
  rename(country_name = Country, Happy2022 = Happiness.score)%>%
  arrange(country_name)
allStr_allHap <- merge(allStr_h19_20_21, only_happy_2022,by="country_name")

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
  pivot_longer (c("2019", "2020", "2021", "2022"), names_to = "year", values_to = "Happiness")

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

Stringency_Country_Year_all %>%
  group_by(country_code) %>%
  ggplot(aes(x = year, y = Stringency, col = 2)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~country_name)
