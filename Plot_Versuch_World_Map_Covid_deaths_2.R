# create data for world coordinates using map_data() function
world <- map_data("world")

# read volcano_eruption data from volcano.csv
covid_deaths <- readr::read_csv("owid-covid-data.csv")

head(covid_deaths)
# create world map using ggplot() function
ggplot() +
  # geom_map() function takes world coordinates as input
  # to plot world map color parameter determines the
  # color of borders in map fill parameter determines the
  # color of fill in map size determines the thickness of
  # border in map
  geom_map(
    data = covid_deaths, map = world,
    aes(long, lat, map_id = region),
    color = "green", fill= "lightyellow"
  )+
  # geom_point function is used to plot scatter plot on top 
  # of world map
  geom_point(
    data = covid_deaths,
    aes(longitude, latitude, color = total_cases_per_million,
        size=population_within_10_km),
    alpha = 1) +
  
  # legend.position as none removes the legend
  theme(legend.position="none")

world_coordinates <- map_data("world")

# create world map using ggplot() function
ggplot() +
  
  # geom_map() function takes world coordinates as input 
  # to plot world map color parameter determines the 
  # color of borders in map fill parameter determines 
  # the color of fill in map size determines the thickness
  # of border in map
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region),    color = "white", fill = "blue", size = 0.2) + labs(title="Covid-19 deaths world-wide", subtitle = 2020)
