library(data.table)
library(tidyverse)
library(ggmap)
library(lubridate)
library(stringr)

pokemon = fread('300k.csv')

#sara data
filtered_sara = select(pokemon, pokemonId, latitude, longitude, 
                  appearedLocalTime, terrainType,
                  closeToWater, city, weather, temperature,
                  windSpeed, windBearing, pressure, weatherIcon,
                  population_density,
                  population_density,
                  gymDistanceKm,
                  gymIn100m,
                  gymIn250m,
                  pokestopDistanceKm) %>% as.data.frame()

#pokemon classes
sara = read.csv('pokemon_classes.csv', header = TRUE)

#join classes and stats with data
sarita = left_join(filtered_sara, sara, by = 'pokemonId')

#date wrangling
sarita = sarita %>% mutate(pokedate = ymd_hms(str_replace(appearedLocalTime,'T',' ')))

sarita = sarita %>% mutate(poketime = hour(pokedate) + minute(pokedate)/60)

#add variable for hour intervals
sarita$hour_interval = cut(sarita$poketime, 
                                breaks = seq(from = 0, to = 24, by = 2),
                                include.lowest = T, right = F)

#get countries
sarita$countries = coords2country(data.frame(sarita$longitude,sarita$latitude))

#filter USA
usa = sarita %>% filter(countries == 'United States of America')

map_usa = get_map(location = 'USA', zoom = 4, color = 'bw', source = 'google')

ggmap(map_usa) + 
        geom_point(data = usa, aes(x = longitude, y = latitude, 
                                  color = closeToWater))
#heat map
usa_map = ggmap(map_usa, base_layer = ggplot(aes(x = longitude, y = latitude), 
                                               data = sarita))
usa_map +
        stat_density2d(aes(x = longitude, y = latitude, fill = ..level.., 
                           alpha = ..level..), bins = 70, geom = 'polygon', 
                       data = usa) +
        scale_fill_gradient(low = 'gold', high = 'red', guide = F) +
        guides(alpha = F)

#filter Chile
chile = sarita %>% filter(countries == 'Chile')

map_chile = get_map(location = 'Chile', zoom = 6, color = 'bw', 
                  source = 'google')

ggmap(map_chile) + 
        geom_point(data = chile, aes(x = longitude, y = latitude, 
                                   color = closeToWater))

#boxplot
ggplot(data = sarita, aes(x = first_type, y = total_stats)) +
        geom_boxplot()

#jitter
ggplot(data = sarita, aes(x = first_type, y = total_stats, color = closeToWater)) +
        geom_jitter(width = 0.5)

#hour interval
ggplot(data = sarita, aes(x = hour_interval, y = total_stats, color = first_type)) +
        geom_jitter(width = 0.5)


ggplot(data = sarita, aes(x = first_type, y = poketime)) +
        geom_jitter()

#plot terrainType vs first_type
ggplot(data = sarita, aes(x = first_type, y = terrainType, color = closeToWater)) +
        geom_jitter()

#bar plots
ggplot(data = sarita, aes(first_type)) + geom_bar()

#to change plot order of bars, change levels in underlying factor
reorder_size = function(x) {
        factor(x, levels = names(sort(table(x), decreasing = T)))
}

ggplot(data = sarita, aes(reorder_size(first_type))) + 
        geom_bar(aes(fill = closeToWater), position = 'fill') +
        xlab('pokemon type')

ggplot(data = sarita, aes(reorder_size(first_type))) + 
        geom_bar()

ggplot(data = sarita, aes(x = first_type, y = total_stats)) + 
        geom_boxplot()
