library(data.table)
library(dplyr)
## CLEANING
source("coords2country.R")
pokemon = fread("300k.csv")

filtered = dplyr::select(pokemon, 
    pokemonId, 
    latitude, longitude, 
    appearedLocalTime, 
    terrainType,
    closeToWater, 
    temperature,
    weatherIcon,
    population_density,
    rural,
    urban,
    suburban,
    midurban,
    gymDistanceKm,
    gymIn100m,
    gymIn250m,
    pokestopIn100m,
    pokestopDistanceKm) %>% as.data.frame()

# pokemon classes
details = read.csv("pokemon_classes.csv", header = TRUE)

# join classes and stats with data
data = left_join(filtered, details, by = "pokemonId")

# water
data$water = 0
data$water[data$first_type == "Water"] = 1

# bug
data$bug = 0
data$bug[data$first_type == "Bug"] = 1

# pyschic
data$psychic = 0
data$psychic[data$first_type == "Psychic"] = 1

# poision
data$poison = 0
data$poison[data$first_type == "Poison"] = 1

# fighting
data$normal = 0
data$normal[data$first_type == "Normal"] = 1


data$high_stats = 0
data$high_stats[data$total_stats > 250] = 1


data$grouped_type = data$first_type 
data$grouped_type = as.character(data$first_type)
data$grouped_type[!data$grouped_type %in% 
    c("Poison", "Normal", "Water", "Bug") ] = "Other"

## DENSITY
data$type_density = "mid"
data$type_density[data$population_density > 800] = "urban"
data$type_density[data$population_density < 200] = "rural"

#get countries
data$country = coords2country(data.frame(data$longitude,data$latitude))
data$country = as.character(data$country)

# remove countries that didn't work
data = data[is.na(data$country) == FALSE,]

# grouped terrain types
data$terrain_grouped  = "water"
data$terrain_grouped[data$terrainType %in% c(1,2,4,5)] = "forest"
data$terrain_grouped[data$terrainType %in% c(7,8,9,16)] = "savanna"
data$terrain_grouped[data$terrainType %in% c(10,11,12)] = "grassland"
data$terrain_grouped[data$terrainType %in% c(13)] = "urban"

