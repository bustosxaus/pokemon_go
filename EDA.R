library(data.table)
library(dplyr)
pokemon = fread("300k.csv")

data = select(pokemon, pokemonId, latitude, longitude, 
	appearedLocalTime, terrainType,
	closeToWater, city, weather, temperature,
	windSpeed, windBearing, pressure, weatherIcon,
	population_density, sunriseHour, sunriseMinute,
	sunsetHour, sunsetMinute, population_density,
	urban, suburban, midurban, rural, gymDistanceKm,
	pokestopDistanceKm)

write.csv(data, "filtered_pokemon.csv")


new_york = filter(data, city == "New_York")



# night or day
# morning, afternoon, night, late night


# weather

# logistic- pokemonId or class

# poisson - number per hour