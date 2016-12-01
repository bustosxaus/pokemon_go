library(data.table)
library(dplyr)
pokemon = fread("300k.csv")

filtered = select(pokemon, pokemonId, latitude, longitude, 
	appearedLocalTime, terrainType,
	closeToWater, city, weather, temperature,
	windSpeed, windBearing, pressure, weatherIcon,
	population_density,
	population_density,
	gymDistanceKm,
	gymIn100m,
	gymIn250m,
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

data$high_stats = 0
data$high_stats[data$total_stats > 250] = 1

usa = filter(data, 
	latitude < 49.5, latitude > 24, 
	longitude > -125, longitude < -66)

hist(usa$total_stats)


# stats by type
boxplot(total_stats ~ first_type, data = usa)
boxplot(total_stats ~ closeToWater, data = usa)

# contingency tables 
tab1 = table(usa$closeToWater, usa$first_type)
chisq.test(tab1)$expected

tab2 = table(usa$gymIn250m, usa$water)
chisq.test(tab2)$residuals

tab3 = table(usa$terrainType, usa$water)
chisq.test(tab3)$residuals


# counts per type
group_by(usa, first_type) %>% summarise(count = n())

group_by(usa, pokemonId) %>% summarise(count = n()) %>% 
	arrange(count) %>% as.data.frame()

tab1 = table(usa$first_type, usa$closeToWater) 
chisq.test(tab1)$residuals

tab2 = table(usa$water, usa$closeToWater) 
chisq.test(tab2)$expected

fit = glm(water ~ closeToWater, data = usa, 
	family = binomial(link = 'logit'))

predict(fit, data.frame(closeToWater = c("true", "false")), 
	type = 'response')

# water
fit = glm(water ~ 
	population_density + 
	closeToWater +
	factor(terrainType), data = usa, 
	family = binomial(link = 'logit'))

predict(fit, data.frame(
	closeToWater = c("true", "false"),
	population_density = c(6000, 6000),
	terrainType = c(0,13)), 
	type = 'response')

# bug
fit = glm(bug ~ population_density + closeToWater, data = usa, 
	family = binomial(link = 'logit'))

predict(fit, data.frame(closeToWater = c("true", "false"),
	population_density = c(6000, 2000)), 
	type = 'response')


# stats by time of day

# classes by close to water

# classes by time of day


library(nnet)
fit = multinom(first_type ~ closeToWater, data = usa,
	family = binomial(link = 'logit'))
summary(fit)

predict(fit, data.frame(closeToWater = c("true", "false"),
	population_density = c(6000, 2000)), 
	type = 'response')



library(ggmap)
map = get_map(location = 'San Francisco', zoom = 10)


ggmap(map) +
  geom_point(aes(x = longitude, y = latitude, 
  	colour = terrainType), 
  	data = usa, alpha = .5, size = 2)



# night or day
# morning, afternoon, night, late night


# weather

# logistic- pokemonId or class

# poisson - number per hour
