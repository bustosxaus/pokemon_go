library(MASS)
library(lme4)
library(nnet)

# Water: individual level models
fit1 = glm(water ~ closeToWater, data = data, 
	family = binomial(link = 'logit'))

fit2 = glm(water ~ temperature, data = data, 
	family = binomial(link = 'logit'))
fit3 = glm(water ~ type_density, 
	data = data, 
    family = binomial(link = 'logit'))
fit4 = glm(water ~ terrainType, 
	data = data, 
	family = binomial(link = 'logit'))

fit5 = glm(water ~ type_density,
	data = data, 
	family = binomial(link = 'logit'))

fit6 = glm(water ~ weatherIcon,
	data = data, 
	family = binomial(link = 'logit'))

full_model = glm(water ~ closeToWater + temperature + 
	terrainType + type_density + gymIn100m,
	data = data, 
	family = binomial(link = 'logit'))

null = glm(water ~ 1 ,
	data = data, 
	family = binomial(link = 'logit'))

fit = stepAIC(null, scope=list(lower=null, upper=full_model),
 direction = "forward")



# bug: individual level models
fit1 = glm(bug ~ closeToWater, data = data, 
	family = binomial(link = 'logit'))

fit2 = glm(bug ~ temperature, data = data, 
	family = binomial(link = 'logit'))
fit3 = glm(bug ~ type_density, 
	data = data, 
	family = binomial(link = 'logit'))

fit4 = glm(bug ~ terrainType, 
	data = data, 
	family = binomial(link = 'logit'))



## BINARY RANDOM EFFECT MODELS
# fit model with random effects
fit_water = glmer(water ~ 
	closeToWater + 
	type_density +
	gymIn100m +
	pokestopIn100m + 
    terrain_grouped +
	(1|country),
	data = data, 
	family = binomial(link = 'logit'))

ref = ranef(fit_water)$country
REs = data.frame(country = row.names(ref), c(ref))
arrange(REs, X.Intercept.)
fixef(fit_water)


# BUG
fit_bug = glmer(bug ~ 
	closeToWater + 
	type_density +
	gymIn100m +
	pokestopIn100m + 
	terrain_grouped +
	(1|country),
	data = data, 
	family = binomial(link = 'logit'))

ref = ranef(fit_bug)$country
REs = data.frame(country = row.names(ref), c(ref))
arrange(REs, X.Intercept.)
fixef(fit)


# NORMAL
fit_normal = glmer(normal ~ 
	closeToWater + 
	type_density +
	gymIn100m +
	pokestopIn100m + 
    terrain_grouped +
	(1|country),
	data = data, 
	family = binomial(link = 'logit'))
summary(fit_normal)
ref = ranef(fit_normal)$country
REs = data.frame(country = row.names(ref), c(ref))
arrange(REs, X.Intercept.)

# Poison
fit_poison = glmer(poison ~ 
	closeToWater + 
	type_density +
	gymIn100m +
	pokestopIn100m + 
    terrain_grouped +
	(1|country),
	data = data, 
	family = binomial(link = 'logit'))

summary(fit_poison)
ref = ranef(fit_poison)$country
REs = data.frame(country = row.names(ref), c(ref))
arrange(REs, X.Intercept.)

# new data predictions

newdata = data.frame(
	closeToWater = c("true",  "false", "false", "true"),
	type_density = c("urban",  "rural", "mid", "urban"),
	gymIn100m = c("true",  "false", "false", "true"),
	pokestopIn100m = c("true",  "false", "false", "true"),
	terrain_grouped = c("water", 
		"forest", "grassland", "savanna"), 
	country = c("Netherlands",
		"Germany", "Spain", "United Kingdom"))

predict(fit_water, newdata, type = 'response')
predict(fit_bug, newdata, type = 'response')
predict(fit_normal, newdata, type = 'response')
predict(fit_poison, newdata, type = 'response')


### MULTINOMIAL
multi_fit = multinom(grouped_type ~ 
	closeToWater + 
	type_density +
	gymIn100m +
	pokestopIn100m + 
	terrain_grouped,
	data = data)

predictions = predict(multi_fit, data)

