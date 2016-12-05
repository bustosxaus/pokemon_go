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


# fit model with random effects
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


# fit model with random effects
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

newdata = data.frame(
	closeToWater = c("true"),
	type_density = c("urban"),
	terrainType = c(0), 
	country = "Belgium")

newdata = data.frame(
	closeToWater = c("true"),
	type_density = c("urban"),
	terrainType = c(0), 
	country = "United Kingdom")

newdata = data.frame(
	closeToWater = c("true"),
	type_density = c("urban"),
	gymIn100m = "true",
	pokestopIn100m = "true",
	terrain_grouped = "other", 
	country = "United States of America")

predict(fit, newdata, type = 'response')


### MULTINOMIAL
multi_fit = multinom(grouped_type ~ 
	closeToWater + 
	type_density +
	gymIn100m +
	pokestopIn100m + 
	terrain_grouped,
	data = data)

predictions = predict(multi_fit, data)



post_betas = matrix(0, nrow = J, ncol = P+1)
post_betas[2:J,] = coefficients(multi_fit)
    
    
# design matrix for test set, X_test
X_test = model.matrix(formula, data = data)
P = ncol(X_test)
X_test = cbind(1, X_test)
    
# Finding multinomial probabilities associated with each test observation
probs = matrix(0, nrow = nrow(X_test), ncol = J)
    for (w in 1:nrow(X_test)){
      new_x = X_test[w,]
      for (j in 1:J){
        probs[w,j] = exp(new_x %*% post_betas[j,])
      }
    }