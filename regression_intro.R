

# linear regression -------------------------------------------------------

library(ggplot2)
library(tidyverse)

tibble(x = rnorm(300, 0, 3)) %>% 
  ggplot(aes(x = x)) + 
  # geom_density() + 
  geom_histogram()


## let's make a line!

## what are the X values going to be

set.seed(4812)
line_n <- 40

x <- runif(line_n, min = -3, max = 3)

x; plot(x)
plot(rank(x), x)

y_intercept <- 14.5
y_slope <- 2

ys <- y_avg + x * y_slope

plot(x, ys)

pnorm(0, mean = 14, 0.346, log.p = TRUE)

## rnorm for more than one response
rnorm(2, mean = c(0, 100), sd = 1)

y_obs <- rnorm(line_n, mean = ys, sd = 2)
plot(x, y_obs, ylim = c(5,25))

# linear model
line_lm <- lm(y_obs ~ x)

line_lm
summary(line_lm)
coef(line_lm)
broom::tidy(line_lm)


## modifier l'ecart type du distribution du reponse -- a quel point le slope disaparait? 

y_obs <- rnorm(line_n, mean = ys, sd = 15)
plot(x, y_obs, xlim = c(-21, 40))
summary(lm(y_obs ~ x))

y_slope
y_avg



# simulating mass and drug effect -----------------------------------------

# essayez de simuler des donnees pour un etude sur l'effet d'une drogue. 
## l'effect c'est en proportion au mass du corps d'un patient
## la reponse c'est un valeur continue: 
## le temps necessaire pour completer un tache congnitive (moyen = 15 sec)


mass <- rnorm(30, mean = 170, sd = 15)
mass
# baseline <- rnorm(30, mean = 15, sd = 1.4)
# baseline

mean_mass <- mean(mass)
mean_mass
# pourquoi different?
sd_mass <- sd(mass)
sd_mass

mass_centered <- mass - mean_mass
mass_centered # pq 0??
mass_scaled <- mass_centered / sd_mass

mean(mass_scaled)
sd(mass_scaled)

pente_mass <- 0.2
duration <- 15 + pente_mass * mass_centered + rnorm(30, 0, 1)
# note that Rnorm is added here!



summary(lm(duration ~ mass))

coef(lm(duration ~ mass_centered))
# unites de mass_centered ? 

summary(lm(duration ~ mass_scaled))
# unites de slope ici? 

# standardizing -----------------------------------------------------------

# question

x1 <- rnorm(30, mean = 0, sd = 1)
x2 <- rnorm(30, mean = 0, sd = 1)

effect_per_sd_1 <- 1.2
effect_per_sd_1 <- 0.4

sd_1 <- 0.8
sd_2 <- 1.5

## what is the effect of each on the response? 
## what is the effect per unit of each? 



# glms --------------------------------------------------------------------

### poisson distribution
rpois(30, 6)

rand_pois <- rpois(300, 6)
mean(rand_pois)
var(rand_pois)

lambda <- 3
rand_pois <- rpois(300, lambda)
hist(rand_pois, col= "darkgreen", xlim = c(0, 10))
abline(v = lambda, lwd = 3, col = "darkorange")


## cannot be negative
rpois(30, -1)

## how to keep a value positive? 
rpois(30, exp(-1))

exp(-100) # negative numbers are between 0 and 1


# simulating count data ---------------------------------------------------
n_patients <- 50
# treatment with a drug
xs <- rep(c(0,1), times = n_patients/2)

## factors are NUMBERS. 
untreats <- rep(c("untreated","treated"), times = 4)
untreat_fac <- factor(untreats, levels = c("untreated", "treated"))
typeof(untreat_fac)
as.numeric(untreat_fac)

## how many times do people visit hospital on average
y_mean_ctrl <- 5

rpois(200, y_mean_ctrl)

effect_drug <- -2

y_mean <- y_mean_ctrl + effect_drug * xs

visits_hospital <- rpois(n_patients, y_mean)


hospital_glm <- glm(visits_hospital ~ xs, family = "poisson")
summary(hospital_glm)
## where did our values go?

coefs_hosp <- coef(hospital_glm)

a <- coefs_hosp[[1]]
b <- coefs_hosp[[2]]

exp(a)

exp(a) * exp(b) - exp(a)

## visualize with jitter
sim_data <- tibble(visits_hospital,
           treatment = xs)

# see predict.glm 
# broom::augment(sim_data, hospital_glm)
sim_data_predict <- sim_data %>% 
  mutate(new_y = predict(hospital_glm, type = "response"))


sim_data %>% 
  ggplot(aes(x = as.factor(treatment), y = visits_hospital)) + 
  # geom_boxplot() + 
  geom_point(position = position_jitter(width = 0.1, height = 0)) + 
  geom_point(aes(y = new_y), data = sim_data_predict, fill = "orange", pch = 21, size = 3)

