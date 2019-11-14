

# linear regression -------------------------------------------------------

library(ggplot2)
library(tidyverse)

tibble(x = rnorm(300, 0, 3)) %>% 
  ggplot(aes(x = x)) + 
  # geom_density() + 
  geom_histogram()


## let's make a line!

## what are the X values going to be

line_n <- 40

x <- runif(line_n, min = -3, max = 3)

x; plot(x)
plot(rank(x), x)

y_avg <- 14.5
y_slope <- 2

ys <- y_avg + x * y_slope

plot(x, ys)

y_obs <- rnorm(line_n, mean = ys, sd = 2)

plot(x, y_obs, ylim = c(5,25))

# linear model
line_lm <- lm(y_obs ~ x)

coef(line_lm)

broom::tidy(line_lm)
y_slope
y_avg



# simulating mass and drug effect -----------------------------------------

# essayez de simuler des donnees pour un etude sur l'effet d'une drogue. 
## l'effect c'est en proportion au mass du corps d'un patient
## la reponse c'est un valeur continue: 
## le temps necessaire pour completer un tache congnitive (moyen = 15 sec)


mass <- rnorm(30, mean = 170, sd = 9)
mass
# baseline <- rnorm(30, mean = 15, sd = 1.4)
# baseline

mean_mass <- mean(mass)
mean_mass
# pourquoi different?
sd_mass <- sd(mass)
sd_mass

mass_centered <- mass - mean_mass
mass_scaled <- mass_centered / sd_mass

pente_mass <- 0.2
improvement <- 15 + pente_mass * mass_centered + rnorm(30, 0, 1)
# unites de pente_mass ? 

summary(lm(improvement ~ mass_centered))

summary(lm(improvement ~ mass_scaled))
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

## cannot be negative
rpois(30, -1)

## how to keep a value positive? 
rpois(30, exp(-1))

exp(-1) # negative numbers are between 0 and 1


# simulating count data ---------------------------------------------------
n_patients <- 50
# treatment with a drug
xs <- rep(c(0,1), times = n_patients/2)

## how many times do people visit hospital on average
y_mean_ctrl <- 5

mean(rpois(200, y_mean_ctrl))

effect_drug <- -1.5

y_mean <- y_mean_ctrl + effect_drug * xs

visits_hospital <- rpois(30, y_mean)


hospital_glm <- glm(visits_hospital ~ xs, family = "poisson")
summary(hospital_glm)
## where did our values go?
exp(coef(hospital_glm)[[1]])

exp(coef(hospital_glm)[[1]]) * exp(coef(hospital_glm)[[2]]) - exp(coef(hospital_glm)[[1]])


# random effect models ----------------------------------------------------

library(brms)
library(tidybayes)
library(ggplot2)

Kline <- read_csv("kline.csv")

Kline$logpop <- log(Kline$population)

tools_pop_bf <- bf(total_tools ~ 0 + intercept + logpop + (1|culture), family = "poisson")

priors <- c(set_prior("normal(0, 1)", class = "b", coef = "logpop"),
            set_prior("normal(0, 10)", class = "b", coef = "intercept"),
            set_prior("cauchy(0, 1)", class = "sd"))

tools_pop_brms <- brm(tools_pop_bf, data = Kline, prior = priors, chains = 1, cores =1)


# categories --------------------------------------------------------------

set.seed(5)
n <- 10
n_condition <- 5
ABC <-
  data_frame(
    condition = rep(c("A", "B", "C", "D", "E"), n),
    response = rnorm(n * 5, c(0, 1, 2, 1, -1), 0.5)
  )

ABC %>%
  ggplot(aes(y = condition, x = response)) +
  geom_point(pch = 21, size = 4, stroke = 1.4, fill = "#41b6c4")

m <- brm(
  response ~ (1 | condition), data = ABC, 
  control = list(adapt_delta = .99),
  prior = c(
    prior(normal(0, 1), class = Intercept),
    prior(student_t(3, 0, 1), class = sd),
    prior(student_t(3, 0, 1), class = sigma)
  )
)

ABC %>%
  data_grid(condition) %>%
  add_predicted_draws(m) %>%
  ggplot(aes(x = .prediction, y = condition)) +
  geom_density_ridges(fill = "#41b6c4") + 
  theme_minimal()


