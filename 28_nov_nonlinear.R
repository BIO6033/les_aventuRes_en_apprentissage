
## simulate a saturating function

library(tidyverse)
library(randomForest)

set.seed(4812)
fake_curve <- tibble(
  x = runif(300, 2, 450),
  ybar = 0.7 * x / (x + 70),
  yobs = rnorm(300, ybar, 0.1)
       )

fake_curve %>% 
  ggplot(aes(x = x, y = yobs)) + 
  geom_point()


# before we start: histomancy ---------------------------------------------

hist(fake_curve$yobs)

fake_curve_pwr <- tibble(
  x = runif(300, 2, 450),
  ybar = 0.07 * x ^1.7,
  yobs = rnorm(300, ybar, 50)
)


fake_curve_pwr %>% 
  ggplot(aes(x = x, y = yobs)) + geom_point()

hist(fake_curve_pwr$yobs, col = "green", breaks = 40)

# sont-ils normal? 

# comparing different models ----------------------------------------------



fake_curve %>% 
  ggplot(aes(x = x,y = ybar)) + geom_point()


fake_curve %>% 
  ggplot(aes(x = x,y = yobs)) + geom_point()


## with a linear model

mod_lm <- lm(yobs ~ x, data = fake_curve)


## via random forests
mod_forests <-  randomForest(yobs ~ x, data = fake_curve)

summary(mod_forests)

augment(mod_lm) # can also do this via broom


## visualize
library(broom)
lm_predictions <- fake_curve %>% 
  mutate(pred_lm = predict(mod_lm, se.fit = TRUE, type = "response")$fit)


lm_predictions %>% 
  ggplot(aes(x = x, y = pred_lm, ymin = y)) + geom_point()

forest_predictions <- lm_predictions %>%
  mutate(pred_randomForest = predict(mod_forests, type = "response"))


plot_some_models <- function(df) df %>% 
  pivot_longer(starts_with("pred"), names_prefix = "pred_", names_to = "model", values_to = "prediction") %>% 
  ggplot(aes(x = x, y = yobs)) + geom_point() + 
  geom_line(aes(y = prediction), col = "darkgreen", size = 2) + 
  facet_wrap(~model)

forest_predictions %>% 
  plot_some_models()


# GAMs balance nonlinearity and prediction --------------------------------

library(mgcv)

mod_gam <- gam(yobs ~ s(x), data = fake_curve)


gam_predictions <- forest_predictions %>% 
  mutate(pred_gam = predict(mod_gam, type = "response"))
  
gam_predictions %>% 
  plot_some_models

summary(mod_gam)
coef(mod_gam)


### gam examples from https://noamross.github.io

mcycle <- MASS::mcycle
gam_mod <- mgcv::gam(accel ~ s(times), data=mcycle)

## exercise: how to count the number of coefficients? 



# basis functions ---------------------------------------------------------


# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)

# Fit with 20 basis functions
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

# Visualize the GAMs
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)


# smoothing ---------------------------------------------------------------

# Fix the smoothing parameter at 0.1
gam_mod_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)

# Fix the smoothing parameter at 0.0001
gam_mod_s2 <- gam(accel ~ s(times), data = mcycle, sp = 0.0001)

# Plot both models
par(mfrow = c(2, 1))
plot(gam_mod_s1, residuals = TRUE, pch = 1, shade = TRUE)
plot(gam_mod_s2, residuals = TRUE, pch = 1, shade = TRUE)


library(gamair)
data("mpg", package="gamair")

library(mgcv)
# Fit the model
mod <- gam(hw.mpg ~ s(weight) + s(rpm) + s(price) + comp.ratio, 
           data = mpg, method = "REML")

# Plot the weight effect
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink")

# Make another plot adding the intercept value and uncertainty
plot(mod, select = 1, shade = TRUE, shade.col = "hotpink", 
     shift = coef(mod)[1], seWithMean = TRUE)


set.seed(0)
dat <- gamSim(1,n=200)

# Fit the model
mod <- gam(y ~ s(x0, k = 5) + s(x1, k = 5) + s(x2, k = 5) + s(x3, k = 5),
           data = dat, method = "REML")

par(mfrow(c(2,2)))
# Run the check function
gam.check(mod)

library(mgcv)
data(meuse, package="sp")
mod2da <- gam(cadmium ~ s(x, y) + s(dist) + s(elev),
              data = meuse, method = "REML")

# 3D surface plot
plot(mod2da, scheme = 1, pages = 1)

par(mfrow = c(1,1))
data(meuse, package="sp")
library(mgcv)
mod2d <- gam(cadmium ~ s(x,y), data=meuse, method = "REML")

# Make the perspective plot with error surfaces
vis.gam(mod2d, view = c("x", "y"), 
        plot.type = "persp", se = 2)

# totall different topic: interactions ------------------------------------

## modified from https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/interactions.html#building-an-interaction.

data("rugged", package = "rethinking")

d <- rugged


library(tidyverse)

# make the log version of criterion
d <- 
  d %>%
  mutate(log_gdp = log(rgdppc_2000))

# extract countries with GDP data
dd <-
  d %>%
  filter(complete.cases(rgdppc_2000))

# split the data into countries in Africa and not in Africa
d.A1 <-
  dd %>%
  filter(cont_africa == 1)

d.A0 <-
  dd %>%
  filter(cont_africa == 0)

library(brms)

b7.1 <-
  brm(data = d.A1, family = gaussian,
      log_gdp ~ 1 + rugged,
      prior = c(prior(normal(8, 100), class = Intercept),
                prior(normal(0, 1), class = b),
                prior(uniform(0, 10), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 7)

b7.2 <-
  update(b7.1, 
         newdata = d.A0)

b7.3 <-
  update(b7.1,
         newdata = dd)


b7.4 <-
  update(b7.3,
         newdata = dd,
         formula = log_gdp ~ 1 + rugged + cont_africa) 

b7.5 <-
  update(b7.4,
         formula = log_gdp ~ 1 + rugged*cont_africa) 

nd <- 
  tibble(rugged = seq(from = 0, to = 6.3, length.out = 30))



# rearranging for plotting ------------------------------------------------

nd <- 
  tibble(rugged      = seq(from = 0, to = 6.3, length.out = 30) %>% 
           rep(., times = 2),
         cont_africa = rep(0:1, each = 30))

f <-
  fitted(b7.4, newdata = nd) %>%
  as_tibble() %>%
  bind_cols(nd) %>%
  mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa"))


dd %>%
  mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa")) %>%
  ggplot(aes(x = rugged)) +
  geom_smooth(data = f,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5,
                  fill = cont_africa, color = cont_africa),
              stat = "identity", 
              alpha = 1/4, size = 1/2) +
  geom_point(aes(y = log_gdp, color = cont_africa),
             size = 2/3) +
  scale_x_continuous("Terrain Ruggedness Index", expand = c(0, 0)) +
  ylab("log GDP from year 2000") +
  theme(text = element_text(family = "Times"),
        legend.position  = c(.69, .94),
        legend.title     = element_blank(),
        legend.direction = "horizontal")


# plotting the interaction ------------------------------------------------

f <-
  fitted(b7.5, newdata = nd) %>%  # we can use the same `nd` data from last time
  as_tibble() %>%
  bind_cols(nd) %>%
  mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa"))

f_b7.1 <-
  fitted(b7.1, newdata = nd) %>%
  as_tibble() %>%
  bind_cols(nd)

f_b7.2 <-
  fitted(b7.2, newdata = nd) %>%
  as_tibble() %>%
  bind_cols(nd)

# here we'll put both in a single data object, with `f_b7.1` stacked atop `f_b7.2`
f <-
  fitted(b7.5, newdata = nd) %>%  # we can use the same `nd` data from last time
  as_tibble() %>%
  bind_cols(nd) %>%
  mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa"))

dd %>%
  mutate(cont_africa = ifelse(cont_africa == 1, "Africa", "not Africa")) %>%
  ggplot(aes(x = rugged, color = cont_africa)) +
  geom_smooth(data = f,
              aes(y = Estimate, ymin = Q2.5, ymax = Q97.5,
                  fill = cont_africa),
              stat = "identity", 
              alpha = 1/4, size = 1/2) +
  geom_point(aes(y = log_gdp),
             size = 2/3) +
  scale_x_continuous("Terrain Ruggedness Index", expand = c(0, 0)) +
  ylab("log GDP from year 2000") +
  theme(text = element_text(family = "Times"),
        legend.position = "none") +
  facet_wrap(~cont_africa)


post <- posterior_samples(b7.5) 

post %>%
  transmute(gamma_Africa    = b_rugged + `b_rugged:cont_africa`,
            gamma_notAfrica = b_rugged) %>%
  gather(key, value) %>%
  group_by(key) %>%
  summarise(mean = mean(value))
