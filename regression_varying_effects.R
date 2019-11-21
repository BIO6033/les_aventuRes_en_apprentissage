library(brms)
library(tidybayes)
library(ggplot2)
library(tidyverse)
library(ggridges)

# random effect models ----------------------------------------------------

# categories --------------------------------------------------------------

set.seed(5)
n <- 10
n_condition <- 5
ABC <-
  tibble(
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
  select(condition) %>%
  distinct %>% 
  add_predicted_draws(m) %>%
  ggplot(aes(x = .prediction, y = condition)) +
  geom_density_ridges(fill = "#41b6c4") + 
  theme_minimal()

ABC %>% 
  select(condition) %>%
  distinct %>% 
  add_predicted_draws(m, re_formula = ~(1|condition)) %>%
  ggplot(aes(x = .prediction, y = condition)) +
  geom_density_ridges(fill = "#41b6c4") +  
  theme_minimal()

ABC %>% 
  select(condition) %>% 
  distinct %>% 
  add_row(condition = "bugaboo") %>%
  ## Add an new row -- with new levels! 
  add_predicted_draws(m, re_formula = ~(1|condition),
                      allow_new_levels = TRUE,
                      n = 2000) %>%
  ggplot(aes(x = .prediction, y = condition)) +
  geom_density_ridges(fill = "#41b6c4") +    theme_minimal()
  


# varying intercept regression --------------------------------------------



Kline <- readr::read_csv("kline.csv")


Kline$logpop <- log(Kline$population)

Kline %>% 
  ggplot(aes(x = logpop, y = total_tools)) + 
  geom_point()

kline_glm <- glm(total_tools ~ logpop, data = Kline, family = "poisson")

summary(kline_glm)

Kline_predict <- Kline %>% 
  mutate(new_y = predict(kline_glm, type = "response"),
         pred_y = rpois(length(new_y), new_y))

Kline_predict <- Kline %>% 
  mutate(new_y = predict(kline_glm, type = "response"),
         pred_y = map(new_y, ~ rpois(200, .))) %>% 
  unnest(pred_y)

Kline %>% 
  ggplot(aes(x = logpop, y = total_tools)) + 
  geom_point(aes(y= pred_y), data = Kline_predict, col = "green", alpha = 0.3) +
  geom_point()

# this is actually not terrible! 

tools_pop_bf <- bf(total_tools ~ 0 + intercept + logpop + (1|culture), family = "poisson")

priors <- c(set_prior("normal(0, 1)", class = "b", coef = "logpop"),
            set_prior("normal(0, 10)", class = "b", coef = "intercept"),
            set_prior("cauchy(0, 1)", class = "sd"))

tools_pop_brms <- brm(tools_pop_bf, data = Kline, prior = priors, chains = 1, cores =1)



# varying slopes ----------------------------------------------------------

a       <-  3.5  # average morning wait time
b       <- -1    # average difference afternoon wait time
sigma_a <-  1    # std dev in intercepts
sigma_b <-  0.5  # std dev in slopes
rho     <- -.7   # correlation between intercepts and slopes

# the next three lines of code simply combine the terms, above
mu     <- c(a, b)
cov_ab <- sigma_a * sigma_b * rho
sigma  <- matrix(c(sigma_a^2, cov_ab, 
                   cov_ab, sigma_b^2), ncol = 2)


sigmas <- c(sigma_a, sigma_b)          # standard deviations
rho    <- matrix(c(1, rho,             # correlation matrix
                   rho, 1), nrow = 2)

# now matrix multiply to get covariance matrix
sigma <- diag(sigmas) %*% rho %*% diag(sigmas)

# how many cafes would you like?
n_cafes <- 20

set.seed(13)  # used to replicate example
vary_effects <- 
  MASS::mvrnorm(n_cafes, mu, sigma) %>% 
  data.frame() %>% 
  set_names("a_cafe", "b_cafe")

head(vary_effects)

vary_effects %>% 
  ggplot(aes(x = a_cafe, y = b_cafe)) +
  geom_point() +
  geom_rug(color = "#8B9DAF", size = 1/7)

n_visits <- 10
sigma    <-  0.5  # std dev within cafes

set.seed(13)  # used to replicate example
d <-
  vary_effects %>% 
  mutate(cafe      = 1:n_cafes) %>% 
  expand(nesting(cafe, a_cafe, b_cafe), visit = 1:n_visits) %>% 
  mutate(afternoon = rep(0:1, times = n() / 2),
         day       = rep(rep(1:5, each = 2), times = n_cafes)) %>% 
  mutate(mu        = a_cafe + b_cafe * afternoon) %>% 
  mutate(wait      = rnorm(n = n(), mean = mu, sd = sigma))

d

two_cafes <- d %>%
  mutate(afternoon = ifelse(afternoon == 0, "M", "A"),) %>%
  filter(cafe %in% c(3, 5)) %>%
  mutate(cafe = ifelse(cafe == 3, "cafe #3", "cafe #5"))

plot_two_cafes <- function(df){
  df %>%
    ggplot(aes(x = visit, y = wait, group = day, color = afternoon)) +
    geom_point(size = 2) +
    geom_line(color = "#8B9DAF") + 
    scale_x_continuous(NULL, breaks = 1:10,
                       labels = rep(c("M", "A"), times = 5)) +
    coord_cartesian(ylim = c(0,4)) +
    ylab("wait time in minutes") +
    theme(legend.position = "none",
          axis.ticks.x    = element_blank()) +
    facet_wrap(~cafe, ncol = 1)
}

### in brms 

cafe_model <- 
  brm(data = d, family = gaussian,
      wait ~ 1 + afternoon + (1 + afternoon | cafe),
      prior = c(prior(normal(0, 10), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(cauchy(0, 2), class = sd),
                prior(cauchy(0, 2), class = sigma),
                prior(lkj(2), class = cor)),
      iter = 5000, warmup = 2000, chains = 2, cores = 2,
      seed = 13)


## predictions.. 
d_predictions <- d %>% 
  filter(cafe %in% c(3, 5)) %>% 
  add_predicted_draws(cafe_model, n = 100)

d_predictions %>% ungroup %>% 
  mutate(afternoon = ifelse(afternoon == 0, "M", "A"),) %>%
  filter(cafe %in% c(3, 5)) %>%
  mutate(cafe = ifelse(cafe == 3, "cafe #3", "cafe #5")) %>% 
  plot_two_cafes + 
  # geom_point(aes(y = .prediction), alpha = 0.2) + 
  stat_pointinterval(aes(y = .prediction)) +
  geom_point(colour = "black")

partially_pooled_params <-
  # with this line we select each of the 20 cafe's posterior mean (i.e., Estimate)
  # for both `Intercept` and `afternoon`
  coef(cafe_model)$cafe[ , 1, 1:2] %>%
  as_tibble() %>%               # convert the two vectors to a tibble
  rename(Slope = afternoon) %>%
  mutate(cafe = 1:nrow(.)) %>%  # add the `cafe` index
  select(cafe, everything()) 

# compute unpooled estimates directly from data
un_pooled_params <-
  d %>%
  # with these two lines, we compute the mean value for each cafe's wait time 
  # in the morning and then the afternoon
  group_by(afternoon, cafe) %>%
  summarise(mean = mean(wait)) %>%
  ungroup() %>%  # ungrouping allows us to alter afternoon, one of the grouping variables
  mutate(afternoon = ifelse(afternoon == 0, "Intercept", "Slope")) %>%
  spread(key = afternoon, value = mean) %>%  # use `spread()` just as in the previous block
  mutate(Slope = Slope - Intercept)  


# here we combine the partially-pooled and unpooled means into a single data object, 
# which will make plotting easier.
params <-
  # `bind_rows()` will stack the second tibble below the first
  bind_rows(partially_pooled_params, un_pooled_params) %>%
  # index whether the estimates are pooled
  mutate(pooled = rep(c("partially", "not"), each = nrow(.)/2)) 

ggplot(data = params, aes(x = Intercept, y = Slope)) +
  geom_point(aes(group = cafe, color = pooled)) +
  geom_line(aes(group = cafe), size = 1/4) +
  scale_color_manual("Pooled?",
                     values = c("#80A0C7", "#A65141")) +
  coord_cartesian(xlim = range(params$Intercept),
                  ylim = range(params$Slope)) + 
  theme_dark()



# prior simulations  ------------------------------------------------------------------

inv_logit <- function(x) exp(x)/(exp(x) + 1)
inv_logit(0)


dd <- tibble(sd = seq(0.01,5,by = 0.05),
             r  = map(sd,~ rnorm(50,0,.))) %>% 
  unnest(r) %>% 
  mutate(p = inv_logit(r))

dd %>% 
  # for every little world that the normal generates, sample the probability
  mutate(b = map_dbl(p, ~ rbinom(prob = .x, size = 200, n = 1))/200) %>% 
  # mutate(m = map_dbl(b, mean)) %>% ungroup %>% #tail()
  ggplot(aes(x = sd, y = b, fill = b)) + 
  geom_point(alpha = 0.3, size = 4, pch = 21) + 
  scale_fill_distiller(type="div", palette = 3) + 
  theme_bw() + 
  guides(fill = FALSE) + 
  labs(y = "Proportion of students who liked the class",
       x = expression(paste(sigma, " of prior distribution")))

# quick gam intro ---------------------------------------------------------

devtools::install_github('m-clark/noiris')
data("pisa", package = "noiris")

library(mgcv)

head(pisa)

pisa_cc <- pisa %>% 
  select(reading, gdpPercap) %>% 
  filter(complete.cases(.))

pisa_cc %>% 
  ggplot(aes(x = gdpPercap, y = reading)) + geom_point()

mod_lm <- gam(science ~ gdpPercap, data=pisa_cc)
summary(mod_lm)

mod_gam1 <- gam(reading ~ s(gdpPercap, bs="cr"), data=pisa_cc)
summary(mod_gam1)

pisa_cc %>% 
  mutate(sci_predict = predict(mod_gam1, type = "response")) %>% 
  ggplot(aes(x = gdpPercap, y = reading)) + geom_point() + 
  geom_line(aes(y = sci_predict))

## https://twitter.com/ucfagls/status/1196845204252188673