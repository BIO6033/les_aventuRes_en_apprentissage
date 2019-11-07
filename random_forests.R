

# load packages and data --------------------------------------------------


library("randomForest")
data("polls_2008", package = "dslabs")
#remotes::install_github("njtierney/broomstick")


# bootstrapping example ---------------------------------------------------

# There is a well-known expression for the confidence interval of a mean, but
# none for the confidence interval of *median* 

library(tidyverse)
normal_numbers <- tribble(~ mean, ~sd,  ~n,
        160,      5,  200,
        155,      3,  250,
        170,      4,   300)


simulated_values_list <- pmap(normal_numbers, rnorm)

values_table <- tibble(x = flatten_dbl(simulated_values_list))

density_plot <- values_table %>% 
  ggplot(aes(x = x)) + 
  geom_density()

density_plot

# what is the median and confidence interval of median? 

median_value <- values_table$x %>% median

median_value

#replicate(40, sample_n(values_table, size = 150, replace = TRUE), simplify = FALSE)

many_medians <- rerun(40, sample_n(values_table, size = 150, replace = TRUE)) %>% 
  map_dbl(~ median(.$x))

many_medians

low_high_median <- quantile(many_medians, c(0.025, 0.975))

density_plot + 
  geom_vline(xintercept = median_value, colour = "darkgreen", lwd = 2) + 
  geom_vline(xintercept = low_high_median[1]) + 
  geom_vline(xintercept = low_high_median[2])


# https://towardsdatascience.com/what-is-out-of-bag-oob-score-in-random-forest-a7fa23d710


# examining an example ----------------------------------------------------


fit <- randomForest(margin ~ day, data = polls_2008)
plot(fit)


# model fit to data
polls_2008 %>% 
  mutate(random_forest = predict(fit)) %>% 
  ggplot(aes(x = day, y = margin)) + 
  geom_point() + 
  geom_line(aes(y = random_forest), color = "darkblue")

range(polls_2008$day)


# counterfactual, smooth data
fake_days <- data.frame(day =  seq(from = -155, to = -1, length.out = 3000))

fake_days %>%
  mutate(y_hat = predict(fit, newdata = fake_days)) %>% 
  ggplot(aes(day, y_hat)) +
  geom_line(col="red") +
  geom_point(aes(day, margin), data = polls_2008) 

broomstick::augment(fit)


all_pred <- predict(fit, newdata = fake_days, predict.all = TRUE)

all_pred %>% str

dim(all_pred$individual)

long_trees <- all_pred$individual %>% 
  data.frame %>% 
  mutate(day = fake_days$day) %>% 
  pivot_longer(-day, values_to = "prediction", names_to = "tree")

# there is a lot here! simplify the numbers of trees
some_trees <- long_trees$tree %>% unique %>% sample(size = 50, replace = FALSE)

long_trees %>% 
  filter(tree %in% some_trees) %>% 
  ggplot(aes(day, prediction)) +
  geom_point(alpha = 0.02)

## multiple variable randomforests

library(caret)
data("mnist_27", package = "dslabs")
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)

summary(train_rf)
str(train_rf)
train_rf$importance


train_rf_2 <- train(y ~ ., method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)), data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]



# randomForestExplainer ---------------------------------------------------

# using randomForestExplainer https://cran.rstudio.com/web/packages/randomForestExplainer/vignettes/randomForestExplainer.html
library(randomForest)
devtools::install_github("MI2DataLab/randomForestExplainer")
library(randomForestExplainer)
