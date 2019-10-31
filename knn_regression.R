## summary of the knn example introduced here https://ubc-dsci.github.io/introduction-to-datascience/regression1.html#k-nearest-neighbours-regression


# loading packages --------------------------------------------------------

library(tidyverse)
library(scales)
library(caret)
# library(gridExtra)


# look at the data --------------------------------------------------------

data(Sacramento)
head(Sacramento)


plot_sacramento <- function(x){ 
  ggplot(x, aes(x = sqft, y = price)) +
    geom_point(alpha = 0.4) +
    xlab("House size (square footage)") +
    ylab("Price (USD)") +
    scale_y_continuous(labels = dollar_format()) 
}

eda <- plot_sacramento(Sacramento)
eda

set.seed(1234)
small_sacramento <- sample_n(Sacramento, size = 30)

small_sacramento %>% plot_sacramento() + geom_vline(xintercept = 2000)

## how much for 2000 sq foot? find nearest neighbours 
nearest_neighbours <- small_sacramento %>% 
  mutate(diff = abs(2000 - sqft)) %>% 
  arrange(diff) %>% 
  head(5)

mean(nearest_neighbours$price)

unlist(select(nearest_neighbours, price))

nearest_neighbours %>% 
  select(price) %>% 
  unlist 

# gives a column
nearest_neighbours["price"]

nearest_neighbours[["price"]]


# separate data and train a k-NN algorithm -------------------------------------

set.seed(1234)
training_rows <- Sacramento %>% 
  select(price) %>% 
  unlist() %>% 
  createDataPartition(p = 0.6, list = FALSE)

head(training_rows)

xx <- c(3,4,5,6,7)
xx
xx[1]
xx[c(1,3,4)]
# xx[1,3,4]

X_train <- Sacramento %>% 
  select(sqft) %>% 
  slice(training_rows) %>% 
  data.frame()

Y_train <- Sacramento %>% 
  select(price) %>% 
  slice(training_rows) %>% 
  unlist()

X_test <- Sacramento %>% 
  select(sqft) %>% 
  slice(-training_rows) %>% 
  data.frame()

Y_test <- Sacramento %>% 
  select(price) %>% 
  slice(-training_rows) %>% 
  unlist()

## find a good value of K ----------------------
train_control <- trainControl(method = "cv", number = 10)
# makes a column of k's, from 1 to 500 in increments of 5
k_lots = data.frame(k = seq(from = 1, to = 500, by = 5)) 

set.seed(1234)
knn_reg_cv_10 <- train(x = X_train, 
                       y = Y_train, 
                       method = "knn", 
                       tuneGrid = k_lots, 
                       trControl = train_control) 

ggplot(knn_reg_cv_10$results, aes(x = k, y = RMSE)) +
  geom_point() +
  geom_line()

knn_reg_cv_10

str(knn_reg_cv_10)

knn_reg_cv_10$bestTune$k

## extract the best K value
k = data.frame(k = knn_reg_cv_10$bestTune$k)


set.seed(1234)
knn_reg_final <- train(x = X_train, y = Y_train, method = "knn", tuneGrid = k)

# predict is useful -- many models have a "predict" function
test_pred <- predict(knn_reg_final, X_test)
modelvalues <- data.frame(obs = Y_test, pred = test_pred)
test_results <- defaultSummary(modelvalues)
test_results

# recalculate RMSE

# a plot of predictions ---------------------------------------------------

set.seed(1234)
Sacramento$sqft %>% range
predictions_all <- data.frame(sqft = seq(from = 500, to = 4250, by = 1))
predictions_all$price <- predict(knn_reg_final, 
                                 newdata = predictions_all)

plot_final <- Sacramento %>% 
  plot_sacramento()  +
  geom_line(data = predictions_all, color = "#E6550D", lwd = 2) +
  ggtitle("k = 41")
plot_final


RColorBrewer::brewer.pal(3, "Oranges")
