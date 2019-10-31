## summary of the knn example introduced here https://ubc-dsci.github.io/introduction-to-datascience/regression1.html#k-nearest-neighbours-regression


library(tidyverse)
library(scales)
library(caret)
library(gridExtra)

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

small_sacramento %>% plot_sacramento()

## how much for 2000 sq foot? find nearest neighbours 
nearest_neighbours <- small_sacramento %>% 
  mutate(diff = abs(2000 - sqft)) %>% 
  arrange(diff) %>% 
  head(5)
nearest_neighbours


set.seed(1234)
training_rows <- Sacramento %>% 
  select(price) %>% 
  unlist() %>% 
  createDataPartition(p = 0.6, list = FALSE)

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

## extract the best K value
k = data.frame(k = knn_reg_cv_10$bestTune$k)


set.seed(1234)
knn_reg_final <- train(x = X_train, y = Y_train, method = "knn", tuneGrid = k)

test_pred <- predict(knn_reg_final, X_test)
modelvalues <- data.frame(obs = Y_test, pred = test_pred)
test_results <- defaultSummary(modelvalues)
test_results



# a plot of predictions ---------------------------------------------------

set.seed(1234)
predictions_all <- data.frame(sqft = seq(from = 500, to = 4250, by = 1))
predictions_all$price <- predict(knn_reg_final, 
                                 data.frame(sqft = seq(from = 500, to = 4250, by = 1)))
train_data <- bind_cols(X_train, data.frame(price = Y_train)) #combines X_train and Y_train to be on data set
plot_final <- ggplot(train_data, aes(x = sqft, y = price)) +
  geom_point(alpha = 0.4) +
  xlab("House size (square footage)") +
  ylab("Price (USD)") +
  scale_y_continuous(labels = dollar_format())  +
  geom_line(data = predictions_all, aes(x = sqft, y = price), color = "blue") +
  ggtitle("k = 41")
plot_final


