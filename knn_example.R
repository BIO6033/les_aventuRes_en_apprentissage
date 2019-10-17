

# packages and data -------------------------------------------------------


library(tidyverse)
library(caret)
library(ggplot2)

wdbc <- read_csv("https://raw.githubusercontent.com/BIO6033/course_resources/master/data/wdbc.csv",
                 col_types = "ccdddddddddddddddddddddddddddddd")

# Select only the mean columns
wdbc_means <- wdbc %>% 
  select(ID_number:fractal_dimension_mean)

wdbc %>% 
  select(ID_number, Diagnosis, matches("mean"))


wdbc %>% 
  select(ID_number, Diagnosis, ends_with("mean"))


scale2 <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, TRUE)

scale2 <- function(j) {
  moyenne <- mean(j, na.rm = TRUE)
  ecart <- sd(j, na.rm = TRUE)
  resultat <- (j - moyenne) / ecart
  return(resultat)
}

wallets <- c(193, 201, 298, 120)

scale2(wallets)

# standardize
wdbc_stand <- wdbc_means %>% 
  mutate_if(is.double, scale2)

wdbc_stand %>% glimpse

# look at the data --------------------------------------------------------


glimpse(wdbc)


unique(wdbc$Diagnosis)

# summary of dataset -- matches example
num_obs <- nrow(wdbc)
wdbc %>% 
  group_by(Diagnosis) %>% 
  summarize(n = n(),
            percentage = n() / num_obs * 100)


# format data for analysis ------------------------------------------------

cancer_train <- wdbc_stand %>%
  select("perimeter_mean", "concavity_mean") 

head(cancer_train)

cancer_labels <- wdbc$Diagnosis


# use `caret` to train a model --------------------------------------------

k <- data.frame(k = 5)
model_knn <- train(x = data.frame(cancer_train),
                   y = cancer_labels,
                   method='knn',
                   tuneGrid = k)

## prediction
new_obs <- data.frame(perimeter_mean = 2, concavity_mean = 2)
predict(object = model_knn, newdata = new_obs)

# visualizing model spread to the data ------------------------------------

new_perim <- seq(from = min(cancer_train$perimeter_mean), to = max(cancer_train$perimeter_mean), length.out = 200)
new_concav <- seq(from = min(cancer_train$concavity_mean), to = max(cancer_train$concavity_mean), length.out = 200)

grid_data <- expand_grid(perimeter_mean = new_perim,
                         concavity_mean = new_concav)

grid_data$Diagnosis <- predict(object = model_knn, newdata = as.data.frame(grid_data))

wdbc_stand %>% 
  ggplot(aes(x = perimeter_mean, y = concavity_mean, color = Diagnosis, fill = Diagnosis)) + 
  geom_point(data = grid_data, alpha = 0.3) + 
  geom_point(alpha = 0.7, pch = 21, color = "black") + 
  scale_color_brewer(type = "qual") + 
  scale_fill_brewer(type = "qual")


# manual calculation -------------------------------------------------------------

new_obs_Perimeter <- 2
new_obs_Concavity <- 2

# manual calculation
wdbc_stand %>% 
  select(Diagnosis, perimeter_mean, concavity_mean) %>% 
  mutate(dist_from_new = sqrt((perimeter_mean - new_obs_Perimeter)^2  + (concavity_mean - new_obs_Concavity)^2)) %>% 
  arrange(dist_from_new) %>% 
  head(n = 5)

wdbc_stand %>% 
  ggplot(aes(x = perimeter_mean, y = concavity_mean, color = Diagnosis)) + 
  geom_point(alpha = 0.7) + 
  scale_color_brewer(type = "qual") + 
  geom_point(x = new_obs_Perimeter, y = new_obs_Concavity, color = "orange")


# train / test ------------------------------------------------------------

set.seed(1234) # makes the random selection of rows reproducible
set_rows <- wdbc_stand$Diagnosis %>%
  createDataPartition(p = 0.75, list = FALSE)
head(set_rows)

# note -- not all the same numbers !

