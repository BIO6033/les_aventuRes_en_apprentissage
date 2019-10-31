

# load packages and data --------------------------------------------------


library(tidyverse)
library(dslabs)
data("olive")


# look at the data --------------------------------------------------------


head(olive)


olive %>% 
  select(-area) %>% 
  gather(key = fatty_acid, value = percentage, -region) %>% glimpse#View

# third argument can be any kind of "select()" syntax: e.g. -region


olive %>% 
  select(-area) %>% glimpse %>% 
  gather(key = fatty_acid, value = percentage, 2:9) %>% glimpse

olive %>% 
  select(-area) %>% glimpse %>% 
  gather(key = fatty_acid, value = percentage, palmitic:eicosenoic) %>% glimpse

## gather -- and MAKE A UNIQUE ROW NUMBER
olive_long <- olive %>% 
  select(-area) %>% #glimpse %>% 
  mutate(rownum = seq_along(region)) %>% 
  gather(key = fatty_acid, value = percentage, ends_with("ic")) %>% glimpse


olive_no_area <- olive %>% 
  select(-area, -region)

olive_no_area %>% #glimpse %>% 
  gather(key = fatty_acid, value = percentage) %>% glimpse

# the opposite of gather is spread()
olive_long %>% head
olive_long %>% 
  arrange(rownum) %>% head(10)

## reverse a gather()

olive_long %>% 
  spread(fatty_acid, percentage) %>% glimpse
# doesn't work!! 

## the new, modern syntax -- pivot_longer

olive_long <- olive %>% 
  select(-area) %>% 
  mutate(rownum = seq_along(region)) %>% 
  pivot_longer(cols = ends_with("ic"), names_to = "fatty_acid", values_to = "percentage")

browseVignettes()

olive_long %>% 
  ggplot(aes(x = region, y = percentage, fill = region)) + 
  # geom_point() + 
  facet_wrap(~fatty_acid, scale = "free") + 
  geom_boxplot()


## classification par kNN ?? 

library(caret)
fit <- train(region ~ ., method = "knn", data = olive,
             tuneGrid = data.frame(k = 1:10))

plot(fit)


library(rpart)

set.seed(4812)
kill_these <- sample(1:572, 5, replace = FALSE)
olive$arachidic[kill_these] <- NA_real_


olive_ruined <- olive
set.seed(4812)
kill_these <- sample(1:572, 80, replace = FALSE)
olive_ruined$linoleic[kill_these] <- NA_real_

olive$arachidic
olive_fit_full <- rpart(region ~ . , data = select(olive_ruined, - area))

plot(olive_fit_full)
text(olive_fit_full)

olive_fit_full

data("polls_2008")

head(polls_2008)

polls_2008 %>% 
  ggplot(aes(x = day, y = margin)) + 
  geom_point()

poll_model <- rpart(margin ~ day, data = polls_2008)

plot(poll_model, margin = 0.1)
text(poll_model)

# predict with no new data == prediction on original data

polls_2008 %>% 
  mutate(y_predicted = predict(poll_model)) %>% 
  ggplot(aes(x = day, y = margin)) + 
  geom_point() + 
  geom_step(aes(y = y_predicted), col = "darkgreen")

