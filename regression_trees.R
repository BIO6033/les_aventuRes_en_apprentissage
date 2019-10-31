library(tidyverse)
library(dslabs)
data("olive")

head(olive)

olive %>% 
  select(-area) %>% 
  gather(fatty_acid, percentage, -region) %>% 
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


olive_fit_full <- rpart(region ~ . , data = select(olive, - area))

plot(olive_fit_full)
text(olive_fit_full)

data("polls_2008")

head(polls_2008)

polls_2008 %>% 
  ggplot(aes(x = day, y = margin)) + 
  geom_point()

poll_model <- rpart(margin ~ day, data = polls_2008)

plot(poll_model, margin = 0.1)
text(poll_model)

polls_2008 %>% 
  mutate(y_predicted = predict(poll_model)) %>% 
  ggplot(aes(x = day, y = margin)) + 
  geom_point() + 
  geom_step(aes(y = y_predicted), col = "darkgreen")
