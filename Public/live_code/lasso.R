set.seed(111)

library(tidyverse)

data <- nanoparquet::read_parquet("data.parquet")

split <- rsample::initial_split(data)

train <- rsample::training(split)
test <- rsample::testing(split)

model_long <- lm(
  Price ~ (Size + Tenure + Distance + District)^2 +
    poly(Size, 4) +
    poly(Tenure, 4) +
    poly(Distance, 4),
  train
)

model <- lm(
  Price ~ Size + Tenure + Distance + District,
  train
)

model_LASSO <- hdm::rlasso(
  Price ~ (Size + Tenure + Distance + District)^2 +
    poly(Size, 4) +
    poly(Tenure, 4) +
    poly(Distance, 4),
  train,
  post = FALSE
) #

test$pred_long <- predict(model_long, test)
test$pred <- predict(model, test)
test$pred_LASSO <- predict(model_LASSO, test) #

mean((test$pred_long - test$Price)^2)
mean((test$pred - test$Price)^2)
mean((test$pred_LASSO - test$Price)^2) #

# 10時40分再開
