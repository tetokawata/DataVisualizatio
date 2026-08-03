set.seed(111)

library(tidyverse)

data <- nanoparquet::read_parquet("data.parquet")

split <- rsample::initial_split(data)

train <- rsample::training(split)
test <- rsample::testing(split)

model <- rpart::rpart(
  Price ~ Size + Tenure + Distance + District,
  train,
  control = rpart::rpart.control(
    maxdepth = 3,
    cp = 0
  )
) # Tree

ols <- lm(Price ~ Size + Tenure + Distance + District, train)

test$tree <- predict(model, test)
test$ols <- predict(ols, test)

mean((test$Price - test$tree)^2)

mean((test$Price - test$ols)^2)

# ctr + A -> ctr + Enter
# ctr + S
