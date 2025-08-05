library(tidyverse)

data <- read_csv("data.csv")

model_Y <- ranger::ranger(
  Price ~ Size + Tenure + Distance,
  data
)

model_D <- ranger::ranger(
  RoomNumber ~ Size + Tenure + Distance,
  data
)

Res_Y <- data$Price - model_Y$predictions

Res_D <- data$RoomNumber - model_D$predictions

estimatr::lm_robust(Res_Y ~ Res_D)
