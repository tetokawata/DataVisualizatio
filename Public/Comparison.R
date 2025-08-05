# SetUp ----

library(tidyverse)

data <- read_csv("data.csv")

# Balance: Size ----

model_short <- estimatr::lm_robust(Price ~ RoomNumber, data)

estimatr::lm_robust(Price ~ RoomNumber + poly(Size, 2), data) # Sizeをバランス

# Balance check ----

table <- cobalt::bal.tab(
  RoomNumber ~ Size + Tenure + Distance + District,
  data
)

cobalt::love.plot(table)

# Balance: Many
#
model_long <- estimatr::lm_robust(
  Price ~ RoomNumber + Size + Tenure + Distance + District,
  data
)

model_very_long <- estimatr::lm_robust(
  Price ~ RoomNumber + (Size + Tenure + Distance + District)^2 +
    I(Size^2) + I(Tenure^2) + I(Distance^2),
  data
)

dotwhisker::dwplot(
  list(
    very_long = model_very_long,
    long = model_long,
    short = model_short
  ),
  vars_order = "RoomNumber"
)

# Double-selection ----

lasso <- hdm::rlassoEffects(
  Price ~ RoomNumber + (Size + Tenure + Distance + District)^2 +
    I(Size^2) + I(Tenure^2) + I(Distance^2),
  I = ~RoomNumber,
  data
)

summary(lasso)
confint(lasso)

lasso$selection.matrix

table(lasso$selection.matrix)

# FWL ----
#

data$y_hat <- data$Price - lm(
  Price ~ (Size + Tenure + Distance + District)^2 +
    I(Size^2) + I(Tenure^2) + I(Distance^2),
  data
)$fitted

data$d_hat <- data$RoomNumber - lm(
  RoomNumber ~ (Size + Tenure + Distance + District)^2 +
    I(Size^2) + I(Tenure^2) + I(Distance^2),
  data
)$fitted

ggplot(
  data,
  aes(
    x = RoomNumber,
    y = Price
  )
) +
  geom_bin2d() +
  geom_smooth(method = "lm")

ggplot(
  data,
  aes(
    x = d_hat,
    y = y_hat
  )
) +
  geom_bin2d() +
  geom_smooth(method = "lm")


test <- data |>
  select(
    Price,
    RoomNumber,
    Size,
    y_hat,
    d_hat
  )
# ctr + A -> ctr + Enter
