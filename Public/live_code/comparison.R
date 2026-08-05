set.seed(111)

library(tidyverse)

data <- nanoparquet::read_parquet("data.parquet")

ggplot(
  data,
  aes(
    x = Distance,
    fill = factor(year_2024)
  )
) +
  geom_density(alpha = 0.5)

tab <- cobalt::bal.tab(
  year_2024 ~ District,
  data
)

cobalt::love.plot(tab)

model <- estimatr::lm_robust(
  Price ~ year_2024,
  data
)

model_long <- estimatr::lm_robust(
  Price ~ year_2024 +
    (Size +
      Tenure +
      Distance +
      District)^2 +
    poly(Size, 2) +
    poly(Tenure, 2) +
    poly(Distance, 2),
  data
)

X <- model.matrix(
  ~ 0 +
    (Size +
      Tenure +
      Distance +
      District +
      RoomNumber +
      RoomK +
      RoomL +
      RoomD)^2 +
    poly(Size, 2) +
    poly(Tenure, 2) +
    poly(Distance, 2) +
    poly(RoomNumber, 2),
  data
)

model_ds <- hdm::rlassoEffect(
  x = X,
  d = data$year_2024,
  y = data$Price
)

summary(model_ds)
confint(model_ds)
table(model_ds$selection.index)

dotwhisker::dwplot(
  list("adjust" = model_long, "non-adjust" = model),
  vars_order = "year_2024"
)

# ctr + A -> ctr + Enter
