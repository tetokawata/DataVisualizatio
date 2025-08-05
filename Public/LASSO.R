library(tidyverse)

data <- read_csv("data.csv") |>
  mutate(District = factor(District) |>
    fct_relevel("葛飾区"))

lm(Price ~ District, data)

hdm::rlasso(Price ~ District, data, post = FALSE)

ggplot(
  data,
  aes(
    x = Size,
    y = Price
  )
) +
  geom_smooth(
    method = "lm",
    formula = "y ~ poly(x,15)"
  )

lm(Price ~ poly(Size, 15), data)

hdm::rlasso(Price ~ poly(Size, 15), data)

# ctr + A -> ctr + Enter
