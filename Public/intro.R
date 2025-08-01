# Set up ----

library(tidyverse)

data <- read_csv("data.csv")

# Histogram ----

hist(data$Price)

ggplot(
  data,
  aes(x = Tenure)
) +
  geom_histogram()

ggplot(
  data,
  aes(x = Tenure)
) +
  geom_histogram() +
  facet_wrap(~year_2024)

## facet----

ggplot(
  data,
  aes(x = Tenure)
) +
  geom_histogram(aes(y = after_stat(density))) +
  facet_grid(Reform ~ year_2024)

# Scatter ----

ggplot(
  data,
  aes(x = Distance, y = Price)
) +
  geom_point()

# Heatmap ----

ggplot(
  data,
  aes(x = Distance, y = Price)
) +
  geom_bin2d() +
  geom_smooth(method = "lm")

# OLS ----

model <- lm(year_2024 ~ Distance + Size, data)

dotwhisker::dwplot(model, ci = 0)

# District ----
#

model <- estimatr::lm_robust(Price ~ 0 + District, data)

dotwhisker::dwplot(model)

# estimatr tidyverse dotwhisker

# ctr + A -> ctr + Enter
# ctr + C -> ctr + V
# ctr + S
