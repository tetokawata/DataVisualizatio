library(tidyverse)

Y <- sample(0:1, size = 5000, replace = TRUE) # Sampling

mean(Y) # Mean

estimatr::lm_robust(Y ~ 1)

data <- nanoparquet::read_parquet("data.parquet")

estimatr::lm_robust(Reform ~ 1, data) # Distribution

estimatr::lm_robust(Size ~ 1, data) # Mean

model <- estimatr::lm_robust(Size ~ 0 + District, data) # OLS

dotwhisker::dwplot(model)

model <- estimatr::lm_robust(
  Price ~ Size + Tenure + Distance + District,
  data
)

dotwhisker::dwplot(model) +
  geom_vline(xintercept = 0)

# ctr + A -> ctr + Enter
# ctr + S
