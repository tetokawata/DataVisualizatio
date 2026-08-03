library(tidyverse) # load tidyverse

data <- nanoparquet::read_parquet("data.parquet") # load data

model <- lm(Price ~ Tenure + Size + Distance, data) # OLS

dotwhisker::dwplot(model, ci = 0)

model_size <- lm(Size ~ 0 + District, data)

dotwhisker::dwplot(model_size, ci = 0)

# ctr + A -> ctr + Enter
# ctr + S
