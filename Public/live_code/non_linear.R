library(tidyverse)

data <- nanoparquet::read_parquet("data.parquet")

ggplot(
  data,
  aes(
    x = RoomNumber,
    y = Price
  )
) +
  stat_summary(geom = "point") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    formula = "y ~ poly(x,3)"
  )

table(data$RoomNumber)

# ctr + A -> ctr + Enter
# 45分再開
