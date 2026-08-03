library(tidyverse) # load tidyverse

data <- nanoparquet::read_parquet("data.parquet") # load data

hist(data$Price) # make histogram

ggplot(data, aes(x = Tenure)) +
  geom_histogram()

ggplot(data, aes(x = Tenure)) +
  geom_histogram() +
  facet_wrap(~year_2024)

ggplot(data, aes(x = Tenure, y = Price)) +
  geom_bin2d() +
  facet_wrap(~year_2024)

ggplot(data, aes(x = Tenure, y = Price)) +
  stat_summary(geom = "point") +
  geom_bin2d(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = Tenure, y = Price)) +
  stat_summary(geom = "point") +
  geom_smooth(
    method = "lm",
    se = FALSE
  )

ggplot(data, aes(x = Tenure, y = Price)) +
  stat_summary(geom = "point") +
  geom_smooth(
    method = "lm",
    se = FALSE,
    formula = y ~ poly(x, 20)
  )

# ctr + A -> ctr + Enter
# ctr + S セーブ
# 10時30分再開
