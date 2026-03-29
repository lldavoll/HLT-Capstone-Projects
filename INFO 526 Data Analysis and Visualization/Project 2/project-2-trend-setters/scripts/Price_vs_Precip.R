library(tidyverse)
library(lubridate)


precipitation <- read_csv("data/precipitation.csv") %>%
  pivot_longer(-Date, names_to = "Month", values_to = "Precipitation") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = make_date(as.integer(Date), Month, 1)
  ) %>%
  arrange(Date)


loess_pre <- loess(Precipitation ~ as.numeric(Date), data = precipitation, span = 0.9)
precipitation$preSmooth <- predict(loess_pre)


price <- read_csv("data/adjusted_price.csv") %>%
  mutate(Date = as.Date(Date))


merged_partial <- left_join(price, precipitation, by = "Date") %>% drop_na()

pre_min <- min(precipitation$preSmooth, na.rm = TRUE)
pre_max <- max(precipitation$preSmooth, na.rm = TRUE)

price_min <- min(merged_partial$AdjPrice, na.rm = TRUE)
price_max <- max(merged_partial$AdjPrice, na.rm = TRUE)

scale_price <- function(x) {
  (x - price_min) / (price_max - price_min) * (pre_max - pre_min) + pre_min
}

inv_scale_price <- function(x) {
  (x - pre_min) / (pre_max - pre_min) * (price_max - price_min) + price_min
}

price$PriceScaled <- scale_price(price$AdjPrice)

ggplot() +
  geom_line(data = precipitation, aes(x = Date, y = preSmooth), color = "#0D92F4", size = 2, alpha=0.7) +
  geom_line(data = price, aes(x = Date, y = PriceScaled), color = "#1DCD9F", size = 1) +
  scale_y_continuous(
    name = "Smoothed Precipitation (inches)",
    limits = c(pre_min, pre_max),
    sec.axis = sec_axis(
      ~ inv_scale_stock(.), 
      name = "Adjusted Corn Price ($)",
      breaks = seq(3, 10, by = 1),
      labels = function(x) sprintf("$%g", x)
    )
  ) +
  labs(
    title = "Precipitation (2000–2024) and Adjusted Corn Price (2000–2024)",
    x = "Date"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    axis.title.y.left = element_text(color = "#0D92F4", size = 16),
    axis.title.y.right = element_text(color = "#1DCD9F", size = 16),
    axis.text.y.left = element_text(color = "#0D92F4", size = 14),
    axis.text.y.right = element_text(color = "#1DCD9F", size = 14),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

