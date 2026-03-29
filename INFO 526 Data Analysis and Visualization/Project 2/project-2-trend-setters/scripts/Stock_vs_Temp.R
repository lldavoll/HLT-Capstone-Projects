library(tidyverse)
library(lubridate)


temperature <- read_csv("data/temperature.csv") %>%
  pivot_longer(-Date, names_to = "Month", values_to = "Temperature") %>%
  mutate(
    Month = match(Month, month.abb),
    Date = make_date(as.integer(Date), Month, 1)
  ) %>%
  arrange(Date)


loess_temp <- loess(Temperature ~ as.numeric(Date), data = temperature, span = 1)
temperature$TempSmooth <- predict(loess_temp)


stock <- read_csv("data/adjusted_stock.csv") %>%
  mutate(Date = as.Date(Date))


merged_partial <- left_join(stock, temperature, by = "Date") %>% drop_na()

temp_min <- min(temperature$TempSmooth, na.rm = TRUE)
temp_max <- max(temperature$TempSmooth, na.rm = TRUE)

stock_min <- min(merged_partial$AdjStockPrice, na.rm = TRUE)
stock_max <- max(merged_partial$AdjStockPrice, na.rm = TRUE)

scale_stock <- function(x) {
  (x - stock_min) / (stock_max - stock_min) * (temp_max - temp_min) + temp_min
}

inv_scale_stock <- function(x) {
  (x - temp_min) / (temp_max - temp_min) * (stock_max - stock_min) + stock_min
}

stock$StockScaled <- scale_stock(stock$AdjStockPrice)

ggplot() +
  geom_line(data = temperature, aes(x = Date, y = TempSmooth), color = "#FF6600", size = 2, alpha=0.7) +
  geom_line(data = stock, aes(x = Date, y = StockScaled), color = "#1DCD9F", size = 1) +
  scale_y_continuous(
    name = "Smoothed Temperature (°F)",
    limits = c(48, 51.3),
    sec.axis = sec_axis(
      ~ inv_scale_stock(.), 
      name = "Adjusted Stock Price ($)",
      breaks = seq(10, 70, by = 10),
      labels = function(x) sprintf("$%g", x)
    )
  ) +
  labs(
    title = "Temperature (2000–2024) and Adjusted Stock Price (2011–2024)",
    x = "Date"
  ) +
  
theme_minimal(base_size = 16) +
  theme(
    axis.title.y.left = element_text(color = "#FF6600", size = 16),
    axis.title.y.right = element_text(color = "#1DCD9F", size = 16),
    axis.text.y.left = element_text(color = "#FF6600", size = 14),
    axis.text.y.right = element_text(color = "#1DCD9F", size = 14),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold")
  )

