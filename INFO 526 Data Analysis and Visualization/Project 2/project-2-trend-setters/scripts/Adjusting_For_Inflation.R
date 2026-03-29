library(tidyverse)
library(lubridate)

cpi <- read_csv("data/cpi.csv")
price <- read_csv("data/price.csv")
stock <- read_csv("data/stock.csv")

# Reshape CPI
cpi_long <- cpi %>%
  pivot_longer(-Year, names_to = "Month", values_to = "CPI") %>%
  filter(!is.na(CPI)) %>%
  mutate(Month = match(Month, month.abb),
         Date = make_date(Year, Month, 1))

# Reshape Price
price_long <- price %>%
  pivot_longer(-Date, names_to = "Month", values_to = "CornPrice") %>%
  mutate(Month = match(Month, month.abb),
         Year = Date,
         Date = make_date(Year, Month, 1))

stock <- stock %>%
  mutate(Date = mdy(Date)) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  mutate(DateKey = make_date(Year, Month, 1))

base_cpi <- cpi_long %>% filter(year(Date) == 2024) %>% pull(CPI) %>% mean(na.rm = TRUE)

# Join CPI to Corn Price
price_adj <- price_long %>%
  left_join(cpi_long %>% select(Date, CPI), by = "Date") %>%
  mutate(AdjPrice = CornPrice * base_cpi / CPI)

# Join CPI to Stock
stock_adj <- stock %>%
  left_join(cpi_long %>% select(Date, CPI), by = c("DateKey" = "Date")) %>%
  mutate(AdjStockPrice = Price * base_cpi / CPI)

write_csv(price_adj, "data/adjusted_price.csv")
write_csv(stock_adj, "data/adjusted_stock.csv")
