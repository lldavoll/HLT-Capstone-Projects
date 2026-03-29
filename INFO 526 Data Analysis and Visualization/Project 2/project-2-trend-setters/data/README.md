# Our data:
This file contains the datasets used for analyzing the spatio-temporal effects of climate variables (temperature and precipitation) on corn production and prices in the United States, including inflation-adjusted financial indicators.

# Variable Names and Description for our datasets:

## 1. temperature.csv: A monthly average temperature by year.

-Date: Year

-Jan to Dec: Average temperature in °F for each month

## 2. yearly_temp.csv: A yearly temperature summary.

-Date: Year

-avg: Annual average temperature (°F)

## 3. precipitation.csv:A Monthly total precipitation by year.

-Date: Year

-Jan to Dec: Precipitation in inches for each month

## 4. price.csv: Raw corn prices by month.

-Date: Year

-Jan to Dec: Corn price per bushel (USD)

## 5. stock.csv: A historical corn-related stock prices (raw).

-Date: Observation date

-Price: Adjusted close price

-Open, High, Low: Daily price ranges

-Vol.: Volume traded

-Change %: Percent change

## 6. stock_data.csv: Alternative stock data format.

-DateOpen

-High

-Low

-Close

-Volume

## 7. cpi.csv: The CPI for inflation adjustment.

-Year

-Jan to Dec: CPI values per month

## 8. adjusted_price.csv: Inflation-adjusted corn prices.

-Date: Full date (YYYY-MM-DD)

-Month: Extracted month label

-CornPrice: Raw corn price

-CPI: CPI value for that month

-AdjPrice: Inflation-adjusted corn price (base year = 2024)

## 9. adjusted_stock.csv: Inflation-adjusted stock prices.

-Date: Full date (YYYY-MM-DD)

-Price, Open, High, Low, Vol., Change %: Stock metrics

-Year, Month, DateKey: Derived date references

-CPI: Matched CPI value

-AdjStockPrice: Inflation-adjusted stock price (base year = 2024)

# IMPORTANT NOTES!

1. Adjustments for inflation were calculated using the formula:
Adjusted_Price = Raw_Price × (Base_CPI / Observed_CPI)

2. adjusted_price.csv and adjusted_stock.csv were used in visualizations and trend analysis.
