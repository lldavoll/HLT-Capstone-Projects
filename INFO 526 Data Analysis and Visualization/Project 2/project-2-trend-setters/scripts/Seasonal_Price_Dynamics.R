library(tidyverse)
library(plotly)
library(lubridate)

price <- read_csv("data/adjusted_price.csv") %>%
  mutate(Month = month(Date, label = TRUE), Year = year(Date))

temp <- read_csv("data/temperature.csv") %>%
  pivot_longer(-Date, names_to = "Month", values_to = "Temp") %>%
  mutate(Month = match(Month, month.abb),
         Date = make_date(Date, Month, 1),
         Month = month(Date, label = TRUE))

precip <- read_csv("data/precipitation.csv") %>%
  pivot_longer(-Date, names_to = "Month", values_to = "Precip") %>%
  mutate(Month = match(Month, month.abb),
         Date = make_date(Date, Month, 1),
         Month = month(Date, label = TRUE))

# Combine all into one monthly average dataframe
monthly_avg <- price %>%
  mutate(Month = factor(month(Date, label = TRUE), levels = month.abb)) %>%
  group_by(Month) %>%
  summarise(AvgPrice = mean(AdjPrice, na.rm = TRUE)) %>%
  left_join(
    precip %>%
      group_by(Month) %>%
      summarise(AvgPrecip = mean(Precip, na.rm = TRUE)),
    by = "Month"
  ) %>%
  left_join(
    temp %>%
      group_by(Month) %>%
      summarise(AvgTemp = mean(Temp, na.rm = TRUE)),
    by = "Month"
  )

temp_min <- min(monthly_avg$AvgTemp)
temp_max <- max(monthly_avg$AvgTemp)
precip_min <- min(monthly_avg$AvgPrecip)

temp_values <- monthly_avg$AvgTemp
precip_values <- monthly_avg$AvgPrecip


fig <- plot_ly()

# Top: Temperature (inverted)
fig <- fig %>%
  add_trace(
    x = monthly_avg$Month, y = -temp_values,
    type = 'scatter', mode = 'lines+markers', fill = 'tozeroy',
    name = "Temperature (inverted)", yaxis = "y1"
  )

# Middle: Corn Price
fig <- fig %>%
  add_trace(
    x = monthly_avg$Month, y = monthly_avg$AvgPrice,
    type = 'scatter', mode = 'lines+markers',
    name = "Adjusted Corn Price", yaxis = "y2"
  )

# Bottom: Precipitation
fig <- fig %>%
  add_trace(
    x = monthly_avg$Month, y = precip_values,
    type = 'scatter', mode = 'lines+markers', fill = 'tozeroy',
    name = "Precipitation", yaxis = "y3"
  )

# Set pretty tick range for temperature
pretty_ticks <- pretty(temp_values)

# Final layout
fig <- fig %>%
  layout(
    title = "Seasonal Dynamics: Temperature, Corn Price, Precipitation",
    
    # Completely hide X-axis
    xaxis = list(
      title = "",
      showticklabels = FALSE,
      showline = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    
    # Y-axis for Temperature (top)
    yaxis = list(
      title = "Temperature (°F)",
      domain = c(0.66, 1),
      range = c(-max(temp_values), -min(temp_values)),
      tickvals = -rev(pretty_ticks),
      ticktext = rev(pretty_ticks),
      showticklabels = TRUE
    ),
    
    # Y-axis for Corn Price (middle)
    yaxis2 = list(
      title = "Corn Price",
      domain = c(0.33, 0.66),
      showticklabels = TRUE
    ),
    
    # Y-axis for Precipitation (bottom)
    yaxis3 = list(
      title = "Precipitation",
      domain = c(0, 0.33),
      range = c(min(precip_values), max(precip_values)),
      showticklabels = TRUE
    ),
    
    showlegend = TRUE
  )

fig

# Generate smoothed values using loess
smooth_temp <- loess(AvgTemp ~ as.numeric(Month), data = monthly_avg)
smooth_price <- loess(AvgPrice ~ as.numeric(Month), data = monthly_avg)
smooth_precip <- loess(AvgPrecip ~ as.numeric(Month), data = monthly_avg)

# Add smoothed columns
monthly_avg <- monthly_avg %>%
  mutate(
    TempSmooth = predict(smooth_temp),
    PriceSmooth = predict(smooth_price),
    PrecipSmooth = predict(smooth_precip)
  )

fig <- plot_ly()

# Temperature (inverted)
fig <- fig %>%
  add_trace(
    x = monthly_avg$Month, y = -monthly_avg$TempSmooth,
    type = 'scatter', mode = 'lines', fill = 'tozeroy',
    name = "Temperature (inverted)", yaxis = "y1"
  )

# Corn Price
fig <- fig %>%
  add_trace(
    x = monthly_avg$Month, y = monthly_avg$PriceSmooth,
    type = 'scatter', mode = 'lines',
    name = "Adjusted Corn Price", yaxis = "y2"
  )

# Precipitation
fig <- fig %>%
  add_trace(
    x = monthly_avg$Month, y = monthly_avg$PrecipSmooth,
    type = 'scatter', mode = 'lines', fill = 'tozeroy',
    name = "Precipitation", yaxis = "y3"
  )

fig <- fig %>%
  layout(
    title = "Seasonal Dynamics: Temperature, Corn Price, Precipitation",
    
    # Completely hide X-axis
    xaxis = list(
      title = "",
      showticklabels = FALSE,
      showline = FALSE,
      showgrid = FALSE,
      zeroline = FALSE
    ),
    
    # Y-axis for Temperature (top)
    yaxis = list(
      title = "Temperature (°F)",
      domain = c(0.66, 1),
      range = c(-max(temp_values), -min(temp_values)),
      tickvals = -rev(pretty_ticks),
      ticktext = rev(pretty_ticks),
      showticklabels = TRUE
    ),
    
    # Y-axis for Corn Price (middle)
    yaxis2 = list(
      title = "Corn Price",
      domain = c(0.33, 0.66),
      showticklabels = TRUE
    ),
    
    # Y-axis for Precipitation (bottom)
    yaxis3 = list(
      title = "Precipitation",
      domain = c(0, 0.33),
      range = c(min(precip_values), max(precip_values)),
      showticklabels = TRUE
    ),
    
    showlegend = TRUE
  )

fig

