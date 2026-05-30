library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

url <- "https://api.open-meteo.com/v1/forecast?latitude=-33.8688&longitude=151.2093&daily=temperature_2m_max,temperature_2m_min,precipitation_sum&timezone=Australia%2FSydney"

response <- GET(url)

weather_json <- fromJSON(content(response, "text"))

weather_data <- data.frame(
  date = weather_json$daily$time,
  max_temp = weather_json$daily$temperature_2m_max,
  min_temp = weather_json$daily$temperature_2m_min,
  precipitation = weather_json$daily$precipitation_sum
)

weather_data <- weather_data %>%
  mutate(
    avg_temp = (max_temp + min_temp) / 2,
    temp_range = max_temp - min_temp
  )

head(weather_data)

summary(weather_data)

mean(weather_data$max_temp)
mean(weather_data$min_temp)
mean(weather_data$avg_temp)
mean(weather_data$precipitation)

ggplot(weather_data, aes(x = date)) +
  geom_line(aes(y = max_temp, color = "Maximum Temperature"), linewidth = 1.2) +
  geom_line(aes(y = min_temp, color = "Minimum Temperature"), linewidth = 1.2) +
  labs(
    title = "Daily Maximum and Minimum Temperature",
    x = "Date",
    y = "Temperature (°C)",
    color = "Legend"
  ) +
  theme_minimal()

ggplot(weather_data, aes(x = date, y = precipitation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Daily Precipitation Levels",
    x = "Date",
    y = "Precipitation (mm)"
  ) +
  theme_minimal()

ggplot(weather_data, aes(x = date, y = avg_temp)) +
  geom_point(color = "red", size = 3) +
  geom_line(group = 1, color = "darkred") +
  labs(
    title = "Average Daily Temperature",
    x = "Date",
    y = "Average Temperature (°C)"
  ) +
  theme_minimal()