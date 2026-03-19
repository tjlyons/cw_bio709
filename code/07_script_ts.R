#' DESCRIPTION:
#' Script for time-series

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               forecast,
               lterdatasampler,
               daymetr,
               glarma)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_ts_anormaly.csv"
(df_ts <- read_csv(url))

# Plot time-series anomalies
g_base <- df_ts %>% 
  ggplot(aes(x = year,          # Map 'year' to the x-axis
             y = anormaly)) +  # Map 'anormaly' to the y-axis
  geom_line() +                  # Add a line connecting the points
  geom_point() +                 # Add points at each observation
  theme_bw() +                   # Use a clean black-and-white theme
  labs(
    x = "Year",                     # Label x-axis
    y = "Anomaly"                   # Label y-axis
  )

## Data analysis using LM

m_lm <- lm(anormaly ~ year,
   data = df_ts)
summary(m_lm) # This yeilds highly significant

g_base +
  geom_abline(intercept = coef(m_lm)[1],
              slope = coef(m_lm)[2])

## Random walk
# The dataset above was generated this way
# Each year's data is connected...
y <- NULL
y[1] <- 0
y[2] <- y[1] + rnorm(1, mean = 0, sd = 1)

for (i in 1:99) {
  y[i + 1] <- y[i] + rnorm(1, mean = 0, sd = 1)
}

tibble(y = y,
       x = 1:length(y)) %>% 
  ggplot(aes(x = x,
             y = y)) + 
  geom_point() +
  geom_line()
# every time we run this, we get a different result, BUT
# our data always trends upwards because previous times' values accumulate
# our data points need to be independent
# we need to model using a different assumption

# Autoregressive Model (AR)

# Create a tibble (modern data frame) from the LakeHuron time series
df_huron <- tibble(
  year = time(LakeHuron),                # Extracts the time component (years) from the LakeHuron ts object
  water_level = as.numeric(LakeHuron)    # Converts LakeHuron values to numeric (from ts class)
) %>% 
  arrange(year)                           # Ensures the data is ordered by year

# Plot Lake Huron time series with a linear trend
df_huron %>% 
  ggplot(aes(x = year, y = water_level)) +
  geom_point(alpha = 0.25) +       # Semi-transparent points
  geom_line(linetype = "dotted") + # Dotted line connecting points
  geom_smooth(method = "lm",       # Linear trend line
              color = "black",
              linewidth = 0.5) +
  theme_bw() +
  labs(x = "Year",
       y = "Water Level")

m_ar1 <- Arima(df_huron$water_level,
               order = c(1, 0, 0)) ##(p, d, q) AR: 1,0,0

## fitted values
df_huron_ar1 <- df_huron %>% 
  mutate(fit = fitted(m_ar1) %>% 
           as.numeric())

df_huron_ar1 %>% 
  ggplot() +
  geom_point(aes(x = year, 
                 y = water_level),
             alpha = 0.25) +        # Plot observed water levels
  geom_line(aes(x = year, 
                y = fit),           # Plot AR(1) fitted values
            color = "hotpink") +
  theme_bw() +
  labs(title = "AR")

# Moving Average Model (AM)

m_ma1 <- Arima(df_huron$water_level,
               order = c(0, 0, 1)) # MA: 0,0,1

## fitted values
df_huron_ma1 <- df_huron %>% 
  mutate(fit = fitted(m_ma1) %>% 
           as.numeric())

df_huron_ma1 %>% 
  ggplot() +
  geom_point(aes(x = year, 
                 y = water_level),
             alpha = 0.25) +        # Plot observed water levels
  geom_line(aes(x = year, 
                y = fit),           # Plot AR(1) fitted values
            color = "chartreuse") +
  theme_bw() +
  labs(title = "MA")

# Autoregressive Moving Average Model (ARMA)

m_arma <- Arima(
  df_huron$water_level,
  order = c(1, 0, 1) # ARIMA: (1,0,1)
)

## fitted values
df_huron_arma <- df_huron %>% 
  mutate(fit = fitted(m_arma) %>% 
           as.numeric())

df_huron_arma %>% 
  ggplot() +
  geom_point(aes(x = year, 
                 y = water_level),
             alpha = 0.25) +        # Plot observed water levels
  geom_line(aes(x = year, 
                y = fit),           # Plot AR(1) fitted values
            color = "cyan") +
  theme_bw() +
  labs(title = "ARMA")

# ARIMA

m_arima <- Arima(df_huron$water_level,
                  order = c(1, 1, 0)) # ARIMA: 1,1,0

## fitted values
df_huron_arima <- df_huron %>% 
  mutate(fit = fitted(m_arima) %>% 
           as.numeric())

df_huron_arima %>% 
  ggplot() +
  geom_point(aes(x = year, 
                 y = water_level),
             alpha = 0.25) +        # Plot observed water levels
  geom_line(aes(x = year, 
                y = fit),           # Plot AR(1) fitted values
            color = "salmon") +
  theme_bw() +
  labs(title = "ARIMA")

# Model selection

# Forecast has a tool to automatically pick which model
auto.arima(y = df_huron$water_level,
           stepwise = FALSE,
           ic = "aic") #information criterion, which statistic to use 
# ARIMA returns the lowest AIC, so it's picked as the best model
# This is not perfect!!

# ARIMAX Model (this is where these become useful!)
## includes other predictors

data("ntl_icecover")

# convert to dataframe and subset to lake and year range
df_ice <- ntl_icecover %>% 
  as_tibble() %>% 
  filter(between(year, 1980, 2014),
         lakeid == "Lake Mendota") %>% 
  arrange(year)

df_ice %>% 
  ggplot(aes(x = year,
             y = ice_duration)) +
  geom_line(linetype = "dashed") +
  geom_point(alpha = 0.2) +
  theme_bw() +
  labs(x = "Year",
       y = "Duration of Ice")

# download climate data using daymet 
list_mendota <- download_daymet(
  site = "Lake Mendota", #arbitrary name
  lat = 43.1,
  lon = -89.4,
  start = 1980,
  end = 2024,
  internal = TRUE)

df_temp <- list_mendota$data %>% 
  as_tibble() %>%                  # Convert the data to a tibble for tidyverse-friendly operations
  janitor::clean_names() %>%       # Clean column names (lowercase, underscores)
  mutate(
    # Create a proper Date object from year and day-of-year
    date = as.Date(paste(year,
                         yday,
                         sep = "-"),
                   format = "%Y-%j"),
    # Extract the month from the date
    month = month(date)
  ) %>% 
  arrange(year,
          yday) %>% 
  group_by(year) %>% # Group by year
  summarize(temp_min = round(mean(tmin_deg_c), 2)) # Compute average daily minimum temperature

# Join temp and ice cover data

df_ice <- df_ice %>% 
  left_join(df_temp, 
            by = "year")

## Do not try lm(ice_duration ~ temp_min), our data isn't independent
## instead, use autoarima()

obj_arima <- auto.arima(y = df_ice$ice_duration,
           xreg = df_ice$temp_min,
           stepwise = FALSE
           )
# if xreg +/- xreg SE = stays negative or positive, then significant
confint(obj_arima, level = 0.95) 

df_ice %>% 
  ggplot(aes(x = temp_min,
             y = ice_duration)) +
  geom_point(alpha = 0.5)



# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Bison Body Mass, Climate, and Time-Series Analysis
# ============================================================

library(lterdatasampler)


# The "knz_bison" dataset contains long-term monitoring data
# on bison captured at Konza Prairie Biological Station.
#
# ------------------------------------------------------------
# Key columns may include:
# rec_year      : Year of capture
# animal_sex    : Sex of the individual (e.g., female, male)
# animal_weight : Body mass of bison
# ------------------------------------------------------------
#
# In this exercise, you will explore long-term trends in bison
# body mass and evaluate how climate variability may influence
# weight dynamics over time.

# 1. Explore the structure of the knz_bison dataset.
#    - Inspect variable types and missing values.
#    - Reformat variables as needed for analysis.

knz <- knz_bison %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  rename("year" = rec_year,
         "month" = rec_month,
         "day" = rec_day) %>% 
  mutate(date = as.Date(paste(year, month, day, sep = "-")))

# 2. Subset the data to include observations from 1994–2012.

knz <- knz %>% 
  filter(between(year, 1994, 2012))

# 3. Calculate the average body mass for female and male bison
#    for each year in the selected time period.

knz %>% 
  group_by(animal_sex,
           year) %>% 
  summarize(mean_weight = mean(animal_weight, na.rm = T)) %>% 
  ungroup()

# 4. Obtain climate data from the daymetr dataset.
#    - Identify relevant climate variables (e.g., temperature,
#      precipitation).
#    - Associate climate data with knz_bison by year.
#    - Coordinates: Lat 39.09300	Lon -96.57500

list_knz <- download_daymet(
  site = "Konza Prairie", #arbitrary name
  lat = 39.093,
  lon = -96.575,
  start = 1994,
  end = 2012,
  internal = TRUE)

knz_daymet <- list_knz$data %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date(paste(year, yday, sep = "-"),
                        format = "%Y-%d"),
         month = month(date)) %>% 
  arrange(year, yday) %>% 
  group_by(year) %>% 
  summarize(mean_precip = mean(prcp_mm_day),
            temp_min = mean(tmin_deg_c),
            temp_max = mean(tmax_deg_c))

knz_full <- knz %>% 
  left_join(knz_daymet,
            by = "year")

# 5. Perform a time-series analysis to examine whether selected
#    climate variables influence annual bison body mass.
#    - Consider temporal autocorrelation and lag effects.
#    - Model males and females separately

#Females
knz_f <- knz_full %>% 
  filter(animal_sex == "F")

## Precipitation
knz_arima_f1 <- auto.arima(y = knz_f$animal_weight,
                        xreg = knz_f$mean_precip,
                        stepwise = FALSE
)
confint(knz_arima_f1, level = 0.95) # Not significant!

## Min temp
knz_arima_f2 <- auto.arima(y = knz_f$animal_weight,
                           xreg = knz_f$temp_min,
                           stepwise = FALSE
)
confint(knz_arima_f2, level = 0.95) # Significant

## Max temp
knz_arima_f3 <- auto.arima(y = knz_f$animal_weight,
                           xreg = knz_f$temp_max,
                           stepwise = FALSE
)
confint(knz_arima_f3, level = 0.95) # Significant

#Males
knz_m <- knz_full %>% 
  filter(animal_sex == "M")

## Precipitation
knz_arima_m1 <- auto.arima(y = knz_m$animal_weight,
                           xreg = knz_m$mean_precip,
                           stepwise = FALSE
)
confint(knz_arima_m1, level = 0.95) # Significant

## Min temp
knz_arima_m2 <- auto.arima(y = knz_m$animal_weight,
                           xreg = knz_m$temp_min,
                           stepwise = FALSE
)
confint(knz_arima_m2, level = 0.95) # Not significant

## Max temp
knz_arima_m3 <- auto.arima(y = knz_m$animal_weight,
                           xreg = knz_m$temp_max,
                           stepwise = FALSE
)
confint(knz_arima_m3, level = 0.95) # Significant

# 6. Using your fitted model, compare observed bison body mass
#    with predicted values for the period 2014–2020.
#    - Evaluate model performance and discuss sources of uncertainty.
