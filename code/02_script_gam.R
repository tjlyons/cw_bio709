#' DESCRIPTION:
#' Script for GAMs

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               ggeffects,
               mgcv)

link <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_water_temp.csv"

df_wt_raw <- read_csv(link)

sapply(df_wt_raw, class)

df_wt <- df_wt_raw %>% 
  mutate(date = as.Date(date_time,
                        format = "%m/%d/%Y"), #convert to date 
         year = year(date), # pulls year from date object
         month = month(date)) %>% #pulls month from date object
  filter(year == 2022,
         between(month, 3, 10)) # subsets data from 2022, march thru oct

view(df_wt)

# get daily averages for water temp

df_wt_daily <- df_wt %>% 
  group_by(date, site) %>% 
  summarize(temp = mean(temp, na.rm = TRUE) %>%  round(3))
# na.rm = TRUE ignores NA values, round(3) rounds to 3 dec. places
view(df_wt_daily)

df_wt_daily %>% 
  ggplot(aes(x = date,
             y = temp,
             color = site)) +
  geom_point(alpha = 0.2) + 
  theme_bw() +
  labs(x = "Date",
       y = "Water Temperature (C)",
       color = "Wetland Type")

df_wt_daily <- df_wt_daily %>% 
  mutate(j_date = yday(date), # convert date to Julian day for seasonal trends
         site = factor(site)) # confirm that site data is a factor
df_wt_daily

m_glm <- glm(temp ~ j_date + site,
             data = df_wt_daily,
             family = "gaussian")
summary(m_glm) # temp ~ date ***, temp ~ site nonsig

# using ggpredict() to generate model predictions

df_pred <- ggpredict(m_glm,
                    terms = c(
                      "j_date [all]", # use all values
                      "site [all]")) %>%  # predict for all sites
                   as_tibble() %>% 
  rename(site = group, # rename output columns to match original dataset
         j_date = x)

view(df_pred)

df_wt_daily %>%  
  ggplot(aes(x = j_date,
             y = temp,
             color = site)) +
  geom_point(alpha = 0.2) +
  geom_line(data = df_pred, # overlay predicted values from model
            aes(y = predicted)) + 
  theme_bw() +
  labs(x = "Julian Date",
       y = "Water Temperature (C)",
       color = "Wetland Type")
# Output is too linear for our data

m_gam <- gam(temp ~ site + s(j_date), # s defines smooth terms in gam
             data = df_wt_daily,
             family = "gaussian")
summary(m_gam) # temp ~ site becomes ***! nonlinear seasonality is smoothed s()

df_pred_gam <- ggpredict(m_gam,
                         terms = c(
                           "j_date [all]",
                           "site [all]")) %>% 
  as_tibble() %>% 
  rename(site = group,
         j_date = x)
  df_pred_gam
  
  df_wt_daily %>% 
    ggplot(aes(x = j_date,
               y = temp,
               color = site)) +
    geom_point(alpha = 0.2) +
    geom_line(data = df_pred_gam, # overlaying predicted values (GAM) to daily means
              aes(y = predicted)) +
    theme_bw() + 
    labs(x = "Julian Date",
         y = "Water Temperature (C)",
         color = "Wetland Type")


# lab ---------------------------------------------------------------------

# 1. Read directly from the raw GitHub URL
url <- "https://raw.githubusercontent.com/aterui/public-proj_restore-aqua-complex/v.1.0/data_raw/data_bat.csv"

# Try reading normally
df_bat <- read_csv(url, show_col_types = FALSE)
view(df_bat)

# ============================================================
# DATA GUIDE: Bat Detector Data
# ============================================================

# ----------------------------
# Raw data columns
# ----------------------------

# Site
#   Location where bat detectors are deployed.
#   Levels:
#     "RECCON"  = prairie site without wetland
#     "RECWET"  = prairie site with constructed wetland
#     "WOODCON" = woody site without wetland
#     "WOODWET" = woody site with constructed wetland

# DATE
#   Calendar date of each bat pass record.
#   Expected format: YYYY-MM-DD (verify and standardize).

# TIME
#   Time of bat pass detection.
#   Expected format: HH:MM:SS (verify and standardize).

# AUTO ID*
#   Automatically identified bat species.
#   Species IDs may contain misclassifications or unknown labels
#   that should be carefully reviewed during data cleaning.

# ============================================================
# GOAL 1: Clean data
# ============================================================

# 1. Format column names
#   - Convert column names to a clean format

pacman::p_load(janitor,
               lubridate)

df_bat <- clean_names(df_bat) %>% 
  mutate(date = mdy(date))

# 2. Examine each column carefully
#   - Check for missing values, inconsistent formats, and typos
#   - Confirm DATE and TIME are properly parsed as date/time objects
#   - Inspect AUTO ID values for NA
#   - Remove or correct invalid or unusable records as needed

str(df_bat)

# New derived columns to create:
# Site-level categories:
#   Prairie sites: "RECCON", "RECWET"
#   Woody sites:   "WOODCON", "WOODWET"

# 3. habitat_type
#   Broad site classification:
#     "prairie" = RECCON, RECWET
#     "woody"   = WOODCON, WOODWET

# 4. wetland_status
#   Presence/absence of wetland:
#     "no_wetland" = RECCON, WOODCON
#     "wetland"    = RECWET, WOODWET

# ============================================================
# GOAL 2: Visualize daily bat activity
# ============================================================

# Objective:
#   Quantify and visualize bat activity as the number of bat passes per day.

# Steps:
#   - Aggregate data to calculate daily bat passes
#   - Convert DATE to Julian date
#   - Plot number of bat passes as a function of Julian date
#   - Optionally:
#       * Color or facet plots by site
#       * Smooth trends to visualize seasonal patterns

# ============================================================
# GOAL 3: Model differences among sites
# ============================================================

# Objective:
#   Test whether bat activity differs among the four detector sites.
#   Does the presence of wetland affect bat activity?
#   Is the effect of wetland is site-dependent?

# Modeling considerations:
#   - Response variable: daily bat passes
#   - Predictors may include:
#       * habitat_type
#       * wetland_status
#       * site (four-level factor)
#       * Julian date (to account for seasonality)
#   - Consider appropriate count models
