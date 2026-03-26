# NOTE:
# When instructed to "test XXX", you must report the outcome as comments
# that clearly summarize the relevant statistical results (e.g., effect size,
# direction, significance, and interpretation).
# Providing code alone without documenting and interpreting the results
# in comments will result in point deductions.

pacman::p_load(tidyverse,
               mgcv,
               glmmTMB,
               piecewiseSEM,
               GGally,
               vegan,
               lme4,
               forecast)

# dataset 1 ---------------------------------------------------------------

link1 <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_insect_emergence.rds"
df_emg <- readRDS(url(link1, "rb"))

# This dataset ('df_emg') contains daily measurements of aquatic insect emergence
# from two wetland sites over a full calendar year (Jan 1–Dec 31).

# Data structure:
# t           : Day of the year (integer), where 1 = January 1 and 365 = December 31
# site        : Site identifier (factor), with "s1" and "s2" representing the two wetlands
# emergence   : Emergence flux of aquatic insects (g/day)

# Q1. Visualize seasonal patterns in emergence flux at both sites
#     (e.g., plot emergence vs. day of year, with separate lines or colors for each site).
#     [1 point]

df_emg %>% 
  ggplot(aes(x = t,
             y = emergence,
             color = as.factor(site))) +
  geom_line() +
  theme_bw() +
  labs(x = "Day of Year",
       y = "Emergence Flux (g/day)",
       color = "Site")

# Q2. Test whether emergence flux differs significantly between the two sites,
#     while appropriately accounting for seasonal variation
#     [4 points]

m_emg <- gam(emergence ~ site + s(t),
             data = df_emg)

summary(m_emg)

# The p-value for whether emergence flux differs between sites when accounting for
# seasonal variation is highly significant (7.4e-10). This means that emergence
# flux differs significantly between sites. Additionally, the p-value
# of the smooth term for time is 2e-16, which is also highly significant.

# dataset 2 ---------------------------------------------------------------

link2 <- "https://raw.githubusercontent.com/aterui/cw_bio709/master/data_fmt/data_lake_invert.rds"
df_inv <- readRDS(url(link2, "rb"))

# This dataset 'df_inv' contains 100 observations from 10 lakes.
# Within each lake, 10 plots were established, spaced ~500 m apart.
# At each plot, the following variables were measured:

# s          : Species richness of invertebrates associated with aquatic plants at each plot
# hb         : Standing biomass of invertebrates associated with aquatic plants at each plot
# prod       : Production rate of aquatic plants (macrophytes), measured as g/month
# substrate  : Median diameter of substrate materials (mm)
# cond       : Water electrical conductivity (µS/cm);
#              a proxy for ionized nutrient levels (higher values may indicate eutrophication)
# lake       : lake ID

# Researcher's hypothesis was that: 
# (a) conductivity influences the productivity of macrophyes.
# (b) macrophyte's production rate ('prod') dictates invertebrate biomass ('hb') through bottom-up effects
# (c) macrophyte's production rate ('prod') dictates invertebrate richness ('s') through bottom-up effects 

# Q1. Create a scatter plot of macrophyte production ('prod', y-axis)
#     versus water conductivity ('cond', x-axis), with points colored by lake identity.
#     [1 point]

df_inv %>% 
  ggplot(aes(x = cond,
             y = prod,
             color = as.factor(lake))) +
  geom_point() +
  theme_bw() +
  labs(x = "Conductivity (µS/cm)",
       y = "Productivity (g/month)",
       color = "Lake")

# Q2. Create a scatter plot of raw invertebrate biomass ('hb', y-axis)
#     versus macrophyte production ('prod', x-axis), with points colored by lake identity.
#     [1 point]

df_inv %>% 
  ggplot(aes(x = prod,
             y = hb,
             color = as.factor(lake))) +
  geom_point() +
  theme_bw() +
  labs(x = "Macrophyte Productivity (g/month)",
       y = "Invertebrate Biomass (g)",
       color = "Lake")

# Q3. Create a scatter plot of "log-transformed" invertebrate biomass ('hb', y-axis)
#     versus macrophyte production ('prod', x-axis), with points colored by lake identity.
#     [1 point]

df_inv %>% 
  ggplot(aes(x = prod,
             y = log(hb),
             color = as.factor(lake))) +
  geom_point() +
  theme_bw() +
  labs(x = "Macrophyte Productivity (g/month)",
       y = "Log Transformed Invertebrate Biomass (g)",
       color = "Lake")

# Q4. Test hypothesis (a) by modeling macrophyte production while
#     statistically controlling for potential confounding variables ('substrate', 'lake').
#     [3 points]

m_inv <- glmmTMB(prod ~ cond + (1 | substrate) + (1 | lake),
                 data = df_inv,
                 family = "gaussian")

summary(m_inv)

# The p-value for the GLMM on the effects of Conductivity on Production while 
# including substrate and lake as random effects was 2e-16, which is highly significant.
# There is a positive relationship (slope = 0.48) between conductivity and production.


# Q5. Test hypotheses (a–c) simultaneously using a unified modeling framework.
#     Based on the resulting statistical tests, determine whether the overarching
#     hypothesis (a–c, combined) is supported or rejected.
#     - Use appropriate probability distributions.
#     - Use variable transformation if appropriate given the data.
#     [4 points]

ggpairs(df_inv)

m1 <- glmmTMB(prod ~ cond + (1 | substrate) + (1 | lake),
              data = df_inv,
              family = "gaussian")
m2 <- lm(hb ~ prod + s,
         data = df_inv)
m3 <- lm(s ~ prod,
         data = df_inv)

m_psem <- psem(m1, m2, m3)

plot(m_psem)

## Conductivity has a significant effect on productivity. Productivity has
# a significant effect on both invertebrate biomass and species richness, but 
# species richness does not accurately predict invertebrate biomass.

# dataset 3 ---------------------------------------------------------------

link3 <- "https://raw.githubusercontent.com/aterui/cw_bio709/master/data_fmt/nutrient.rds"
nutrient <- readRDS(url(link3, "rb"))

print(trees)

# This dataset ('trees') contains measurements of 31 felled black cherry trees.
# The three variables represent tree diameter, height, and timber volume.
# Note: the variable 'Girth' is actually the diameter measured at 4 ft 6 in above ground.

# Data structure:
# Girth   : Numeric, tree diameter in inches (mislabelled as girth)
# Height  : Numeric, tree height in feet
# Volume  : Numeric, timber volume in cubic feet

# Q1. Visualize relationships among tree diameter ('Girth'), height ('Height'),
#     and timber volume ('Volume') (e.g., using scatterplot matrix or pairwise scatter plots).
#     [1 point]

ggpairs(trees)

# Q2. Perform an appropriate ordination or dimension reduction method to 
#     summarize these three variables into fewer composite axes.
#     Then, identify and retain axes that explain meaningful variation in the original variables
#     [3 points]

obj_trees <- prcomp(x = trees,
                    scale = TRUE,
                    center = TRUE)
obj_trees
summary(obj_trees)

## PC1 has the highest proportion of variance (80% compared to 19% and 1%).
## Girth and volume contribute the most to PC1 (0.61,0.62) and PC3 (0.68, -0.73),
# and height contributes the most to PC2 (-0.87).

df_trees <- trees %>% 
  bind_cols(obj_trees$x)

# Q3. If justified, test whether the retained axis (or axes) is significantly 
#     related to "nutrient"; 
#     skip regression if the ordination does not support meaningful interpretation.
#     [1 point]

plot(nutrient ~ df_trees$PC1)

m_trees <- lm(nutrient ~ PC1,
              data = df_trees)

summary(m_trees)

## With a p-value of 7.7e-6, PC1 is significantly related to nutrients. The slope
## of this relationship is approximately 2.4.

# dataset 4 ---------------------------------------------------------------

df_nile <- dplyr::tibble(
  year = time(Nile), # observation year
  discharge = as.numeric(Nile) # discharge
)

df_sunspot <- dplyr::tibble(
  year = time(sunspot.year), # observation year
  sunspots = as.numeric(sunspot.year) # the number of sunspots
)

# These datasets contain:
# - df_nile    : Annual discharge of the Nile River (Nile dataset)
# - df_sunspot : Annual sunspot counts (sunspot.year dataset)

# Q1. Create a combined data frame aligning the observation years
#     (i.e., only include years present in both datasets)
#     [1 point]

df_river <- left_join(df_nile, df_sunspot, by = "year")

# Q2. Test whether the number of sunspots is significantly related to Nile's discharge
#     [4 points]

df_river %>% 
  ggplot(aes(x = sunspots,
             y = discharge)) +
  geom_line()

m_arima <- auto.arima(y = df_river$discharge,
                       xreg = df_river$sunspots,
                       stepwise = FALSE)

confint(m_arima, level = 0.95)

## Because the range of xreg within a 95% confidence interval is -0.58 and 1.08,
## the relationship between the amount of sunspots and the amount of river 
## discharge is not significant. If both bounds were negative or positive, then
## it would be.
