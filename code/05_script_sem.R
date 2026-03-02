#' DESCRIPTION:
#' Script for SEM

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               GGally,
               vegan,
               lavaan,
               lavaanPlot)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_foodweb.csv"

(df_fw <- read_csv(url))

# Visualization

df_fw %>% 
  select(-plot_id) %>% 
  ggpairs() +
  theme_bw()

# path analysis

# Specify the SEM model using lavaan syntax
# Note: the variable names must exactly match the column names in the dataframe (df_fw)
m1 <- '
  # regression of herbivore biomass on plant variables
  mass_herbiv ~ mass_plant + cv_h_plant
  # regression of predator biomass on herbivore biomass
  mass_pred ~ mass_herbiv
'

fit1 <- sem(model = m1,
    data = df_fw)

# output interpretation: P-value > 0.05 = significant model (it's backwards!!)
# (is the model significantly different from the data? if so, that's bad!)

summary(fit1, standardize = TRUE) # need to standardize different measurements

lavaanPlot(model = fit1, coefs = TRUE, stand = TRUE)

# Model comparison (including cv_h_plant -> mass_pred)

m2 <- '
mass_herbiv ~ mass_plant + cv_h_plant
mass_pred ~ mass_herbiv + cv_h_plant'

fit2 <- sem(model = m2,
            data = df_fw)

summary(fit2, standardize = TRUE)

lavaanPlot(model = fit2, coefs = TRUE, stand = TRUE)

anova(fit1, fit2) # p > 0.05, models are not significantly different from eachother
# BUT, fit1 is simpler but retains a similar explanatory capacity

# Structural Equation Modelling (SEM)
# latent variables (not observable, only exists within stats)

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_herbivory.csv"

(df_herbv <- read_csv(url))

df_herbv %>% 
  ggpairs(columns = c("soil_n",
                      "per_lignin",
                      "sla",
                      "cn_ratio")) +
  theme_bw()

# Creating a latent variable

m_sem <- '
# latent variable
  palatability =~ sla + cn_ratio + per_lignin
  
# regression
  palatability ~ soil_n
  herbivory ~ palatability
'


(fit_sem <- sem(model = m_sem,
               data = df_herbv))

summary(fit_sem, standardize = TRUE)

# SEM assumes normal distribution! (specifically the creation of a latent variable)

# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Path Analysis and Covariance Visualization
# ============================================================

library(piecewiseSEM)
data("keeley")  
keeley <- as_tibble(keeley)

# The "keeley" dataset contains fire-related vegetation data
# collected from shrublands in California.
#
# ------------------------------------------------------------
# Column descriptions:
# elev  : Elevation of the site
# slope : Slope steepness
# aspect: Slope aspect (orientation)
# heat  : Heat load index (a function of slope and aspect)
# firesev: Fire severity
# age   : Time since last fire
# cover : Vegetation cover
# rich  : Plant species richness
# ------------------------------------------------------------
#
# In this exercise, you will explore relationships among variables
# using covariance and path analysis. You will replicate a published
# path model and propose an alternative.

# 1. For the variables depicted in Figure 22.1, draw a figure
#    showing the covariance between variables.

keeley %>% 
  ggpairs(columns = c("distance",
                      "age",
                      "firesev",
                      "cover",
                      "hetero",
                      "abiotic",
                      "rich")) +
  theme_bw()

# 2. Following Figure 22.1, develop a path model using the
#    same variables and relationships. Examine if this model
#    captures the data structure using a Chi-Square test.

m_keeley <- '
abiotic ~ distance
hetero ~ distance
age ~ distance
firesev ~ age
cover ~ firesev
rich ~ abiotic + hetero + cover
'

fit_keeley1 <- sem(model= m_keeley,
    data = keeley)

summary(fit_keeley1, standardize = TRUE)

lavaanPlot(fit_keeley1, coefs = TRUE, stand = TRUE)

# The model does not capture the data structure using a chi square test,
# as p = 0.03

# 3. Develop an alternative path model that you consider more
#    appropriate based on theory or observed data patterns.

m_keeley2 <- '
abiotic ~ distance
hetero ~ distance + cover
age ~ distance
firesev ~ distance + age
cover ~ firesev
rich ~ abiotic + hetero + cover
'

fit_keeley2 <- sem(model= m_keeley2,
                   data = keeley)

summary(fit_keeley2, standardize = TRUE)

lavaanPlot(fit_keeley2, coefs = TRUE, stand = TRUE)

# 4. Compare the performance of the published model (Figure 22.1)
#    and your alternative model.
#    - Consider fit indices, path coefficients, and interpretability.

anova(fit_keeley1, fit_keeley2) # p = 0.006, so my two models are significantly 
# different. Model 2 performs better than model 1.
