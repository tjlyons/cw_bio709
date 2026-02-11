#' DESCRIPTION:
#' Script for GLMMs

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               lme4,
               glmmTMB,
               janitor)

# Group Structure

data(Owls)
df_owl_raw <- as_tibble(Owls)
view(df_owl_raw)

df_owl <- df_owl_raw %>% 
  janitor::clean_names() %>% # standardize col names
  mutate(across(.cols = where(is.factor), # select factor-type cols
                .fns = str_to_lower)) # convert factors to lowercase
view(df_owl)

df_owl %>%  # Generating box plot forg negotions per chick
  ggplot(aes(x = food_treatment,
             y = neg_per_chick)) +
  geom_boxplot(outliers = FALSE) + 
  geom_jitter(alpha = 0.25) + 
  theme_bw()

v_g9 <- unique(df_owl$nest)[1:9] # first 9 nest IDs
v_g9
df_owl %>% 
  filter(nest %in% v_g9) %>% # Select only data from first 9 nests
  ggplot(aes(x = food_treatment,
             y = neg_per_chick))+
  geom_jitter(alpha = 0.25,
              width = 0.1) + 
  facet_wrap(facets =~ nest, # Separate panels per nest
             ncol = 3,
             nrow = 3) +
  theme_bw()

m_glm <- MASS::glm.nb(sibling_negotiation ~ food_treatment +
                        nest +
                        offset(log(brood_size)),
                      data = df_owl)
# Random Intercept

m_ri <- glmmTMB(
  sibling_negotiation ~ # response
    food_treatment + # fixed effect
    (1 | nest) + # random intercept for each nest
    offset(log(brood_size)), # offset to model rate per chick
  data = df_owl,
  family = nbinom2()) # negative binomial dist
summary(m_ri)

head(coef(m_ri)$cond$nest) # group specific intercepts

m_ris <- glmmTMB(sibling_negotiation ~ food_treatment +
                   (1 + food_treatment | nest) + # random int/slope per nest
                   offset(log(brood_size)), # rate per chick
                 data = df_owl,
                 family= nbinom2())
summary(m_ris)

head(coef(m_ris)$cond$nest)


# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: GLMM Exercise - `sleep` Data Set
# ============================================================

# ------------------------------------------------------------
# sleep dataset (built-in R dataset)
#
# This dataset contains measurements of increased sleep time
# after administering two different drugs.
#
# Structure:
# - 20 observations total
# - 10 subjects (each measured twice)
# - Paired / repeated-measures design
#
# Variables:
#   extra : Increase in hours of sleep compared to baseline
#   group : Indicates which drug was administered (factor with two levels ("1", "2"))
#   ID    : factor identifying each subject; Each subject appears once in each group
# ------------------------------------------------------------

view(sleep)

# Q1 – Visualization:
# Compare sleep increase ("extra") between the two drug groups.
#
# Goals:
# - Show individual-level responses
# - Highlight paired structure (same subject in both groups)
# - Use color to identify subjects
# - Connect observations from the same subject using lines

sleep %>% 
  ggplot(aes(x = group,
             y = extra,
             color = ID,
             group = ID)) +
         geom_point() +
           geom_line() +
  labs( x = "Treatment Group",
        y = "Extra sleep (hours)")

# Q2 - Model development:
#
# Goal:
#   Examine how drug administration affects sleep duration.
#
# Key considerations:
#   - Response variable (extra) is continuous
#   - Drug (group) represents the treatment of interest
#   - Subject ID represents repeated measurements on the same
#     individuals

lm_sleep <- lm(extra ~ group, data = sleep)
summary(sleep) # bad, since repeat measurements (paired)

lmm_sleep <- lmer(extra ~ group + (1|ID), data = sleep) # We need random intercept for ID, right?
summary(lmm_sleep) # Drug 2 increases mean sleep by 1.6 hrs more than drug 1 on average
# after random effects

# ============================================================
# EXERCISE: GLMM Exercise - `grouseticks` Data Set
# ============================================================

library(lme4)
data("grouseticks")

# ------------------------------------------------------------
# grouseticks dataset (from lme4 package)
#
# This dataset contains counts of parasitic ticks
# collected from red grouse chicks across multiple years
# and locations in Scotland.
#
# Structure:
# - 403 observations
# - Repeated measurements across broods and years
# - Count data with hierarchical (nested) structure
#
# Variables:
#   TICKS : Number of ticks counted on a chick
#   YEAR  : Sampling year
#   HEIGHT: height above sea level (meters)
#   LOCATION : Sampling site (grouping variable)
#   INDEX : Observation-level identifier
#
# Key features of the dataset:
# - Response variable is count data
# - Observations are grouped by brood and year
# ------------------------------------------------------------

# Q1 – Visualization:
#
# Goal:
#   Examine the relationship between parasite load (ticks) at the brood level and height above sea level.
#
# Key considerations:
# - Calculate average tick counts for each brood
# - Plot mean ticks vs. height
# - Color points by sampling year

view(grouseticks)
grouseticks <- grouseticks %>%
  clean_names(case = "snake") # I think this makes it lowercase?

brood_ticks_mu <- grouseticks %>% 
  group_by(brood, height, year) %>% 
  summarize(mu_ticks = mean(ticks))
view(brood_ticks_mu)

brood_ticks_mu %>% 
  ggplot(aes(x = height,
              y = mu_ticks,
             color = factor(year))) +
  geom_point() +
  labs(x = "Height (m)",
       y = "Mean Ticks per Brood")

# Q2 – Model development:
#
# Goal:
#   Develop a model to examine the relationship between parasite load (ticks) at the brood level and height above sea level.
#
# Key considerations:
#   - Response variable (TICKS) is count
#   - HEIGHT represents the variable of interest
#   - BROOD represents a grouping factor of repeated measurements
#   - YEAR represents another grouping factor of repeated measurements

glmm_grouseticks <- glmer(ticks ~ height + (1|brood) + (1|year),
                          data = grouseticks,
                          family = poisson) # Unsure if poisson is the way to go
summary(glmm_grouseticks) # Height has a significant effect on mean ticks per brood
# when accounting for year and brood as random effects
