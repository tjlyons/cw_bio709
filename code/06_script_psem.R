#' DESCRIPTION:
#' Script for piecewise SEM

# in-class ----------------------------------------------------------------


pacman::p_load(tidyverse,
               GGally,
               piecewiseSEM,
               glmmTMB)

data("keeley")

(df_keeley <- keeley %>% 
    as_tibble())

# psem()

m1 <- lm(abiotic ~ distance, data = df_keeley)
m2 <- lm(hetero ~ distance, data = df_keeley)
m3 <- lm(firesev ~ age, data = df_keeley)
m4 <- lm(cover ~ firesev, data = df_keeley)
m5 <- lm(rich ~ cover + abiotic + hetero, data = df_keeley)

sem_model <- psem(m1, m2, m3, m4, m5)

summary(sem_model) # if the p-value of fisher c is less than 0.05, the path
# is not  significant, and vise versa (model acurately predicts)

# psem() with non-normal assumption (richness is a discrete variable)

m1 <- lm(abiotic ~ distance, data = df_keeley)
m2 <- lm(hetero ~ distance, data = df_keeley)
m3 <- lm(firesev ~ age, data = df_keeley)
m4 <- lm(cover ~ firesev + hetero, data = df_keeley) # include hetero as a direct effect
m5 <- MASS::glm.nb(rich ~ cover + abiotic + hetero + distance, 
                   data = df_keeley) #modeled as a negative binomial, distance included

sem_model2 <- psem(m1, m2, m3, m4, m5)
summary(sem_model2)


# visualization
plot(sem_model)
plot(sem_model2)

# including random effects

data("shipley")

df_shipley <- shipley %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  drop_na(growth)

df_shipley %>% 
  group_by(site) %>% 
  summarize(n_tree = n_distinct(tree))
            
df_shipley %>% 
  ggpairs(
    columns = c("dd", 
                "date",
                "growth",
                "live")) +
  theme_bw()
                
m1 <- glmmTMB(date  ~ dd +(1 | site) + (1 | tree),
              data = df_shipley,
              family = "gaussian")

m2 <- glmmTMB(growth  ~ date +(1 | site) + (1 | tree),
              data = df_shipley,
              family = "gaussian")

m3 <- glmmTMB(live  ~ growth +(1 | site) + (1 | tree),
              data = df_shipley,
              family = "binomial")

sem_glmm <- psem(m1, m2, m3)

summary(sem_glmm)

# lab ---------------------------------------------------------------------

library(piecewiseSEM)
data("meadows")

df_meadows <- meadows %>% 
  as_tibble()

# =========================================
# EXERCISE: Piecewise SEM with Meadows Data
# =========================================
#
# ------------------------------------------------------------
# Dataset: meadows (from piecewiseSEM package)
# Variables:
#   grazed - 0 = ungrazed, 1 = grazed
#   mass   - plant biomass (g/m²)
#   elev   - plot elevation above sea level
#   rich   - plant species richness per m²
# ------------------------------------------------------------
#
# 1. Explore the dataset (structure, summary, plots).

head(df_meadows)

df_meadows %>% 
  ggpairs() +
  theme_bw()

# 2. Develop a conceptual model: decide which variables influence others.
#    - Consider direct and indirect effects.
#    - Think about grazing as a disturbance factor.

# Grazing would influence plant species richness, as it's the treatment,
# and elevation might also contribute to this. Plant species richness might contribute
# to plant mass.

# 3. Fit component models (e.g., lm) for each hypothesized relationship.

m1 <- lm(mass ~ grazed + rich,
         data = df_meadows)
m2 <- lm(rich ~ grazed + elev,
         data = df_meadows)


# 4. Combine models into a piecewise SEM using psem().

sem_meadow <- psem(m1, m2)

# 5. Evaluate the SEM: path coefficients, significance, variance explained.

summary(sem_meadow)
# This model is not a good fit, as the fisher's C p value is 0.012.

plot(sem_meadow)

# 6. Optional: try alternative models if your model deviates from the expectation.

m1 <- glmmTMB(mass ~ rich + (1 | grazed),
         data = df_meadows,
         family = "gaussian")

m2 <- glmmTMB(rich ~ elev + (1 | grazed),
         data = df_meadows,
         family = "poisson")

sem_meadow2 <- psem(m1, m2)

summary(sem_meadow2) # fisher C p = 0.008, this is worse!

plot(sem_meadow2)

# another model, including grazing as a response beacause why not

m1 <- lm(grazed ~ elev, 
         data = df_meadows)
m2 <- lm(mass ~ grazed + elev + rich,
         data = df_meadows)
m3 <- MASS::glm.nb(rich ~ grazed + elev,
         data = df_meadows)

sem_meadow3 <- psem(m1, m2, m3)

summary(sem_meadow3) # p value is 0 now, this model is even worse.
plot(sem_meadow3)


# another one!

m1 <- lm(rich ~ elev, 
         data = df_meadows)
m2 <- lm(mass ~ grazed + rich,
         data = df_meadows)

sem_meadow4 <- psem (m1, m2)

summary(sem_meadow4)

plot(sem_meadow4)

#
# Deliverables:
# - Code for component models and combined SEM
# - Conceptual SEM diagram
# - Short reasoning about your SEM results
