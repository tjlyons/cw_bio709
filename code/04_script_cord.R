#' DESCRIPTION:
#' Script for Constrained Ordination

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               GGally,
               vegan)

data("varespec", "varechem") # species abundance, environmental data

m_y <- varespec

colnames(m_y) <- str_to_lower(colnames(m_y))

df_env <- as_tibble(varechem) %>% 
  janitor::clean_names()

# visualization

m_y %>% 
  ggpairs(
    column = 1:3,
    aes(alpha = 0.25)
  ) +
  theme_bw()

# RDA
obj_rda <- rda(m_y ~ n + p + ca,
    data = df_env)
 # inertia corresponds to how variable the data is
 # constrained = variance explained by predictors
 # unconstrained = variance not explained by predictors
 
# statistical test
anova.cca(obj_rda,
          by = "margin", # type 2 anova, formula order doesnt affect results
          permutations = 999)
 
#visualization
# community data
df_rda <- scores(obj_rda,
       display = "site",
       scaling = 2) %>% 
  bind_cols(df_env) %>% 
  janitor::clean_names()

# env vectors
df_bp <- scores(obj_rda,
                display = "bp",
                scaling = 2) %>%
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

# Create a ggplot2 ordination plot
# - Points represent sites positioned by their constrained community composition
# - Color gradient reflects the nitrogen (n) concentration at each site
df_rda %>% 
  ggplot(aes(x = rda1,
             y = rda2)) +        # color sites by nitrogen level
  geom_point(aes(color = n)) +
  geom_segment(data = df_bp,
               aes(x = 0, xend = rda1 * 10, # 10 is arbitrary scaling for visualization
                   y = 0, yend = rda2 * 10),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_bp,
            aes(x = rda1 * 10.5,    # slightly beyond arrow tip
                y = rda2 * 10.5,
                label = variable),  # or use a variable column
            size = 4) +
  theme_bw() +
  labs(x = "RDA1",
       y = "RDA2",
       color = "Nitrogen") +
  scale_color_viridis_c()
# RDA falls apart with strong nonlinear relationships (PCA-like)

# dbRDA (distance based) uses direction, allowing use on nonlinear data

obj_db <- dbrda(m_y ~ n + p + ca,
               data = df_env,
               distance = "bray")

anova.cca(obj_db,
          by = "margin",
          permutations = 999) # N is no longer significant, the first method was wrong!

# dbRDA vectors for species data
df_db <- scores(obj_db, 
                display = "sites",
                scaling = 2) %>% 
  as_tibble() %>%              
  bind_cols(df_env) %>%        
  janitor::clean_names()       

# dbRDA vectors for environmental predictors
df_bp <- scores(obj_db, 
                display = "bp", 
                scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

# Create a ggplot2 ordination plot
df_db %>% 
  ggplot(aes(x = db_rda1,
             y = db_rda2)) +        # color sites by nitrogen level
  geom_point(aes(color = n)) +
  geom_segment(data = df_bp,
               aes(x = 0, xend = db_rda1,
                   y = 0, yend = db_rda2),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_bp,
            aes(x = db_rda1 * 1.1,    # slightly beyond arrow tip
                y = db_rda2 * 1.1,
                label = variable),  # or use a variable column
            size = 4) +
  theme_bw() +
  labs(x = "dbRDA1",
       y = "dbRDA2",
       color = "Nitrogen") +
  scale_color_viridis_c()


# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: Community Ordination and Environmental Gradients
# ============================================================

library(vegan)
data("mite", "mite.env")

m_mite <- vegan::wisconsin(mite)

df_mite_env <- as_tibble(mite.env) %>% 
  janitor::clean_names()

# The mite datasets contain information on Oribatid mite communities
# sampled from a small peatland area (2.5 m × 10 m).
#
# There are linked datasets:
# ------------------------------------------------------------
# mite     : Species abundance data (35 mite species × 70 sites)
# mite.env : Environmental variables measured at the same sites
# ------------------------------------------------------------
#
# Environmental variable descriptions (mite.env):
# ------------------------------------------------------------
# SubsDens : Substrate density (g/L)
# WatrCont : Water content of the substrate (g/L)
# Substrate: Substrate type (factor with multiple levels)
# Shrub    : Shrub density (ordered factor: low → high)
# Topo     : Microtopography (Blanket vs Hummock)
# ------------------------------------------------------------

# 1. Explore and visualize interrelationships among species abundances.
#    - Examine patterns of co-occurrence.
#    - Assess whether relationships among species appear linear or nonlinear.

m_mite %>% ggpairs(
  column = 1:3,
  aes(alpha = 0.2)
) +
  theme_bw()

# 2. Fit a redundancy analysis (RDA) model using environmental variables of your choice.
#    - Visualize the ordination results.
#    - Examine gradients and species–environment relationships.
#    - Evaluate whether the assumptions of RDA are appropriate for these data.

obj_mite_rda <- rda(m_mite ~ watr_cont + subs_dens,
                    data = df_mite_env)

# rda for community
df_mite_rda <- scores(obj_mite_rda,
                      display = "site",
                      scaling = 2) %>% 
  bind_cols(df_mite_env) %>% 
  janitor::clean_names()

# rda for environmental predictors
df_mite_bp <- scores(obj_mite_rda,
                     display = "bp",
                     scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

mite_rda <- df_mite_rda %>% 
  ggplot(aes(x = rda1,
             y = rda2)) +        
  geom_point(aes(color = subs_dens)) +
  geom_segment(data = df_mite_bp,
               aes(x = 0, xend = rda1 * 1,
                   y = 0, yend = rda2 * 1),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_mite_bp,
            aes(x = rda1 * 1.1,    # slightly beyond arrow tip
                y = rda2 * 1.1,
                label = variable),  # or use a variable column
            size = 4) +
  theme_bw() +
  labs(x = "RDA1",
       y = "RDA2",
       color = "Substrate Density") +
  scale_color_viridis_c()


# 3. Apply alternative ordination methods.
#    - Canonical correspondence analysis (CCA; see ?cca()).
#    - Distance-based RDA (dbRDA).

#dbRDA

obj_mite_db <- dbrda(m_mite ~ watr_cont + subs_dens,
                                   data = df_mite_env,
                     distance = "bray")

df_mite_db <- scores(obj_mite_db,
                      display = "site",
                      scaling = 2) %>% 
  bind_cols(df_mite_env) %>% 
  janitor::clean_names()

df_mite_db_bp <- scores(obj_mite_db,
                     display = "bp",
                     scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

mite_dbrda <- df_mite_db %>% 
  ggplot(aes(x = db_rda1,
             y = db_rda2)) +        
  geom_point(aes(color = subs_dens)) +
  geom_segment(data = df_mite_db_bp,
               aes(x = 0, xend = db_rda1 * 2,
                   y = 0, yend = db_rda2 * 2),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_mite_db_bp,
            aes(x = db_rda1 * 2.1,    # slightly beyond arrow tip
                y = db_rda2 * 2.1,
                label = variable),  # or use a variable column
            size = 4) +
  theme_bw() +
  labs(x = "dbRDA1",
       y = "dbRDA2",
       color = "Substrate Density") +
  scale_color_viridis_c()


#CCA

obj_mite_cca <- cca(m_mite ~ watr_cont + subs_dens, #CCA Model
                     data = df_mite_env)

df_mite_cca <- scores(obj_mite_cca, # species data
                     display = "site",
                     scaling = 2) %>% 
  bind_cols(df_mite_env) %>% 
  janitor::clean_names()

df_mite_cca_bp <- scores(obj_mite_cca, # environmental predictors
                        display = "bp",
                        scaling = 2) %>% 
  as_tibble(rownames = "variable") %>% 
  janitor::clean_names()

mite_cca <- df_mite_cca %>%  
  ggplot(aes(x = cca1,
             y = cca2)) +        
  geom_point(aes(color = subs_dens)) +
  geom_segment(data = df_mite_cca_bp,
               aes(x = 0, xend = cca1 * 2,
                   y = 0, yend = cca2 * 2),
               arrow = arrow(length = unit(0.2, "cm"))
  ) +
  geom_text(data = df_mite_cca_bp,
            aes(x = cca1 * 2.1,    # slightly beyond arrow tip
                y = cca2 * 2.1,
                label = variable),  # or use a variable column
            size = 4) +
  theme_bw() +
  labs(x = "CCA1",
       y = "CCA2",
       color = "Substrate Density") +
  scale_color_viridis_c()



# 4. Compare RDA, CCA, and dbRDA.
#    - Perform permutation analysis to examine the significance of predictor variables
#    - Discuss which method is most appropriate for these data and why.

anova.cca(obj_mite_rda,
          by = "margin",
          permutations = 999) # both significant, transformed mite less different

anova.cca(obj_mite_db,
          by = "margin",
          permutations = 999) # both significant

anova.cca(obj_mite_cca,
          by = "margin",
          permutations = 999) # both significant

library(patchwork)

(mite_rda + mite_dbrda) / mite_cca 

obj_mite_rda # 15.2% explained by constrained
obj_mite_db # 24.1% explained by constrained
obj_mite_cca # 15.4% explained by constrained

# dbRDA would be the most appropriate, as this dataset is nonlinear, and it has
# the largest proportion of variance explained by constrained ordination.

