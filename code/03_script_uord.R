#' DESCRIPTION:
#' Script for Unconstrained Ordination

# in-class ----------------------------------------------------------------

pacman::p_load(tidyverse,
               GGally,
               vegan)

# PCA

df_iris <- iris %>% 
  as_tibble() %>% 
  janitor::clean_names()
view(df_iris)

# visualization

df_iris %>% 
  ggpairs(columns = c("sepal_length",
                      "sepal_width",
                      "petal_length",
                      "petal_width"),
          aes(color = species,
              alpha = 0.5)) +
  theme_bw()
          
df_petal <- df_iris %>% 
  select(starts_with("petal_"))

# PCA using prcomp()

obj_pca <- prcomp(x = df_petal,
       center  = TRUE,
       scale = TRUE)
obj_pca
summary(obj_pca)

# extracting pc values

df_pca <- df_iris %>% 
  bind_cols(obj_pca$x)

df_pca %>% 
  ggplot(aes(x = species,
             y = PC1,
             color = species)) +
           geom_boxplot()

# NMDS

data(dune) # dune dataset from vegan package

dune %>% 
  as_tibble() %>% 
  select(1:3) %>% 
  ggpairs() +
  theme_bw()

m_bray <- vegdist(dune, # calculate distance between units
                  method = "bray") # bray-curtis dissimilarity

obj_nmds <- metaMDS(m_bray, #input
        k = 2) # number of dimensions
# using "dune" as input works, but better to specify distance

data(dune.env)
head(dune.env)

df_nmds <- dune.env %>%  
  as_tibble() %>% 
  bind_cols(obj_nmds$points) %>% 
  janitor::clean_names()

df_nmds %>% 
  ggplot(aes(x = mds1,
             y = mds2,
             color = use)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95, # 95% conf interval
               linetype = 2) + # dashed line
  theme_bw() +
  labs(x = "NMDS 1", 
       y = "NMDS 2",
       color = "Land Use")

# Using PERMANOVA on NMDS

adonis2(m_bray ~ use,
                  data = df_nmds)
# not significant

# lab ---------------------------------------------------------------------

# ============================================================
# EXERCISE: PCA using the iris dataset
# ============================================================

# In this exercise, you will perform a Principal Component
# Analysis (PCA) using all morphological measurements in the
# iris dataset and visualize multivariate trait patterns
# among species.

# 1. Using all four morphological variables
#    (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
#    perform a PCA.

df_iris2 <- df_iris %>% 
  select(!species)

obj_pca2 <- prcomp(x = df_iris2,
       center = TRUE,
       scale = TRUE)

obj_pca2
summary(obj_pca2) #PC1: 73%, PC2:23%

# 2. Visualize flower morphology in PC axes whose cumulative
#    contribution exceeds 90%; color points by species.

df_pca2 <- bind_cols(df_iris, obj_pca2$x)
view(df_pca2)

df_pca2 %>% 
  ggplot(aes(x = PC1,
             y = PC2,
             color = species)) +
  geom_point()



# 3. Which morphological traits contribute most strongly to
#    the first and second principal components? How?

# Sepal length and petal length and width contribute the most to PC1, and sepal
# width contributes the most to PC2. This can be seen in the rotation values for
# each, which approach 1 or -1 depending on the strength of their correlation.

# ============================================================
# EXERCISE: NMDS using the BCI dataset
# ============================================================

# In this exercise, you will perform a Non-metric Multidimensional
# Scaling (NMDS) using the BCI tree community dataset and explore
# patterns in species composition among sites.

data("BCI", "BCI.env")

# 1. Using the BCI dataset, calculate a dissimilarity matrix
#    (e.g., Bray-Curtis) and perform NMDS.


BCI <- BCI %>% 
  as_tibble() %>% 
  janitor::clean_names()

m_bray2 <- vegdist(BCI,
                   method = "bray")

obj_nmds2 <- metaMDS(m_bray2,
                     k = 2)
obj_nmds2

# 2. Visualize sites in NMDS space.
#    - How are sites positioned relative to each other?
#    - Color or shape points by environmental groups or site
#      characteristics of your choice.

bci_nmds <- BCI.env %>%
  as_tibble() %>% 
  bind_cols(obj_nmds2$points) %>% 
  janitor::clean_names()
  
head(bci_nmds)

bci_nmds %>% 
  ggplot(aes(x = mds1,
             y = mds2,
             color = habitat)) +
  geom_point(size = 3) +
  stat_ellipse(level = 0.95,
               linetype = 2) +
  theme_bw()

# 3. Perform PERMANOVA to examine if communities are grouped
#    by the environmental variable you selected.


adonis2(m_bray2 ~ habitat,
       data = bci_nmds) # ***
