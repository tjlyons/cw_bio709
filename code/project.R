pacman::p_load(tidyverse,
               glmmTMB,
               lme4,
               janitor,
               patchwork)

df_summer <- read_csv("su25biomass1.csv")  %>% 
  as_tibble() %>% 
  select(-Nodule_num) %>% 
  janitor::clean_names()
 
df_fall <- read_csv("fa25biomass1.csv") %>% 
  as_tibble() %>% 
  janitor::clean_names()

## Visualize distribution of variables

# total aboveground biomass
df_summer %>% 
  ggplot(aes(x = total_agb)) +
  geom_histogram(bins = 20)
df_fall %>% 
  ggplot(aes(x = total_agb)) +
  geom_histogram(bins = 20)

# total belowground biomass
df_summer %>% 
  ggplot(aes(x = total_bgb)) +
  geom_histogram(bins = 20)
df_fall %>% 
  ggplot(aes(x = total_bgb)) +
  geom_histogram(bins = 20)

# seed weight
df_summer %>% 
  ggplot(aes(x = seed_weight)) +
  geom_histogram(bins = 20)
df_fall %>% 
  ggplot(aes(x = seed_weight)) +
  geom_histogram(bins = 20)

# pod count
df_summer %>% 
  ggplot(aes(x = pod_num)) +
  geom_histogram(bins = 20)
df_fall %>% 
  ggplot(aes(x = pod_num)) +
  geom_histogram(bins = 20)

# seed count
df_summer %>% 
  ggplot(aes(x = seed_num)) +
  geom_histogram(bins = 20)
df_fall %>% 
  ggplot(aes(x = seed_num)) +
  geom_histogram(bins = 20)

## Linear Models

# total aboveground biomass
m_sum_agb <- lm(total_agb~ treatment + beetle,
                data = df_summer)
summary(m_sum_agb) # *treatment: p = 0.0303, slope = 5.121

m_fall_agb <- lm(total_agb~ treatment + beetle,
                data = df_fall)
summary(m_fall_agb) # ***treatment: p = 1.57e-9, slope = 11.313

# total belowground biomass
m_sum_bgb <- lm(total_bgb ~ treatment + beetle,
                data = df_summer)
summary(m_sum_bgb) # No significant effect
m_fall_bgb <- lm(total_bgb ~ treatment + beetle,
                data = df_summer)
summary(m_fall_bgb) # No significant effect

# seed weight
m_sum_seedwt <- lm(seed_weight ~ treatment + beetle,
                    data = df_summer)
summary(m_sum_seedwt) # ***Microplastics: p = 2.68-5, slope = 3.145,
                      # *Beetles: p = 0.0256, slope = -1.8568
m_fall_seedwt <- lm(seed_weight ~ treatment + beetle,
                   data = df_fall)
summary(m_fall_seedwt) # ***Microplastics: p = 4.08e-7, slope = 3.149,
                       # *Beetles: p = 0.0165, slope = 1.3375

## Generalized Linear Mixed-Effects Model, poisson vs negative binom

# pod count
m_s_pod_num <- glmmTMB(pod_num ~ treatment + beetle + (1 | plant_id),
                     data = df_summer,
                     family = poisson)
m_f_pod_num <- glmmTMB(pod_num ~ treatment + beetle + (1 | plant_id),
                         data = df_fall,
                         family = poisson)

sum(residuals(m_s_pod_num, type = "pearson")^2) / df.residual(m_s_pod_num) # 0.576
sum(residuals(m_f_pod_num, type = "pearson")^2) / df.residual(m_f_pod_num) # 1.005

summary(m_s_pod_num) # ** Treatment: p = 0.0067, slope = 0.1684
summary(m_f_pod_num) # *** Treatment: p= 4.02e-14, slope = 0.4216

# seed count
m_s_seed_num <- glmmTMB(seed_num ~ treatment + beetle + (1 | plant_id),
                         data = df_summer,
                         family = poisson)
m_f_seed_num <- glmmTMB(seed_num ~ treatment + beetle + (1 | plant_id),
                         data = df_fall,
                         family = poisson)

sum(residuals(m_s_seed_num, type = "pearson")^2) / df.residual(m_s_seed_num) # 0.354
sum(residuals(m_f_seed_num, type = "pearson")^2) / df.residual(m_f_seed_num) # 0.518

summary(m_s_seed_num) # *** Treatment: p = 0.00092, slope = 0.2336
summary(m_f_seed_num) # *** Treatment: p= 1.54e-11, slope = 0.4335

## Visualization

# Aboveground biomass
sum_agb <- df_summer %>% 
  ggplot(aes(x = treatment,
             y = total_agb,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Total Aboveground Biomass (g)",
       title = "Summer 2025 AGB",
       color = "Beetle Present")

fall_agb <- df_fall %>% 
  ggplot(aes(x = treatment,
             y = total_agb,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Total Aboveground Biomass (g)",
       title = "Fall 2025 AGB",
       color = "Beetle Present")

sum_agb + fall_agb

# Belowground biomass
sum_bgb <- df_summer %>% 
  ggplot(aes(x = treatment,
             y = total_bgb,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Total Belowground Biomass (g)",
       title = "Summer 2025 BGB",
       color = "Beetle Present")

fall_bgb <- df_fall %>% 
  ggplot(aes(x = treatment,
             y = total_bgb,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Total Belowground Biomass (g)",
       title = "Fall 2025 BGB",
       color = "Beetle Present")

sum_bgb + fall_bgb

# Seed weight
sum_seedwt <- df_summer %>% 
  ggplot(aes(x = treatment,
             y = seed_weight,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Seed Weight (g)",
       title = "Summer 2025 Seed Weight",
       color = "Beetle Present")

fall_seedwt <- df_fall %>% 
  ggplot(aes(x = treatment,
             y = seed_weight,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Seed Weight (g)",
       title = "Fall 2025 Seed Weight",
       color = "Beetle Present")

sum_seedwt + fall_seedwt

# Pod count
sum_podnum <- df_summer %>% 
  ggplot(aes(x = treatment,
             y = pod_num,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Pod Count",
       title = "Summer 2025 Pod Count",
       color = "Beetle Present")

fall_podnum <- df_fall %>% 
  ggplot(aes(x = treatment,
             y = pod_num,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Pod Count",
       title = "Fall 2025 Pod Count",
       color = "Beetle Present")

sum_podnum + fall_podnum

# Seed count
sum_seednum <- df_summer %>% 
  ggplot(aes(x = treatment,
             y = seed_num,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Seed Count",
       title = "Summer 2025 Seed Count",
       color = "Beetle Present")

fall_seednum <- df_fall %>% 
  ggplot(aes(x = treatment,
             y = seed_num,
             color = beetle)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "See Count",
       title = "Fall 2025 Seed Count",
       color = "Beetle Present")

sum_seednum + fall_seednum
