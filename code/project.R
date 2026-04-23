pacman::p_load(tidyverse,
               glmmTMB,
               lme4,
               janitor,
               patchwork,
               performance,
               MASS)

df_summer <- read_csv("C:\\Users\\teaml\\OneDrive\\Documents\\bio709\\cw_bio709\\code\\su25biomass1.csv")  %>% 
  as_tibble() %>% 
  janitor::clean_names()
 
df_fall <- read_csv("C:\\Users\\teaml\\OneDrive\\Documents\\bio709\\cw_bio709\\code\\fa25biomass1.csv") %>% 
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
m_sum_agb <- lm(total_agb~ treatment * beetle,
                data = df_summer)
summary(m_sum_agb) # * No sig relationship

m_fall_agb <- lm(total_agb~ treatment * beetle,
                data = df_fall)
summary(m_fall_agb) # ***treatment: p = 7.56e-12, slope = 9.879

# total belowground biomass
m_sum_bgb <- lm(total_bgb ~ treatment * beetle,
                data = df_summer)
summary(m_sum_bgb) # microplastics only sig, 0.0274,  slope = 3.2449
m_fall_bgb <- lm(total_bgb ~ treatment * beetle,
                data = df_fall)
summary(m_fall_bgb) # No significant effect

# seed weight
m_sum_seedwt <- lm(seed_weight ~ treatment * beetle,
                    data = df_summer)
summary(m_sum_seedwt) # * beetle, 0.0347 slope = -2.4883
m_fall_seedwt <- lm(seed_weight ~ treatment * beetle,
                   data = df_fall)
summary(m_fall_seedwt) # ***Microplastics: p = 0.00311, slope = 2.6687

## Generalized Linear Model, poisson vs negative binom

# pod count
m_s_pod_num_pois <- glm(pod_num ~ treatment * beetle,
                     data = df_summer,
                     family = poisson)
m_f_pod_num_pois <- glm(pod_num ~ treatment * beetle,
                         data = df_fall,
                         family = poisson)

performance::check_overdispersion(m_s_pod_num_pois) # overdispersed 
performance::check_overdispersion(m_f_pod_num_pois) # NOT overdispersed

m_s_pod_num_nb <- MASS::glm.nb(pod_num ~ treatment * beetle,
                        data = df_summer)

summary(m_s_pod_num_nb) 
summary(m_f_pod_num_pois) 

# seed count

m_s_seed_num_pois <- glm(seed_num ~ treatment * beetle,
                        data = df_summer,
                        family = poisson)
m_f_seed_num_pois <- glm(seed_num ~ treatment * beetle,
                        data = df_fall,
                        family = poisson)

performance::check_overdispersion(m_s_seed_num_pois) # overdispersed 
performance::check_overdispersion(m_f_seed_num_pois) # overdispersed

m_s_seed_num_nb <- MASS::glm.nb(seed_num ~ treatment * beetle,
                               data = df_summer)

m_f_seed_num_nb <- MASS::glm.nb(seed_num ~ treatment * beetle,
                               data = df_fall)

summary(m_s_seed_num_nb) # No significant effect
summary(m_f_seed_num_nb) # Treatment: 3.16e-05, slope = 0.439745

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
