######  ######

# Libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)

# Loading the data
## How is the live music scene
Q11_gender_mean_cate <- readRDS(file = "Q11_gender_mean_cate")
### Age group mean category
Q11_age_group_mean_cate <-readRDS(file = "Q11_age_group_mean_cate")
## Coef estimates
Q11_coefs_whole <- readRDS(file = "Q11_coefs_whole")
## City estimates
Q11_city_estimates <- readRDS(file = "Q11_city_estimates")

## How was your night datasets
### Gender mean category
Q9_gender_mean_cate <- readRDS(file = "Q9_gender_mean_cate")
### Age group mean category
Q9_age_group_mean_cate <-readRDS(file = "Q9_age_group_mean_cate")
## Coef estimates
Q9_coefs_whole <- readRDS(file = "Q9_coefs_whole")
## City estimates
Q9_city_estimates <- readRDS(file = "Q9_city_estimates")

# Mean category plots for "How is the live music scene in your city?" question
## Age group
p_Q11_Age_effect <- Q11_gender_mean_cate %>%
  ggplot(aes(x = mean_cat, y = Age_group)) +
  stat_interval() +
  scale_color_grey(end = 0.1, start = 0.6, name = "Credible\ninterval") +
  theme_bw(base_size = 14) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(x = "Mean category", y = "Age group")

## Gender
p_Q11_Q2_effect <- Q11_age_group_mean_cate %>%
  ggplot(aes(x = mean_cat, y = Q2)) +
  stat_interval() +
  scale_color_grey(end = 0.1, start = 0.6, name = "Credible\ninterval") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) +
  labs(x = "Mean category", y = "Gender")

# Compining the plots
Q11_coef_effects <- cowplot::plot_grid(p_Q11_Age_effect, p_Q11_Q2_effect)

# Saving the plot
ggsave(filename = "Q11_coef_effects.png", plot = Q11_coef_effects,
       width = 2550, height = 1350, units = "px")

# Mean category plots for "How was your night?" question
## Age group
p_Q9_Age_effect<- Q9_gender_mean_cate %>%
  ggplot(aes(x = mean_cat, y = Age_group)) +
  stat_interval() +
  scale_color_grey(end = 0.1, start = 0.6, name = "Credible\ninterval") +
  theme_bw(base_size = 14) +
  theme(legend.key.size = unit(0.3, "cm")) +
  labs(x = "Mean category", y = "Age group")

# Gender
p_Q9_Q2_effect <- Q9_age_group_mean_cate %>%
  ggplot(aes(x = mean_cat, y = Q2)) +
  stat_interval() +
  scale_color_grey(end = 0.1, start = 0.6, name = "Credible\ninterval") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) +
  labs(x = "Mean category", y = "Gender")

# Compining the plots
Q9_coef_effects <- cowplot::plot_grid(p_Q9_Age_effect, p_Q9_Q2_effect)

# Saving the plot
ggsave(filename = "Q9_coef_effects.png", plot = Q9_coef_effects,
       width = 2550, height = 1350, units = "px")

# Coef effect plot
coef_comp_whole <- bind_rows(Q11_coefs_whole, Q9_coefs_whole) %>%
  filter(!term %in% c("1|2", "2|3", "3|4", "4|5", "sd")) %>%
  mutate(term = case_match(term,
                           "Q2Male" ~ "Male",
                           "Q2Another gender identity" ~ "Another gender\nidentity",
                           "Age_group18-29" ~ "Age: 18-29",
                           "Age_group30-49" ~ "Age: 30-49",
                           "Age_group50-64" ~ "Age: 50-64")) %>%
  ggplot(aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0,
                position = position_dodge(width = 0.4), size = 0.5) +
  geom_errorbar(aes(xmin = conf.low50, xmax = conf.high50), width = 0,
                position = position_dodge(width = 0.4), size = 1.5) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) +
  labs(x = "Estimate", y = "Coefficent",
       color = "Model", alpha = "Data type", fill = "Data type") +
  scale_color_grey(start = 0.1, end = 0.6) + facet_wrap(~Question) +
  geom_vline(xintercept = 0, linetype = 2, color = "red")

# Saving the plot
ggsave(filename = "coef_comp_whole.png", plot = coef_comp_whole,
       width = 2550, height = 1350, units = "px")

# City comparison plots
p_Q9_Q11_city_est <- bind_rows(Q11_city_estimates, Q9_city_estimates) %>%
  ggplot(aes(x = .category, y = est, color = City)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0,
                position = position_dodge(width = 0.4), size = 0.5)  +
  geom_errorbar(aes(ymin = lwr50, ymax = upr50), width = 0,
                position = position_dodge(width = 0.4), size = 1.5) +
  theme_bw(base_size = 14) + facet_wrap(~Question) +
  theme(legend.position = c(0.1,0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) +
  labs(x = "Answer category", y = "Probability",
       color = "City") +
  scale_y_continuous(label = scales::percent)

# Saving the plot
ggsave(filename = "Q9_Q11_city_est.png", plot = p_Q9_Q11_city_est,
       width = 2550, height = 1350, units = "px")
