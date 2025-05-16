######## Code for the simulation study figures #########

# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Reading the datasets
## Optimal
optimal_model_estimates_prob <- readRDS(file = "opt_est.rds")
## One missing category
mis1_model_estimates_prob <- saveRDS(file = "mis1_est.rds")
## Two missing categories
mis2_model_estimates_prob <- saveRDS(file = "mis2_est.rds")
## Hierarchical
hier_model_estimates_prob <- saveRDS(file = "hier_est.rds")

# Box plot codes
## Optimal
opt_box <- optimal_model_estimates_int %>%
  ungroup() %>%
  complete(category, x, model, sim_id,
           fill = list(est = 0, lwr95 = 0, upr95 = 0, lwr50 = 0, upr50 = 0)) %>%
  left_join(p2, by = c("category", "x"), suffix = c("_sim", "_true")) %>%
  mutate(sq_error = (est_sim - est_true)^2) %>%
  group_by(sim_id, model, category) %>%
  summarise(mse = mean(sq_error), .groups = "drop") %>%
  ggplot(aes(x = category, y = mse, fill = model)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = model), shape = 21, fill = "white", stroke = 0.5, size = 2,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.52), alpha = 0.35) +
  theme_bw(base_size = 14) +
  labs(x = "Category", y = "MSE", title = "Optimal situation") +
  guides(fill = guide_legend(title = "Model"),
         color = guide_legend(title = "Model")) +
  theme(legend.position = c(0.9,0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"))

ggsave(filename = "opt_box.png", plot = opt_box,
       width = 2550, height = 1350, units = "px")

## One missing category
mis1_box <- mis1_model_estimates_int %>%
  ungroup() %>%
  complete(category, x, model, sim_id,
           fill = list(est = 0, lwr95 = 0, upr95 = 0, lwr50 = 0, upr50 = 0)) %>%
  left_join(p2, by = c("category", "x"), suffix = c("_sim", "_true")) %>%
  mutate(sq_error = (est_sim - est_true)^2) %>%
  group_by(sim_id, model, category) %>%
  summarise(mse = mean(sq_error), .groups = "drop") %>%
  ggplot(aes(x = category, y = mse, fill = model)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = model), shape = 21, fill = "white", stroke = 0.5, size = 2,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.52), alpha = 0.35) +
  theme_bw(base_size = 14) +
  labs(x = "Category", y = "MSE", subtitle = "One category with no responses") +
  guides(fill = guide_legend(title = "Model"),
         color = guide_legend(title = "Model")) +
  theme(legend.position = c(0.1,0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"))

ggsave(filename = "mis1_box.png", plot = mis1_box,
       width = 2550, height = 1350, units = "px")

## Two missing categories
mis2_box <- mis2_model_estimates_int %>%
  ungroup() %>%
  complete(category, x, model, sim_id,
           fill = list(est = 0, lwr95 = 0, upr95 = 0, lwr50 = 0, upr50 = 0)) %>%
  left_join(p3, by = c("category", "x"), suffix = c("_sim", "_true")) %>%
  mutate(sq_error = (est_sim - est_true)^2) %>%
  group_by(sim_id, model, category) %>%
  summarise(mse = mean(sq_error), .groups = "drop") %>%
  ggplot(aes(x = category, y = mse, fill = model)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = model), shape = 21, fill = "white", stroke = 0.5, size = 2,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.52), alpha = 0.35) +
  theme_bw(base_size = 14) +
  labs(x = "Category", y = "MSE", subtitle = "Two categories with no responses") +
  guides(fill = guide_legend(title = "Model"),
         color = guide_legend(title = "Model")) +
  theme(legend.position = c(0.1,0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"))

ggsave(filename = "mis2_box.png", plot = mis2_box,
       width = 2550, height = 1350, units = "px")

# Hierarchical setting
hier_box <- hier_model_estimates_int %>%
  ungroup() %>%
  complete(category, x, model, sim_id,
           fill = list(est = 0, lwr95 = 0, upr95 = 0, lwr50 = 0, upr50 = 0)) %>%
  left_join(phl, by = c("category", "x", "city"), suffix = c("_sim", "_true")) %>%
  mutate(sq_error = (est_sim - est_true)^2) %>%
  group_by(sim_id, model, category) %>%
  summarise(mse = mean(sq_error), .groups = "drop") %>%
  ggplot(aes(x = category, y = mse, fill = model)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = model), shape = 21, fill = "white", stroke = 0.5, size = 2,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.52), alpha = 0.35) +
  theme_bw(base_size = 14) +
  labs(x = "Category", y = "MSE", subtitle = "Hierarchical scenario") +
  guides(fill = guide_legend(title = "Model"),
         color = guide_legend(title = "Model")) +
  theme(legend.position = c(0.1,0.8),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"))

ggsave(filename = "hier_box.png", plot = hier_box,
       width = 2550, height = 1350, units = "px")
