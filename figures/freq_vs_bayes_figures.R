######### Comparison figures for the live music census questions ##########

# Libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Loading the data
census_data_all <- read_sav("combined_census_data.sav") %>%
  filter(exclude == 0) %>%
  mutate(Q2 = haven::as_factor(Q2),
         Age_group = case_when(
           Age < 18 ~ "<18",
           Age > 17 & Age < 30 ~ "18-29",
           Age > 29 & Age < 50 ~ "30-49",
           Age > 49 & Age < 65 ~ "50-64",
           Age > 64 ~ ">64"
         ))
## How would you rate the live music scene data
## Bayes
## Aggregate intervals
Q11_b_int <- readRDS(file = "Q11_b_int")
### Gender intervals
Q11_b_int_Q2 <- readRDS(file = "Q11_b_int_Q2")
### Age group intervals
Q11_b_int_Age_group <- readRDS(file = "Q11_b_int_Age_group")
## Freq
### Aggregate intervals
Q11_f_int <- readRDS(file = "Q11_f_int")
### Gender intervals
Q11_f_int_Q2 <- readRDS(file = "Q11_f_int_Q2")
### Age group intervals
Q11_f_int_Age_group <- readRDS(file = "Q11_f_int_Age_group")
## Coef comparison
Q11_coefs <- readRDS(file = "Q11_coefs")

## How was your night data
## Bayes
## Aggregate intervals
Q9_b_int <- readRDS(file = "Q9_b_int")
### Gender intervals
Q9_b_int_Q2 <- readRDS(file = "Q9_b_int_Q2")
### Age group intervals
Q9_b_int_Age_group <- readRDS(file = "Q9_b_int_Age_group")
## Freq
### Aggregate intervals
Q9_f_int <- readRDS(file = "Q9_f_int")
### Gender intervals
Q9_f_int_Q2 <- readRDS(file = "Q9_f_int_Q2")
### Age group intervals
Q9_f_int_Age_group <- readRDS(file = "Q9_f_int_Age_group")
## Coef comparison
Q9_coefs <- readRDS(file = "Q9_coefs")

# How would you rate the live music scene
## Aggregate comparison plots
p_Q11_general <- bind_rows(Q11_b_int, Q11_f_int) %>%
  ggplot(aes(x = category, y = est, color = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0,
                position = position_dodge(width = 0.4), size = 0.5)  +
  geom_errorbar(aes(ymin = lwr50, ymax = upr50), width = 0,
                position = position_dodge(width = 0.4), size = 1.5) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.1, 0.75),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.y = unit(0.1, "cm")) +
  labs(x = "Answer category", y = "Probability",
       subtitle ="Question: Opinion on the Live Music Scene",
       color = "Model", alpha = "Data type", fill = "Data type") +
  scale_y_continuous(label = scales::percent) +
  scale_color_grey(start = 0.1, end = 0.6) +
  geom_point(data = census_data_all %>%
               count(City, Q11) %>%
               na.omit() %>%
               group_by(City) %>%
               mutate(percent = n/sum(n)) %>%
               mutate(type = case_when(
                 City == "Helsinki" ~ "Test",
                 TRUE ~ "Train")),
             aes(x = Q11, y = percent, fill = type, alpha = type),
             inherit.aes = FALSE, shape = 21) +
  scale_fill_manual(values = c("Test" = "blue", "Train" = "red")) +
  scale_alpha_manual(values = c("Test" = 1.0, "Train" = 0.3)) +
  guides(fill = guide_legend(order = 1), alpha = "none")

ggsave(filename = "Q11_general.png", plot = p_Q11_general,
       width = 2550, height = 1350, units = "px")

## Gender comparison plots
p_Q11_Q2 <- bind_rows(Q11_b_int_Q2, Q11_f_int_Q2) %>%
  ggplot(aes(x = category, y = est, color = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0,
                position = position_dodge(width = 0.4), size = 0.5)  +
  geom_errorbar(aes(ymin = lwr50, ymax = upr50), width = 0,
                position = position_dodge(width = 0.4), size = 1.5) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.072, 0.73),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) +
  labs(x = "Answer category", y = "Probability",
       subtitle ="Question: Opinion on the Live Music Scene",
       color = "Model", alpha = "Data type", fill = "Data type") +
  scale_y_continuous(label = scales::percent) +
  scale_color_grey(start = 0.1, end = 0.6) + facet_wrap(~Q2) +
  geom_point(data = census_data_all %>%
               count(City, Q11, Q2) %>%
               na.omit() %>%
               group_by(City, Q2) %>%
               mutate(percent = n/sum(n)) %>%
               mutate(type = case_when(
                 City == "Helsinki" ~ "Test",
                 TRUE ~ "Train")),
             aes(x = Q11, y = percent, fill = type, alpha = type),
             inherit.aes = FALSE, shape = 21) +
  scale_fill_manual(values = c("Test" = "blue", "Train" = "red")) +
  scale_alpha_manual(values = c("Test" = 1.0, "Train" = 0.3)) +
  guides(fill = guide_legend(order = 1), alpha = "none")

ggsave(filename = "Q11_Q2.png", plot = p_Q11_Q2,
       width = 2550, height = 1350, units = "px")

## Age group comparison plots
p_Q11_Age_group <- bind_rows(Q11_b_int_Age_group, Q11_f_int_Age) %>%
  ggplot(aes(x = category, y = est, color = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0,
                position = position_dodge(width = 0.4), size = 0.5)  +
  geom_errorbar(aes(ymin = lwr50, ymax = upr50), width = 0,
                position = position_dodge(width = 0.4), size = 1.5) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.072, 0.84),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.y = unit(0.1, "cm")) +
  labs(x = "Answer category", y = "Probability",
       subtitle ="Question: Opinion on the Live Music Scene",
       color = "Model", alpha = "Data type", fill = "Data type") +
  scale_y_continuous(label = scales::percent) +
  scale_color_grey(start = 0.1, end = 0.6) + facet_wrap(~Age_group) +
  geom_point(data = census_data_all %>%
               count(City, Q11, Age_group) %>%
               na.omit() %>%
               group_by(City, Age_group) %>%
               mutate(percent = n/sum(n)) %>%
               mutate(type = case_when(
                 City == "Helsinki" ~ "Test",
                 TRUE ~ "Train")),
             aes(x = Q11, y = percent, fill = type, alpha = type),
             inherit.aes = FALSE, shape = 21) +
  scale_fill_manual(values = c("Test" = "blue", "Train" = "red")) +
  scale_alpha_manual(values = c("Test" = 1.0, "Train" = 0.3)) +
  guides(fill = guide_legend(order = 1), alpha = "none")

ggsave(filename = "Q11_Age_group.png", plot = p_Q11_Age_group,
       width = 2550, height = 1800, units = "px")

# How was your night
## Aggregate comparison plots
p_Q9_general <- bind_rows(Q9_b_int, Q9_f_int) %>%
  ggplot(aes(x = category, y = est, color = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0,
                position = position_dodge(width = 0.4), size = 0.5)  +
  geom_errorbar(aes(ymin = lwr50, ymax = upr50), width = 0,
                position = position_dodge(width = 0.4), size = 1.5) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.1, 0.75),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.y = unit(0.1, "cm")) +
  labs(x = "Answer category", y = "Probability",
       subtitle ="Question: Opinion on the Night",
       color = "Model", alpha = "Data type", fill = "Data type") +
  scale_y_continuous(label = scales::percent) +
  scale_color_grey(start = 0.1, end = 0.6) +
  geom_point(data = census_data_all %>%
               count(City, Q9) %>%
               na.omit() %>%
               group_by(City) %>%
               mutate(percent = n/sum(n)) %>%
               mutate(type = case_when(
                 City == "Helsinki" ~ "Test",
                 TRUE ~ "Train")),
             aes(x = Q9, y = percent, fill = type, alpha = type),
             inherit.aes = FALSE, shape = 21) +
  scale_fill_manual(values = c("Test" = "blue", "Train" = "red")) +
  scale_alpha_manual(values = c("Test" = 1.0, "Train" = 0.3)) +
  guides(fill = guide_legend(order = 1), alpha = "none")

ggsave(filename = "Q9_general.png", plot = p_Q9_general,
       width = 2550, height = 1350, units = "px")

## Gender comparison plots
p_Q9_Q2 <- bind_rows(Q9_b_int_Q2, Q9_f_int_Q2) %>%
  ggplot(aes(x = category, y = est, color = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0,
                position = position_dodge(width = 0.4), size = 0.5)  +
  geom_errorbar(aes(ymin = lwr50, ymax = upr50), width = 0,
                position = position_dodge(width = 0.4), size = 1.5) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.072, 0.73),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) +
  labs(x = "Answer category", y = "Probability",
       subtitle ="Question: Opinion on the Night",
       color = "Model", alpha = "Data type", fill = "Data type") +
  scale_y_continuous(label = scales::percent) +
  scale_color_grey(start = 0.1, end = 0.6) + facet_wrap(~Q2) +
  geom_point(data = census_data_all %>%
               count(City, Q9, Q2) %>%
               na.omit() %>%
               group_by(City, Q2) %>%
               mutate(percent = n/sum(n)) %>%
               mutate(type = case_when(
                 City == "Helsinki" ~ "Test",
                 TRUE ~ "Train")),
             aes(x = Q9, y = percent, fill = type, alpha = type),
             inherit.aes = FALSE, shape = 21) +
  scale_fill_manual(values = c("Test" = "blue", "Train" = "red")) +
  scale_alpha_manual(values = c("Test" = 1.0, "Train" = 0.3)) +
  guides(fill = guide_legend(order = 1), alpha = "none")

ggsave(filename = "Q9_Q2.png", plot = p_Q9_Q2,
       width = 2550, height = 1350, units = "px")

## Age group comparison plots
p_Q9_Age_group <- bind_rows(Q9_b_int_Age_group, Q9_f_int_Age) %>%
  ggplot(aes(x = category, y = est, color = model)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0,
                position = position_dodge(width = 0.4), size = 0.5)  +
  geom_errorbar(aes(ymin = lwr50, ymax = upr50), width = 0,
                position = position_dodge(width = 0.4), size = 1.5) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.072, 0.84),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.y = unit(0.1, "cm")) +
  labs(x = "Answer category", y = "Probability",
       subtitle ="Question: Opinion on the Night",
       color = "Model", alpha = "Data type", fill = "Data type") +
  scale_y_continuous(label = scales::percent) +
  scale_color_grey(start = 0.1, end = 0.6) + facet_wrap(~Age_group) +
  geom_point(data = census_data_all %>%
               count(City, Q9, Age_group) %>%
               na.omit() %>%
               group_by(City, Age_group) %>%
               mutate(percent = n/sum(n)) %>%
               mutate(type = case_when(
                 City == "Helsinki" ~ "Test",
                 TRUE ~ "Train")),
             aes(x = Q9, y = percent, fill = type, alpha = type),
             #position = position_jitter(width = 0.2, height = 0.02),
             inherit.aes = FALSE, shape = 21) +
  scale_fill_manual(values = c("Test" = "blue", "Train" = "red")) +
  scale_alpha_manual(values = c("Test" = 1.0, "Train" = 0.3)) +
  guides(fill = guide_legend(order = 1), alpha = "none")

ggsave(filename = "Q9_Age_group.png", plot = p_Q9_Age_group,
       width = 2550, height = 1800, units = "px")

# Coef comparisons plot
coef_comp <- bind_rows(Q11_coefs, Q9_coefs) %>%
  filter(!term %in% c("1|2", "2|3", "3|4", "4|5")) %>%
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
  theme(legend.position = c(0.1, 0.85),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm")) +
  labs(x = "Estimate", y = "Coefficent",
       color = "Model", alpha = "Data type", fill = "Data type") +
  scale_color_grey(start = 0.1, end = 0.6) + facet_wrap(~Question) +
  geom_vline(xintercept = 0, linetype = 2, color = "red")

ggsave(filename = "coef_comp.png", plot = coef_comp,
       width = 2550, height = 1350, units = "px")
