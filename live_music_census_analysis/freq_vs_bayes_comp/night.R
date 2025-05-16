############ Code for analysing the how was your night question##############

# Libraries
library(brms)
library(ordinal)
library(tidyr)
library(dplyr)
library(emmeans)
library(tidybayes)
library(broom)
library(broom.mixed)

# Loading the data and setting up different datasets
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
# Learning data
learn_data <- census_data_all %>%
  filter(!City %in% c("Helsinki"))
learn_data2 <- learn_data %>%
  mutate(Q9 = factor(Q9))
learn_data <- learn_data %>%
  mutate(Q9 = as.numeric(Q9))
test_data <- census_data_all %>%
  filter(City %in% c("Helsinki"))

# Starting off by fitting all the models
## Specifing variables
exp_vars <- c("1", "Q2", "Q2 + Age_group", "Q2 + Age_group + (1 | City)")
## Setting up priors
priors <- list(
  "1" = c(prior(normal(-1.4,1), class = "Intercept", coef = "1") +
            prior(normal(-0.4,1), class = "Intercept", coef = "2") +
            prior(normal(0.4,1), class = "Intercept", coef = "3") +
            prior(normal(1.4,1), class = "Intercept", coef = "4")),

  "Q2" = c(prior(normal(-1.4,1), class = "Intercept", coef = "1") +
             prior(normal(-0.4,1), class = "Intercept", coef = "2") +
             prior(normal(0.4,1), class = "Intercept", coef = "3") +
             prior(normal(1.4,1), class = "Intercept", coef = "4") +
             prior(normal(0,1), class = "b")),

  "Q2 + Age_group" =
    c(prior(normal(-1.4,1), class = "Intercept", coef = "1") +
        prior(normal(-0.4,1), class = "Intercept", coef = "2") +
        prior(normal(0.4,1), class = "Intercept", coef = "3") +
        prior(normal(1.4,1), class = "Intercept", coef = "4") +
        prior(normal(0,1), class = "b")),

  "Q2 + Age_group + (1 | City)"
  = c(prior(normal(-1.4,1), class = "Intercept", coef = "1") +
        prior(normal(-0.4,1), class = "Intercept", coef = "2") +
        prior(normal(0.4,1), class = "Intercept", coef = "3") +
        prior(normal(1.4,1), class = "Intercept", coef = "4") +
        prior(normal(0,1), class = "b") +
        prior(normal(0,1), class = "sd"))
)

# Fitting the different models for question "How was your night?"
Q9_models <- lapply(exp_vars, function(explanatory) {
  formula <- as.formula(paste("Q9", "~", explanatory))
  brm(formula, family = cumulative("logit"), data = learn_data, seed = 2025,
      cores = 4, control = list(adapt_delta = 0.99),
      prior = priors[[explanatory]], silent = 2, refresh = 0)
})

# Loo comparisons
loo(Q9_models[[1]], Q9_models[[2]], Q9_models[[3]], Q9_models[[4]])

# Fittign the frequentist model
Q9_clm_f <- clm(Q9 ~ Q2 + Age_group, data = learn_data2, link = "logit")

# Calculating intervals
## Freq
### Aggregate
Q9_f_int95 <- emmeans::emmeans(Q9_clm_f, ~ Q9, mode = "prob") %>%
  as.tibble() %>%
  select(1, 2, 5, 6) %>%
  rename_with(~c("category", "est", "lwr95", "upr95")) %>%
  mutate(model = "Frequentist")
Q9_f_int50<- emmeans::emmeans(Q9_clm_f, ~ Q9, mode = "prob", level = 0.5) %>%
  as.tibble() %>%
  select(1, 5, 6) %>%
  rename_with(~c("category", "lwr50", "upr50"))
Q9_f_int <- Q9_f_int95 %>%
  left_join(Q9_f_int50, by = "category") %>%
  mutate(category = factor(category, levels = c("1", "2", "3", "4", "5")))
### Gender
Q9_clm_f <- clm(Q9 ~ Q2 + Age_group, data = learn_data2, link = "logit")
Q9_clm_f <- clmm(Q9 ~ Q2 + Age_group + (1 | City),
                  data = learn_data2, link = "logit")

Q9_f_int95 <- emmeans::emmeans(Q9_clm_f, ~ Q9, mode = "prob") %>%
  as.tibble() %>%
  select(1, 2, 5, 6) %>%
  rename_with(~c("category", "est", "lwr95", "upr95")) %>%
  mutate(model = "Frequentist")
Q9_f_int50<- emmeans::emmeans(Q9_clm_f, ~ Q9, mode = "prob", level = 0.5) %>%
  as.tibble() %>%
  select(1, 5, 6) %>%
  rename_with(~c("category", "lwr50", "upr50"))
Q9_f_int <- Q9_f_int95 %>%
  left_join(Q9_f_int50, by = "category") %>%
  mutate(category = factor(category, levels = c("1", "2", "3", "4", "5")))
### Age group
Q9_f_int95_Age <- emmeans::emmeans(Q9_clm_f, ~ Q9 | Age_group, mode = "prob") %>%
  as.tibble() %>%
  select(1, 2, 3, 6, 7) %>%
  rename_with(~c("category", "Age_group", "est", "lwr95", "upr95")) %>%
  mutate(model = "Frequentist")
Q9_f_int50_Age <- emmeans::emmeans(Q9_clm_f, ~ Q9 | Age_group, mode = "prob",
                                    level = 0.5) %>%
  as.tibble() %>%
  select(1, 2, 6, 7) %>%
  rename_with(~c("category", "Age_group", "lwr50", "upr50"))
Q9_f_int_Age <- Q9_f_int95_Age %>%
  left_join(Q9_f_int50_Age, by = c("category", "Age_group")) %>%
  mutate(category = factor(category, levels = c("1", "2", "3", "4", "5")))

## Bayes
### Aggregate
Q9_b_int <- expand.grid(Q2 = c("Female", "Male", "Another gender identity"),
                         Age_group = c("30-49", "18-29", "50-64", ">64")) %>%
  add_epred_draws(object = Q9_models[[3]]) %>%
  group_by(.category) %>%
  summarise(est = mean(.epred), lwr95 = quantile(.epred, 0.025),
            upr95 = quantile(.epred, 0.975),
            lwr50 = quantile(.epred, 0.25), upr50 = quantile(.epred, 0.75)) %>%
  rename(category = .category) %>%
  mutate(model = "Bayes") %>%
  mutate(category = factor(category, levels = c("1", "2", "3", "4", "5")))
### Gender
Q9_b_int_Q2 <- expand.grid(Q2 = c("Female", "Male", "Another gender identity"),
                            Age_group = c("30-49", "18-29", "50-64", ">64")) %>%
  add_epred_draws(object = Q9_models[[3]]) %>%
  group_by(.category, Q2) %>%
  summarise(est = mean(.epred), lwr95 = quantile(.epred, 0.025),
            upr95 = quantile(.epred, 0.975),
            lwr50 = quantile(.epred, 0.25), upr50 = quantile(.epred, 0.75)) %>%
  rename(category = .category) %>%
  mutate(model = "Bayes") %>%
  mutate(category = factor(category, levels = c("1", "2", "3", "4", "5")))
### Age group
Q9_b_int_Age_group <- expand.grid(
  Q2 = c("Female", "Male", "Another gender identity"),
  Age_group = c("30-49", "18-29", "50-64", ">64")) %>%
  add_epred_draws(object = Q9_models[[3]]) %>%
  group_by(.category, Age_group) %>%
  summarise(est = mean(.epred), lwr95 = quantile(.epred, 0.025),
            upr95 = quantile(.epred, 0.975),
            lwr50 = quantile(.epred, 0.25), upr50 = quantile(.epred, 0.75)) %>%
  rename(category = .category) %>%
  mutate(model = "Bayes") %>%
  mutate(category = factor(category, levels = c("1", "2", "3", "4", "5")))

# Calculating MSE
mse_Q9 <- census_data_all %>%
  count(City, Q9, Q2) %>%
  na.omit() %>%
  filter(City %in% c("Helsinki")) %>%
  group_by(City, Q2) %>%
  mutate(percent = n/sum(n),
         Q9 = as.factor(Q9),
         model = "real") %>%
  ungroup() %>%
  select(-c(City, n)) %>%
  rename(category = Q9) %>%
  bind_rows(Q9_b_int_Q2 %>%
              select(-c(lwr95, upr95, lwr50, upr50)) %>%
              rename(percent = est)) %>%
  bind_rows(Q9_f_int_Q2 %>%
              select(-c(lwr95, upr95, lwr50, upr50)) %>%
              rename(percent = est)) %>%
  pivot_wider(names_from = model, values_from = percent) %>%
  mutate(real = replace_na(real, 0),
         Frequentist = replace_na(Frequentist, 0)) %>%
  mutate(sq_error_b = (real - Bayes)^2,
         sq_error_f = (real - Frequentist)^2)

mean(mse_Q9$sq_error_b)
mean(mse_Q9$sq_error_f)

# Coefficent estimates
Q9_coef_estimates_f95 <- tidy(Q9_clm_f, conf.int = TRUE) %>%
  select(term, estimate, std.error, conf.low, conf.high) %>%
  mutate(model = "Frequentist") %>%
  rename(conf.low95 = conf.low, conf.high95 = conf.high)

Q9_coef_estimates_b95 <- tidy(Q9_models[[3]], conf.int = TRUE) %>%
  select(term, estimate, std.error, conf.low, conf.high) %>%
  mutate(model = "Bayes",
         term = case_match(term,
                           "(Intercept)[1]" ~ "1|2",
                           "(Intercept)[2]" ~ "2|3",
                           "(Intercept)[3]" ~ "3|4",
                           "(Intercept)[4]" ~ "4|5",
                           "Q2Male" ~ "Q2Male",
                           "Q2Anothergenderidentity" ~ "Q2Another gender identity",
                           "Age_group18M29" ~ "Age_group18-29",
                           "Age_group30M49" ~ "Age_group30-49",
                           "Age_group50M64" ~ "Age_group50-64")
  ) %>%
  rename(conf.low95 = conf.low, conf.high95 = conf.high)

Q9_coef_estimates_f50 <- tidy(Q9_clm_f, conf.int = TRUE, conf.level = 0.5) %>%
  select(term, conf.low, conf.high) %>%
  rename(conf.low50 = conf.low, conf.high50 = conf.high)

Q9_coef_estimates_b50 <- tidy(Q9_models[[3]], conf.int = TRUE, conf.level = 0.5) %>%
  select(term, conf.low, conf.high) %>%
  mutate(term = case_match(term,
                           "(Intercept)[1]" ~ "1|2",
                           "(Intercept)[2]" ~ "2|3",
                           "(Intercept)[3]" ~ "3|4",
                           "(Intercept)[4]" ~ "4|5",
                           "Q2Male" ~ "Q2Male",
                           "Q2Anothergenderidentity" ~ "Q2Another gender identity",
                           "Age_group18M29" ~ "Age_group18-29",
                           "Age_group30M49" ~ "Age_group30-49",
                           "Age_group50M64" ~ "Age_group50-64")
  ) %>%
  rename(conf.low50 = conf.low, conf.high50 = conf.high)
Q9_coef_estimates_b <- Q9_coef_estimates_b95 %>%
  left_join(Q9_coef_estimates_b50, by = "term")
Q9_coef_estimates_f <- Q9_coef_estimates_f95 %>%
  left_join(Q9_coef_estimates_f50, by = "term")

# Compining the datasets
Q9_coefs <- bind_rows(Q9_coef_estimates_b, Q9_coef_estimates_f) %>%
  mutate(Question = "Opinion on the scene")

# Testing proportional odds assumption
Q9_category_specific_Q2 <- brm(Q9 ~ cs(Q2) + Age_group,
                               family = acat("logit"),
                               data = learn_data, seed = 2025,
                               cores = 4, control = list(adapt_delta = 0.99),
                               prior = prior(normal(-1.4,1), class = "Intercept", coef = "1") +
                                 prior(normal(-0.4,1), class = "Intercept", coef = "2") +
                                 prior(normal(0.4,1), class = "Intercept", coef = "3") +
                                 prior(normal(1.4,1), class = "Intercept", coef = "4") +
                                 prior(normal(0,1), class = "b"), silent = 2, refresh = 0)

Q9_category_specific_Age <- brm(Q9 ~ Q2 + cs(Age_group),
                                family = acat("logit"),
                                data = learn_data, seed = 2025,
                                cores = 4, control = list(adapt_delta = 0.99),
                                prior = prior(normal(-1.4,1), class = "Intercept", coef = "1") +
                                  prior(normal(-0.4,1), class = "Intercept", coef = "2") +
                                  prior(normal(0.4,1), class = "Intercept", coef = "3") +
                                  prior(normal(1.4,1), class = "Intercept", coef = "4") +
                                  prior(normal(0,1), class = "b"), silent = 2, refresh = 0)

Q9_acat <- brm(Q9 ~ Q2 + Age_group,
               family = acat("logit"),
               data = learn_data, seed = 2025,
               cores = 4, control = list(adapt_delta = 0.99),
               prior = prior(normal(-1.4,1), class = "Intercept", coef = "1") +
                 prior(normal(-0.4,1), class = "Intercept", coef = "2") +
                 prior(normal(0.4,1), class = "Intercept", coef = "3") +
                 prior(normal(1.4,1), class = "Intercept", coef = "4") +
                 prior(normal(0,1), class = "b"), silent = 2, refresh = 0)

loo(Q9_models[[3]], Q9_acat, Q9_category_specific_Age, Q9_category_specific_Q2)

# Saving the values
## Coef comparisons
saveRDS(Q9_coefs, file = "Q9_coefs")
## Bayes
### Aggregate intervals
saveRDS(Q9_b_int, file = "Q9_b_int")
### Gender intervals
saveRDS(Q9_b_int_Q2, file = "Q9_b_int_Q2")
### Age group intervals
saveRDS(Q9_b_int_Age_group, file = "Q9_b_int_Age_group")
## Freq
### Aggregate intervals
saveRDS(Q9_f_int, file = "Q9_f_int")
### Gender intervals
saveRDS(Q9_f_int_Q2, file = "Q9_f_int_Q2")
### Age group intervals
saveRDS(Q9_f_int_Age_group, file = "Q9_f_int_Age_group")
