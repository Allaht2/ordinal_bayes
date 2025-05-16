####### Analysis of the whole data for "How was your night?" question #######

# Libraries
library(brms)
library(tidyr)
library(dplyr)
library(tidybayes)
library(broom)
library(broom.mixed)

# Loading the data and setting up datasets
census_data_all <- read_sav("../data/combined_census_data.sav") %>%
  filter(exclude == 0) %>%
  mutate(Q2 = haven::as_factor(Q2),
         Age_group = case_when(
           Age < 18 ~ "<18",
           Age > 17 & Age < 30 ~ "18-29",
           Age > 29 & Age < 50 ~ "30-49",
           Age > 49 & Age < 65 ~ "50-64",
           Age > 64 ~ ">64"
         ))
whole_Q9 <- census_data_all %>%
  filter(!is.na(Q9)) %>%
  mutate(Q9 = as.numeric(Q9))


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

# Fitting the different models for question "How is the live music scene in you city?"
Q9_models_whole <- lapply(exp_vars, function(explanatory) {
  formula <- as.formula(paste("Q9", "~", explanatory))
  brm(formula, family = cumulative("logit"), data = whole_Q9, seed = 2025,
      cores = 4, control = list(adapt_delta = 0.99),
      prior = priors[[explanatory]], silent = 2, refresh = 0)
})

# Model comparison
loo(Q9_models_whole[[1]], Q9_models_whole[[2]],
    Q9_models_whole[[3]], Q9_models_whole[[4]])

# Calculating parameter effects and intervals
Q9_whole_draws <- expand.grid(Q2 = c("Female", "Male", "Another gender identity"),
                               Age_group = c("30-49", "18-29", "50-64", ">64"),
                               City = c("Helsinki", "Lviv", "Mannheim", "Vilnius",
                                        "Heidelberg")) %>%
  add_epred_draws(object = Q9_models_whole[[4]])
## Mean categories
### Gender
Q9_gender_mean_cate <- Q9_whole_draws %>%
  ungroup() %>%
  mutate(mean_cat = as.numeric(.category) * .epred) %>%
  group_by(.draw, .row, Q2) %>%
  summarise(mean_cat = sum(mean_cat)) %>%
  mutate(Q2 = case_match(Q2,
                         "Another gender identity" ~ "Another gender\nidentity",
                         "Male" ~ "Male",
                         "Female" ~ "Female"))
### Age group
Q9_age_group_mean_cate <- Q9_whole_draws %>%
  ungroup() %>%
  mutate(mean_cat = as.numeric(.category) * .epred) %>%
  group_by(.draw, .row, Age_group) %>%
  summarise(mean_cat = sum(mean_cat)) %>%
  mutate(Age_group = factor(Age_group,
                            levels = c("18-29", "30-49", "50-64", ">64"))) %>%
  arrange(Age_group)

## Coefficent intervals
Q9_coef_estimates_b95_whole <- tidy(Q9_models_whole[[4]], conf.int = TRUE) %>%
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
                           "Age_group50M64" ~ "Age_group50-64",
                           "sd__(Intercept)" ~ "sd")
  ) %>%
  rename(conf.low95 = conf.low, conf.high95 = conf.high)

Q9_coef_estimates_b50_whole <- tidy(Q9_models_whole[[4]],
                                     conf.int = TRUE, conf.level = 0.5) %>%
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
                           "Age_group50M64" ~ "Age_group50-64",
                           "sd__(Intercept)" ~ "sd")
  ) %>%
  rename(conf.low50 = conf.low, conf.high50 = conf.high)

Q9_coefs_whole <- Q9_coef_estimates_b95_whole %>%
  left_join(Q9_coef_estimates_b50_whole, by = "term") %>%
  mutate(Question = "Opinion on the scene")

# Calculating city estimates
Q9_city_estimates <- Q9_whole_draws %>%
  group_by(.category, City) %>%
  summarise(est = mean(.epred), lwr95 = quantile(.epred, 0.025),
            upr95 = quantile(.epred, 0.975), lwr50 = quantile(.epred, 0.25),
            upr50 = quantile(.epred, 0.75)) %>%
  mutate(Question = "Opinion on the scene")

# Saving the values
## Mean category values
### Gender
saveRDS(Q9_gender_mean_cate, file = "Q9_gender_mean_cate")
### Age group
saveRDS(Q9_age_group_mean_cate, file = "Q9_age_group_mean_cate")
# Coef estimates
saveRDS(Q9_coefs_whole, file = "Q9_coefs_whole")
# City estimates
saveRDS(Q9_city_estimates, file = "Q9_city_estimates")
