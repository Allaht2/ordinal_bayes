######## Code for the simulation study ############

# Libraries
library(brms)
library(ordinal)
library(tidyr)
library(dplyr)
library(emmeans)
library(arm)
library(tidybayes)


# Optimal situation
## Parameter values
N <- 600 # Sample size
or <- 3 # Odds ratio
k <- 5 # number of ordinal categories
probs0 <- rep(1/k, k) # Probabilities
alpha <- prob_to_alpha(probs0, lin = "logit") # Transforming the probabilities into threshold values

## Calculating the probabilities when x = 1
y <- data.frame(x= rep(c(0, 1), each = N/2))
lp <- lapply(alpha, function(x) x - log(or) * y$x)
names(lp) <- sprintf("lp_leq%s", 1:(k-1))
lp <- data.frame(lp)
cump <- lapply(lp, arm::invlogit)
cump <- data.frame(cump)
names(cump) <- sprintf("cump_leq%s", 1:(k-1))
cump <- cbind(0, cump, 1)
p <- apply(cump, 1, diff, simplify = FALSE)
p <- data.frame(do.call(rbind, p))
names(p) <- sprintf("p%s", 1:k)
p1 <- p[c(1, 301), ]
p1 <- p1 %>%
  rename("1" = p1, "2" = p2, "3" = p3, "4" = p4, "5" = p5) %>%
  pivot_longer(everything(), names_to = "category", values_to = "est") %>%
  mutate(x = factor(rep(0:1, each = 5)))

##  Tibble for the model times
optimal_model_times <- tibble(freq = as.numeric(), bayes = as.numeric())
## Tibbles for the results
optimal_model_estimates_prob <- tibble()
optimal_model_estimates_para <- tibble()

## Simulations
for (i in 1:50) {
  ### Simulating data
  y <- data.frame(x= rep(c(0, 1), each = N/2))

  ### Fitting the frequentist model
  y <- sim_ord_latent(location = ~x, beta = log(or), prob0 = probs0, data = y,
                      link = "logit")
  y$x <- as.factor(y$x)
  start_time <- Sys.time()
  fit_f_1 <- clm(y ~ x, data = y, lin = "logit")
  end_time <- Sys.time()
  optimal_model_times[i,1] <- as.numeric(end_time - start_time)

  ### Getting the frequentist estimates
  freq_est <- cbind(confint(fit_f_1), Estimate = fit_f_1$beta) %>%
    as.data.frame() %>%
    rename("Q2.5" = "2.5 %", "Q97.5" = "97.5 %") %>%
    mutate(model = "Frequentist")
  freq_int <- emmeans::emmeans(fit_f_1, ~y|x, mode = "prob") %>%
    as.tibble() %>%
    select(1, 2, 3, 6, 7) %>%
    rename_with(~c("category", "x", "est", "lwr95", "upr95")) %>%
    mutate(model = "Frequentist")


  ### Fitting the Bayesian model
  y$y <- as.numeric(y$y)
  ### Compiling the model first time
  start_time <- Sys.time()
  fit_b_1 <- brm(formula = y ~ x, data = y, family = cumulative("logit"),
                 prior = prior(normal(0,1), class = b, coef = "x1") +
                   prior(normal(-1.4,1), class = "Intercept", coef = "1") +
                   prior(normal(-0.4,1), class = "Intercept", coef = "2") +
                   prior(normal(0.4,1), class = "Intercept", coef = "3") +
                   prior(normal(1.4,1), class = "Intercept", coef = "4"),
                 silent = 2, refresh = 0, file = "opt_model")
    end_time <- Sys.time()
    optimal_model_times[i,2] <- as.numeric(end_time - start_time)

  bayes_est <- fixef(fit_b_1)[5,-2] %>%
    t() %>%
    as.data.frame() %>%
    mutate(model = "Bayes")
  bayes_int <- add_epred_draws(object = fit_b_1,
                               newdata = expand_grid(x = c(0,1))) %>%
    group_by(.category, x) %>%
    summarise(est = mean(.epred), lwr95 = quantile(.epred, 0.025),
              upr95 = quantile(.epred, 0.975), lwr50 = quantile(.epred, 0.25),
              upr50 = quantile(.epred, 0.75)) %>%
    mutate(model = "Bayes", x = as.factor(x)) %>%
    rename(category = .category)

  ### Saving the values
  optimal_model_estimates_prob <- bind_rows(bayes_int, freq_int) %>%
    mutate(sim_id = i) %>%
    bind_rows(optimal_model_estimates_int)

  optimal_model_estimates_para <- bind_rows(bayes_est, freq_est) %>%
    mutate(sim_id = i) %>%
    bind_rows(optimal_model_estimates_est)
}

## Calculating parameter coverage in the two models
optimal_model_estimates_para %>%
  mutate(covered = (log(or) >= Q2.5 & log(or) <= Q97.5)) %>%
  group_by(model) %>%
  summarise(coverage_rate = mean(covered))

## Calculating the MSE for both models
optimal_model_estimates_prob %>%
  left_join(p1, by = c("category", "x"), suffix = c("_sim", "_true")) %>%
  mutate(sq_error = (est_sim - est_true)^2) %>%
  group_by(model) %>%
  summarise(mse = mean(sq_error))

## Model fitting times
optimal_model_times$bayes[1] # Model compiling time
mean(optimal_model_times$bayes[-1]) # Bayes times
mean(optimal_model_times$freq) # Freq times

# One category has no responses
## Parameter values
N <- 50 # Sample size
or <- 3 # Odds ratio
k <- 5 # number of ordinal categories
probs0 <- c(0.05, 0.225, 0.25, 0.25, 0.225)  # Probabilities
alpha <- prob_to_alpha(probs0, lin = "logit")

## Calculating probabilities when x = 1
y <- data.frame(x= rep(c(0, 1), each = N/2))
lp <- lapply(alpha, function(x) x - log(or) * y$x)
names(lp) <- sprintf("lp_leq%s", 1:(k-1))
lp <- data.frame(lp)
cump <- lapply(lp, arm::invlogit)
cump <- data.frame(cump)
names(cump) <- sprintf("cump_leq%s", 1:(k-1))
cump <- cbind(0, cump, 1)
p <- apply(cump, 1, diff, simplify = FALSE)
p <- data.frame(do.call(rbind, p))
names(p) <- sprintf("p%s", 1:k)
p2 <- p[c(1, 26), ]
p2 <- p2 %>%
  rename("1" = p1, "2" = p2, "3" = p3, "4" = p4, "5" = p5) %>%
  pivot_longer(everything(), names_to = "category", values_to = "est") %>%
  mutate(x = factor(rep(0:1, each = 5)))

## Tible for the model times
mis1_model_times <- tibble(freq = as.numeric(), bayes = as.numeric())
## Tibbles for results
mis1_model_estimates_prob <- tibble()
mis1_model_estimates_para <- tibble()

## Simulations
for (i in 1:50) {
  # Simulating data
  y <- data.frame(x= rep(c(0, 1), each = N/2))
  y <- sim_ord_latent(location = ~x, beta = log(or), alpha = alpha, data = y,
                      link = "logit")
  y$x <- as.factor(y$x)
  # Fit frequentist model
  start_time <- Sys.time()
  fit_f_2 <- clm(y ~ x, data = y, lin = "logit")
  end_time <- Sys.time()
  mis1_model_times[i,1] <- as.numeric(end_time - start_time)

  # Getting the frequentist estimates
  freq_est <- cbind(confint(fit_f_2), Estimate = fit_f_2$beta) %>%
    as.data.frame() %>%
    rename("Q2.5" = "2.5 %", "Q97.5" = "97.5 %") %>%
    mutate(model = "Frequentist")
  freq_int <- emmeans::emmeans(fit_f_2, ~y|x, mode = "prob") %>%
    as.tibble() %>%
    select(1, 2, 3, 6, 7) %>%
    rename_with(~c("category", "x", "est", "lwr95", "upr95")) %>%
    mutate(model = "Frequentist")
  if (length(unique(freq_int$category)) < 5) {
    freq_int <- freq_int %>%
      mutate(category = factor(as.numeric(category) + 1))
  }

  # Fitting the Bayesian model
  y$y <- as.numeric(as.character(y$y))
  start_time <- Sys.time()
  fit_b_2 <- brm(formula = y ~ x, data = y, family = cumulative("logit"),
                 prior = prior(normal(0,1), class = b, coef = "x1") +
                   prior(normal(-1.4,1), class = "Intercept", coef = "1") +
                   prior(normal(-0.4,1), class = "Intercept", coef = "2") +
                   prior(normal(0.4,1), class = "Intercept", coef = "3") +
                   prior(normal(1.4,1), class = "Intercept", coef = "4"),
                 silent = 2, refresh = 0, file = "mis1_model")
  end_time <- Sys.time()
  mis1_model_times[i,2] <- as.numeric(end_time - start_time)

  bayes_est <- fixef(fit_b_2)[5,-2] %>%
    t() %>%
    as.data.frame() %>%
    mutate(model = "Bayes")
  bayes_int <- add_epred_draws(object = fit_b_2,
                               newdata = expand_grid(x = c(0,1))) %>%
    group_by(.category, x) %>%
    summarise(est = mean(.epred), lwr95 = quantile(.epred, 0.025),
              upr95 = quantile(.epred, 0.975), lwr50 = quantile(.epred, 0.25),
              upr50 = quantile(.epred, 0.75)) %>%
    mutate(model = "Bayes", x = as.factor(x)) %>%
    rename(category = .category)

  # Saving the values
  mis1_model_estimates_prob <- bind_rows(bayes_int, freq_int) %>%
    mutate(sim_id = i) %>%
    bind_rows(mis1_model_estimates_prob)

  mis1_model_estimates_para <- bind_rows(bayes_est, freq_est) %>%
    mutate(sim_id = i) %>%
    bind_rows(mis1_model_estimates_para)
}

## Calculating parameter coverage
mis1_model_estimates_para %>%
  mutate(covered = (log(or) >= Q2.5 & log(or) <= Q97.5)) %>%
  group_by(model) %>%
  summarise(coverage_rate = mean(covered))

## Calculating the MSE for both models
mis1_model_estimates_prob %>%
  ungroup() %>%
  complete(category, x, model, sim_id,
           fill = list(est = 0, lwr95 = 0, upr95 = 0, lwr50 = 0, upr50 = 0)) %>%
  left_join(p2, by = c("category", "x"), suffix = c("_sim", "_true")) %>%
  mutate(sq_error = (est_sim - est_true)^2) %>%
  group_by(model) %>%
  summarise(mse = mean(sq_error))

## Model fitting times
mis1_model_times$bayes[1] # Model compiling time
mean(mis1_model_times$bayes[-1]) # Bayes times
mean(mis1_model_times$freq) # Freq times

# Two missing categories
## Parameter values
N <- 50 # Sample size
or <- 3 # Odds ratio
k <- 5 # number of ordinal categories
probs0 <- c(0.05, 0.05, 0.325, 0.325, 0.25)  # Probabilities
alpha <- prob_to_alpha(probs0, lin = "logit")

# Calculating probabilities when x = 1
y <- data.frame(x= rep(c(0, 1), each = N/2))
lp <- lapply(alpha, function(x) x - log(or) * y$x)
names(lp) <- sprintf("lp_leq%s", 1:(k-1))
lp <- data.frame(lp)
cump <- lapply(lp, arm::invlogit)
cump <- data.frame(cump)
names(cump) <- sprintf("cump_leq%s", 1:(k-1))
cump <- cbind(0, cump, 1)
p <- apply(cump, 1, diff, simplify = FALSE)
p <- data.frame(do.call(rbind, p))
names(p) <- sprintf("p%s", 1:k)
p3 <- p[c(1, 26), ]
p3 <- p3 %>%
  rename("1" = p1, "2" = p2, "3" = p3, "4" = p4, "5" = p5) %>%
  pivot_longer(everything(), names_to = "category", values_to = "est") %>%
  mutate(x = factor(rep(0:1, each = 5)))

## Tible for the model times
mis2_model_times <- tibble(freq = as.numeric(), bayes = as.numeric())
## Tibbles for results
mis2_model_estimates_prob <- tibble()
mis2_model_estimates_para <- tibble()

## Simulations
for (i in 1:50) {
  # Simulating data
  y <- data.frame(x= rep(c(0, 1), each = N/2))
  y <- sim_ord_latent(location = ~x, beta = log(or), alpha = alpha, data = y,
                      link = "logit")
  y$x <- as.factor(y$x)
  start_time <- Sys.time()
  fit_f_3 <- clm(y ~ x, data = y, lin = "logit")
  end_time <- Sys.time()
  mis1_model_times[i,1] <- as.numeric(end_time - start_time)

  # Getting the frequentist estimates
  freq_est <- cbind(confint(fit_f_3), Estimate = fit_f_3$beta) %>%
    as.data.frame() %>%
    rename("Q2.5" = "2.5 %", "Q97.5" = "97.5 %") %>%
    mutate(model = "Frequentist")
  freq_int <- emmeans::emmeans(fit_f_3, ~y|x, mode = "prob") %>%
    as.tibble() %>%
    select(1, 2, 3, 6, 7) %>%
    rename_with(~c("category", "x", "est", "lwr95", "upr95")) %>%
    mutate(model = "Frequentist")


  # Fitting the Bayesian model
  y$y <- as.numeric(as.character(y$y))
  start_time <- Sys.time()
  fit_b_3 <- brm(formula = y ~ x, data = y, family = cumulative("logit"),
                 prior = prior(normal(0,1), class = b, coef = "x1") +
                   prior(normal(-1.4,1), class = "Intercept", coef = "1") +
                   prior(normal(-0.4,1), class = "Intercept", coef = "2") +
                   prior(normal(0.4,1), class = "Intercept", coef = "3") +
                   prior(normal(1.4,1), class = "Intercept", coef = "4"),
                 silent = 2, refresh = 0, file = "mis2_model",
                 control = list(adapt_delta = 0.99))
  end_time <- Sys.time()
  mis1_model_times[i,2] <- as.numeric(end_time - start_time)

  bayes_est <- fixef(fit_b_3)[5,-2] %>%
    t() %>%
    as.data.frame() %>%
    mutate(model = "Bayes")
  bayes_int <- add_epred_draws(object = fit_b_3,
                               newdata = expand_grid(x = c(0,1))) %>%
    group_by(.category, x) %>%
    summarise(est = mean(.epred), lwr95 = quantile(.epred, 0.025),
              upr95 = quantile(.epred, 0.975), lwr50 = quantile(.epred, 0.25),
              upr50 = quantile(.epred, 0.75)) %>%
    mutate(model = "Bayes", x = as.factor(x)) %>%
    rename(category = .category)

  # Saving the values
  mis2_model_estimates_prob <- bind_rows(bayes_int, freq_int) %>%
    mutate(sim_id = i) %>%
    bind_rows(mis2_model_estimates_prob)

  mis2_model_estimates_para <- bind_rows(bayes_est, freq_est) %>%
    mutate(sim_id = i) %>%
    bind_rows(mis2_model_estimates_para)
}

## Calculating parameter coverage
mis2_model_estimates_para %>%
  mutate(covered = (log(or) >= Q2.5 & log(or) <= Q97.5)) %>%
  group_by(model) %>%
  summarise(coverage_rate = mean(covered))

## Calculating the MSE for both models
mse_mis2 <- mis2_model_estimates_prob %>%
  ungroup() %>%
  complete(category, x, model, sim_id,
           fill = list(est = 0, lwr95 = 0, upr95 = 0, lwr50 = 0, upr50 = 0)) %>%
  left_join(p2, by = c("category", "x"), suffix = c("_sim", "_true")) %>%
  mutate(sq_error = (est_sim - est_true)^2) %>%
  group_by(sim_id, model) %>%
  summarise(mse = mean(sq_error))

## Model fitting times
mis2_model_times$bayes[1] # Model compiling time
mean(mis2_model_times$bayes[-1]) # Bayes times
mean(mis2_model_times$freq) # Freq times

# Hierarchical situation
## Parameter values
k <- 5
probs0 <- c(0.05, 0.05, 0.325, 0.325, 0.25)
N <- 200 # sample size
n <- N/2
# nt <- 50 # number of trials
b1 <- log(3)
alpha <- prob_to_alpha(probs0, link = "logit")
alphai <- rnorm(5, 0, sb0) # Random effects for the clusters
y0 <- y1 <- expand.grid(id = 1:n)
y0$x <- 0
y1$x <- 1

## Calculating probabilities for every cluster and when x = 1
lp <- lapply(alpha, function(x) x - log(or) * y$x - alphai[y$cluster])
names(lp) <- sprintf("lp_leq%s", 1:(k-1))
lp <- data.frame(lp)
cump <- lapply(lp, arm::invlogit)
cump <- data.frame(cump)
names(cump) <- sprintf("cump_leq%s", 1:(k-1))
cump <- cbind(0, cump, 1)
p <- apply(cump, 1, diff, simplify = FALSE)
p <- data.frame(do.call(rbind, p))
names(p) <- sprintf("p%s", 1:k)
ph <- bind_cols(p, x = y$x, cluster = y$cluster)
ph <- ph %>%
  distinct(x, cluster, .keep_all = TRUE) %>%
  arrange(x, cluster)
ph <- ph %>%
  rename("1" = p1, "2" = p2, "3" = p3, "4" = p4, "5" = p5)
phl <- ph %>%
  pivot_longer(!c(x, cluster), names_to = "category", values_to = "est") %>%
  mutate(x = factor(x), cluster = factor(cluster))

## Tible for the model times
hier_model_times <- tibble(freq = as.numeric(), bayes = as.numeric())
## Tibbles for results
hier_model_estimates_prob <- tibble()
hier_model_estimates_para <- tibble()

## Helper function to calculate the cluster specific estimates
get_category_probs <- function(x, cluster, beta, ranef, thresholds) {
  eta <- beta * x + ranef
  cumprobs <- plogis(thresholds - eta)
  probs <- numeric(length = length(thresholds) + 1)
  probs[1] <- cumprobs[1]
  for (k in 2:length(thresholds)) {
    probs[k] <- cumprobs[k] - cumprobs[k - 1]
  }
  probs[length(probs)] <- 1 - cumprobs[length(thresholds)]
  return(probs)
}

# Simulations
for (i in 1:50) {
  # Simulating data
  cluster <- rep(1:5, c(6 * N/20, 5* N/20, 2 * N/20, 3 *N/20, 4 *N/20)) %>%
    sample()
  y <- rbind(y0, y1)
  y$cluster <- cluster

  ystar <- with(y, alphai[cluster] + (b1 * x)) + rlogis(nrow(y), 0, 1)
  y$y2 <- ordered(findInterval(ystar, alpha) + 1)
  y$cluster <- as.factor(y$cluster)
  y$x <- as.factor(y$x)
  # Fitting the frequentist model
  start_time <- Sys.time()
  fit_f_h <- clmm(y2 ~ x + (1 | cluster), data = y, lin = "logit")
  end_time <- Sys.time()
  hier_model_times[i,1] <- as.numeric(end_time - start_time)

  # Getting the frequentist estimates
  freq_est <- cbind(t(confint(fit_f_h)[5,]), Estimate = fit_f_h$beta) %>%
    as.data.frame() %>%
    rename("Q2.5" = "2.5 %", "Q97.5" = "97.5 %") %>%
    mutate(model = "Frequentist")
  freq_int <- expand.grid(
    cluster = 1:5,
    x = c(0, 1)) %>%
    rowwise() %>%
    mutate(
      probs = list(
        get_category_probs(x, cluster,
                           beta = fit_f_h$beta,
                           ranef = fit_f_h$ranef[cluster],
                           thresholds = fit_f_h$alpha))) %>%
    unnest_wider(probs, names_sep = "_") %>%
    rename_with(~ paste0(1:5), starts_with("probs_")) %>%
    pivot_longer(!c(cluster, x), names_to = "category", values_to = "est") %>%
    mutate(cluster = as.factor(cluster), x = factor(x),
           model = "Frequentist")

  # Fitting the Bayesian model
  y$y2 <- as.numeric(as.character(y$y2))
  start_time <- Sys.time()
  fit_b_h <- brm(formula = y2 ~ x + (1 | cluster), data = y,
                 family = cumulative("logit"),
                 prior = prior(normal(0,1), class = b, coef = "x1") +
                   prior(normal(-1.4,1), class = "Intercept", coef = "1") +
                   prior(normal(-0.4,1), class = "Intercept", coef = "2") +
                   prior(normal(0.4,1), class = "Intercept", coef = "3") +
                   prior(normal(1.4,1), class = "Intercept", coef = "4") +
                   prior(normal(0,1), class = "sd"),
                 control = list(adapt_delta = 0.99), cores = 4,
                 silent = 2, refresh = 0, file = "hier_model")
  end_time <- Sys.time()
  hier_model_times[i,2] <- as.numeric(end_time - start_time)

  bayes_est <- fixef(fit_b_h)[5,-2] %>%
    t() %>%
    as.data.frame() %>%
    mutate(model = "Bayes")
  bayes_int <- add_epred_draws(object = fit_b_h,
                               newdata = expand_grid(x = c(0,1),
                                                     cluster = c(1,2,3,4,5))) %>%
    group_by(.category, x, cluster) %>%
    summarise(est = mean(.epred), lwr95 = quantile(.epred, 0.025),
              upr95 = quantile(.epred, 0.975), lwr50 = quantile(.epred, 0.25),
              upr50 = quantile(.epred, 0.75)) %>%
    mutate(model = "Bayes", x = as.factor(x), cluster = factor(cluster)) %>%
    rename(category = .category)

  # Saving the values
  hier_model_estimates_prob <- bind_rows(bayes_int, freq_int) %>%
    mutate(sim_id = i) %>%
    bind_rows(hier_model_estimates_prob)

  hier_model_estimates_para <- bind_rows(bayes_est, freq_est) %>%
    mutate(sim_id = i) %>%
    bind_rows(hier_model_estimates_para)
}

## Parameter coverage
hier_model_estimates_para %>%
  mutate(covered = (log(or) >= Q2.5 & log(or) <= Q97.5)) %>%
  group_by(model) %>%
  summarise(coverage_rate = mean(covered))

## Calculating MSE values
hier_model_estimates_prob %>%
  ungroup() %>%
  complete(category, x, model, sim_id,
           fill = list(est = 0, lwr95 = 0, upr95 = 0, lwr50 = 0, upr50 = 0)) %>%
  left_join(phl, by = c("category", "x", "cluster"), suffix = c("_sim", "_true")) %>%
  mutate(sq_error = (est_sim - est_true)^2) %>%
  group_by(model) %>%
  summarise(mse = mean(sq_error))


# Saving all the probability estimate datasets
## Optimal
saveRDS(optimal_model_estimates_prob, file = "opt_est.rds")
## One missing category
saveRDS(mis1_model_estimates_prob, file = "mis1_est.rds")
## Two missing categories
saveRDS(mis2_model_estimates_prob, file = "mis2_est.rds")
## Hierarchical
saveRDS(hier_model_estimates_prob, file = "hier_est.rds")
