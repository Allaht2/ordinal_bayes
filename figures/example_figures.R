###### Code for the exmaple plots used in the thesis #########

# Libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

## The latent distribution example plot
sections_data <- data.frame(
  x = c(-4, -2, 0, 2, 4),
  y = 0.20,
  label = paste0("Y = ", 1:5)
)

latent <- tibble(x1 = 0:1,
                 mu = c(0, 2)) %>%
  expand(nesting(x1, mu),
         x = seq(from = -10, to = 10, length.out = 1000)) %>%
  mutate(d = dlogis(x, location = mu, scale = 1),
         vari = factor(x1, labels = c(expression(x[1]), expression(x[2])))) %>%
  ggplot(aes(x = x, y = d, fill = vari)) +
  geom_area(alpha = 1/2, position = "identity") +
  geom_vline(xintercept = c(-3, -1, 1, 3), linetype = 3, color = "darkred") +
  scale_fill_manual("Variable\nvalue",
                    values = c("red3", "blue3"), labels = c(expression(x[1]), expression(x[2]))) +
  scale_x_continuous(expression(tilde(Y)), breaks = -8:8,
                     sec.axis = dup_axis(
                       name = NULL,
                       breaks = c(-3, -1, 1, 3),
                       labels = parse(text = str_c("alpha[", 1:4, "]"))
                     )) +
  scale_y_continuous(NULL, breaks = NULL) +
  coord_cartesian(xlim = c(-8, 8)) +
  geom_label(data = sections_data, aes(x = x, y = y, label = label), size = 4, vjust = 1,
             inherit.aes = FALSE) +
  theme_bw(base_size = 20) +
  theme(legend.position = c(0.9,0.5),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.key.size = unit(0.3, "cm"))

### Saving the plot
ggsave(filename = "latent.png", plot = latent,
       width = 2100, height = 1500, units = "px")


# Example of cumulative probabilites

thresholds <- c(-2, -1, 0, 1)
beta <- 1.2

### Define a sequence of predictor values (X)
x_values <- seq(-3, 3, length.out = 100)

### Calculate cumulative probabilities for each threshold
cumulative_probs <- data.frame(
  X = x_values,
  P_Y_leq_1 = plogis(thresholds[1] + beta * x_values),
  P_Y_leq_2 = plogis(thresholds[2] + beta * x_values),
  P_Y_leq_3 = plogis(thresholds[3] + beta * x_values),
  P_Y_leq_4 = plogis(thresholds[4] + beta * x_values)
)

cumulative_probs <- tibble(
  X = x_values,
  "2" = plogis(thresholds[2] + beta * x_values),
  "1" = plogis(thresholds[1] + beta * x_values),
  "3" = plogis(thresholds[3] + beta * x_values),
  "4" = plogis(thresholds[4] + beta * x_values)
)
# Reshape data for plotting
plot_data <- reshape2::melt(cumulative_probs, id.vars = "X",
                            measure.vars = c("P_Y_leq_1", "P_Y_leq_2", "P_Y_leq_3", "P_Y_leq_4"),
                            variable.name = "Threshold",
                            value.name = "Probability")
plot_data <- reshape2::melt(cumulative_probs, id.vars = "X",
                            measure.vars = c("1", "2", "3", "4"),
                            variable.name = "Threshold",
                            value.name = "Probability")

# Plot the cumulative probabilities
cumu_prob <- ggplot(plot_data, aes(x = X, y = Probability, color = Threshold)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("blue", "orange", "green", "purple"),
    labels = c("1", "2", "3", "4")
  ) +
  labs(
    x = "x",
    y = expression("Cumulative Probability " ~P(Y <= j)),
    color = "Category j"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.key.size = unit(0.3, "cm")) +
  geom_hline(yintercept = 0, color = "grey") + geom_vline(xintercept = -3, color = "grey")

ggsave(filename = "cumu_prob.png", plot = cumu_prob,
       width = 2000, height = 1250, units = "px")
