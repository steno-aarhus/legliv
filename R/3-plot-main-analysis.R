# Assuming 'model0t', 'model1t', 'model0r', and 'model1r' are already defined

# Load required libraries
library(ggplot2)
library(gridExtra)

# Calculate confidence intervals manually for model0t
tidy_model0t <- tidy(model0t)
tidy_model0t <- tidy_model0t %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Calculate confidence intervals manually for model1t
tidy_model1t <- tidy(model1t)
tidy_model1t <- tidy_model1t %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Calculate confidence intervals manually for model2t
tidy_model2t <- tidy(model2t)
tidy_model2t <- tidy_model2t %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Calculate confidence intervals manually for model0r
tidy_model0r <- tidy(model0r)
tidy_model0r <- tidy_model0r %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Calculate confidence intervals manually for model1r
tidy_model1r <- tidy(model1r)
tidy_model1r <- tidy_model1r %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Calculate confidence intervals manually for model2r
tidy_model2r <- tidy(model2r)
tidy_model2r <- tidy_model2r %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Calculate confidence intervals manually for model0p
tidy_model0p <- tidy(model0p)
tidy_model0p <- tidy_model0p %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Calculate confidence intervals manually for model1p
tidy_model1p <- tidy(model1p)
tidy_model1p <- tidy_model1p %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Calculate confidence intervals manually for model2p
tidy_model2p <- tidy(model2p)
tidy_model2p <- tidy_model2p %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Extract hazard ratio and confidence interval for 'legume_daily_15' from all models
legume_results_model0t <- tidy_model0t %>%
  filter(term == "legume_daily_15")
legume_results_model1t <- tidy_model1t %>%
  filter(term == "legume_daily_15")
legume_results_model2t <- tidy_model2t %>%
  filter(term == "legume_daily_15")
legume_results_model0r <- tidy_model0r %>%
  filter(term == "legume_daily_15")
legume_results_model1r <- tidy_model1r %>%
  filter(term == "legume_daily_15")
legume_results_model2r <- tidy_model2r %>%
  filter(term == "legume_daily_15")
legume_results_model0p <- tidy_model0p %>%
  filter(term == "legume_daily_15")
legume_results_model1p <- tidy_model1p %>%
  filter(term == "legume_daily_15")
legume_results_model2p <- tidy_model2p %>%
  filter(term == "legume_daily_15")

desired_order <- c(
  "model2p", "model1p", "model0p",
  "model2r", "model1r", "model0r",
  "model2t", "model1t", "model0t"
)

combined_legume_results <- combined_legume_results %>%
  mutate(model = factor(model, levels = desired_order))

# Combine results for 'legume_daily_15' from all models
combined_legume_results <- rbind(
  cbind(legume_results_model2t, model = "model2t"),
  cbind(legume_results_model1t, model = "model1t"),
  cbind(legume_results_model0t, model = "model0t"),
  cbind(legume_results_model2r, model = "model2r"),
  cbind(legume_results_model1r, model = "model1r"),
  cbind(legume_results_model0r, model = "model0r"),
  cbind(legume_results_model2p, model = "model2p"),
  cbind(legume_results_model1p, model = "model1p"),
  cbind(legume_results_model0p, model = "model0p")
)

plot_all <- ggplot(
  combined_legume_results %>% filter(model %in% c("model0t", "model1t", "model2t", "model0r", "model1r", "model2r", "model0p", "model1p", "model2p")),
  aes(x = model, y = exp(estimate), ymin = exp(conf.low), ymax = exp(conf.high))
) +
  geom_pointrange() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(x = NULL, y = "Hazard Ratio") +
  theme_classic() +
  ggtitle("15 g/day substitution") +
  geom_text(aes(label = sprintf("%.2f (%.2f, %.2f)", exp(estimate), exp(conf.low), exp(conf.high))),
    hjust = 1, size = 3, fontface = "italic", color = "black", nudge_x = 0.05
  )
plot_all
