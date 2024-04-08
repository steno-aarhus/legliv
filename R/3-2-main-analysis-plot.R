# Assuming 'model0t', 'model1t', 'model0r', and 'model1r' are already defined

# Load required libraries
library(ggplot2)
library(gridExtra)

tidy_model <- function(model_object, model_name) {
  # For Hazard Ratios, you need to exponentiate the estimates and CI.
  tidy(model_object, conf.int = TRUE, exponentiate = TRUE) |>
    mutate(model_name = model_name)
}

combined_legume_results <- list(
  model0t = model0t,
  model1t = model1t,
  model2t = model2t,
  model0r = model0r,
  model1r = model1r,
  model2r = model2r,
  model0p = model0p,
  model1p = model1p,
  model2p = model2p,
) |>
  # Tidy each model using `tidy_model`
  purrr::imap(tidy_model) |>
  purrr::list_rbind() |>
  dplyr::filter(term == "legume_daily_15") |>
  dplyr::mutate(model_name = model_name |>
    # Sets the given order
    fct_relevel(
      "model2p", "model1p", "model0p",
      "model2r", "model1r", "model0r",
      "model2t", "model1t", "model0t"
    ))

plot_all <- ggplot(
  combined_legume_results,
  aes(x = model, y = estimate, ymin = conf.low, ymax = conf.high)
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
