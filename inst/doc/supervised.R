## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  dpi = 72,
  message = FALSE,
  warning = FALSE,
  fig.alt = "ggchangepoint supervised detection plot"
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_ggcpt())
has_pl <- requireNamespace("penaltyLearning", quietly = TRUE)

## ----labels-------------------------------------------------------------------
set.seed(2026)
x <- c(rnorm(80), rnorm(80, 4), rnorm(80, 1))

labs <- cpt_labels(
  start  = c(  1,  60, 100, 140, 190),
  end    = c( 55,  95, 135, 185, 240),
  change = c("no_change", "one_change", "no_change", "one_change",
             "no_change")
)
labs

## ----labels-plot, fig.alt = "Series with shaded label regions behind it, coloured by what each label asserts"----
d <- data.frame(t = seq_along(x), y = x)
ggplot(d, aes(t, y)) +
  geom_cpt_label(aes(xmin = start, xmax = end, fill = change), data = labs) +
  geom_line(colour = "grey30") +
  scale_fill_cpt_label() +
  labs(fill = "Label", x = "Index", y = "Value")

## ----as-labels----------------------------------------------------------------
as_cpt_labels(c(80, 160), n = 240, margin = 5)

## ----label-error--------------------------------------------------------------
fit <- cpt_detect(x, method = "pelt")
err <- cpt_label_error(fit, labs)
err

## ----label-error-plot, fig.alt = "Series with label regions shaded green for correct, orange for false positive and red for false negative"----
ggplot(d, aes(t, y)) +
  geom_cpt_label(aes(xmin = start, xmax = end, fill = status), data = err) +
  geom_line(colour = "grey30") +
  geom_changepoint(data = tidy(fit), aes(xintercept = cp),
                   colour = "#0072B2", linewidth = 0.6) +
  scale_fill_cpt_label() +
  labs(fill = "Outcome", x = "Index", y = "Value")

## ----curve--------------------------------------------------------------------
curve <- cpt_label_error_curve(x, labs, method = "pelt")
curve

## ----curve-plot, fig.alt = "False positives, false negatives and total label errors against the penalty on a log scale, with the target interval shaded"----
autoplot(curve)

## ----learn--------------------------------------------------------------------
set.seed(5301)
series <- list(
  a = c(rnorm(60), rnorm(60, 4)),
  b = c(rnorm(80), rnorm(80, 2)),
  c = c(rnorm(70), rnorm(70, 6)),
  d = c(rnorm(100, 0, 3), rnorm(100, 9, 3))
)
labels <- list(
  a = as_cpt_labels(60, n = 120),
  b = as_cpt_labels(80, n = 160),
  c = as_cpt_labels(70, n = 140),
  d = as_cpt_labels(100, n = 200)
)
model <- cpt_learn_penalty(series, labels, penalties = 2^(0:10))
model

## ----use-model----------------------------------------------------------------
predict(model, series$d)
cpt_detect(series$d, method = "pelt", penalty = model)

## ----use-model-2--------------------------------------------------------------
cpt_penalty(model, series = series$d)

## ----compare-default----------------------------------------------------------
nrow(cpt_detect(series$d, method = "pelt")$changepoints)
nrow(cpt_detect(series$d, method = "pelt", penalty = model)$changepoints)

## ----engine, eval = has_pl----------------------------------------------------
cpt_learn_penalty(series, labels, penalties = 2^(0:10),
                  engine = "native")$fit$engine

