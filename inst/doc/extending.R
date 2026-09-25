## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  dpi = 72,
  message = FALSE,
  warning = FALSE,
  fig.alt = "ggchangepoint plot of a result from an externally supplied detector"
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_ggcpt())

## ----as-ggcpt-----------------------------------------------------------------
set.seed(2026)
x <- c(rnorm(100), rnorm(100, 4), rnorm(100, 1))

# Pretend these came from a Python detector, a paper, or an analyst.
external <- c(101, 199)

fit <- as_ggcpt(external, x, method = "ruptures::Pelt",
                cp_convention = "right")
fit

## ----as-ggcpt-uses, fig.alt = "Series with the externally supplied changepoints drawn as vertical rules"----
tidy(fit)
glance(fit)
cpt_metrics(tidy(fit)$cp, truth = c(100, 200), n = 300)
autoplot(fit, show_segments = TRUE)

## ----as-ggcpt-contract--------------------------------------------------------
tidy(as_ggcpt(c(150, 150, 9999, NA, 50), x))$cp

## ----as-ggcpt-extras----------------------------------------------------------
with_ci <- as_ggcpt(c(100, 200), x, method = "external",
                    ci = cbind(c(95, 192), c(107, 205)),
                    extra = list(score = c(12.4, 8.1)))
tidy(with_ci)

## ----register-----------------------------------------------------------------
cpt_register_method(
  "biggest_jump",
  fn = function(x, window = 1, ...) {
    d <- abs(diff(as.numeric(x)))
    which.max(stats::filter(d, rep(1, window) / window, sides = 2))
  },
  change_in = "mean",
  engine = "example",
  citation = "No citation supplied (illustration only)."
)

res <- cpt_detect(x, method = "biggest_jump", window = 5)
res

## ----register-visible---------------------------------------------------------
subset(cpt_methods(), status == "registered")
cpt_cite("biggest_jump")

## ----register-tools-----------------------------------------------------------
cpt_consensus(x, methods = c("pelt", "binseg", "biggest_jump"),
              min_votes = 2)
cpt_benchmark(cpt_datasets(n = 200, seed = 1, names = c("step", "teeth")),
              methods = c("pelt", "biggest_jump"), progress = FALSE)

## ----unregister---------------------------------------------------------------
cpt_unregister_method("biggest_jump")

## ----register-full, fig.alt = "Series with the registered detector's changepoint marked and the smoothed signal it fitted overlaid"----
cpt_register_method(
  "smoothed_jump",
  fn = function(x, ...) {
    sm <- stats::filter(x, rep(1, 11) / 11, sides = 2)
    sm[is.na(sm)] <- x[is.na(sm)]
    as_ggcpt(which.max(abs(diff(sm))), x, fitted = as.numeric(sm))
  },
  engine = "example"
)
autoplot(cpt_detect(x, method = "smoothed_jump"), show_fit = TRUE)
cpt_unregister_method("smoothed_jump")

## ----reticulate, eval = FALSE-------------------------------------------------
# library(reticulate)
# rpt <- import("ruptures")
# 
# cpt_register_method(
#   "ruptures_pelt",
#   fn = function(x, model = "l2", pen = 10, ...) {
#     algo <- rpt$Pelt(model = model)$fit(matrix(as.numeric(x), ncol = 1))
#     # ruptures returns 1-based *right* endpoints, with n as the last entry
#     as.integer(unlist(algo$predict(pen = pen)))
#   },
#   change_in = "mean",
#   engine = "ruptures (Python)",
#   cp_convention = "left",
#   citation = paste("Truong, C., Oudre, L. and Vayatis, N. (2020).",
#                    "Selective review of offline change point detection",
#                    "methods. Signal Processing, 167, 107299.")
# )
# 
# cpt_detect(x, method = "ruptures_pelt", pen = 20)

## ----install, eval = FALSE----------------------------------------------------
# cpt_install_engines("bayesian")
# cpt_install_engines(c("highdim", "functional"), dry_run = TRUE)

## ----install-status-----------------------------------------------------------
tab <- cpt_methods()
table(status = tab$status, installed = tab$installed, useNA = "ifany")

