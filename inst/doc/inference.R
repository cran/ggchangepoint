## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  dpi = 72,
  message = FALSE,
  warning = FALSE,
  fig.alt = "ggchangepoint inference and diagnostic plot"
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_ggcpt())

has_nsp   <- requireNamespace("nsp", quietly = TRUE)
# mosum pulls in plot3D -> misc3d -> tcltk, which warns "no DISPLAY variable
# so Tk is not available" on a headless machine. Load it here, quietly.
has_mosum <- suppressWarnings(requireNamespace("mosum", quietly = TRUE))
has_wbs   <- requireNamespace("wbs", quietly = TRUE)
has_cvcp  <- requireNamespace("crossvalidationCP", quietly = TRUE)
has_infl  <- requireNamespace("changepoint.influence", quietly = TRUE)

## ----data---------------------------------------------------------------------
set.seed(2026)
x <- c(rnorm(120), rnorm(120, 3), rnorm(120, 0.5))
fit <- cpt_detect(x, method = "pelt")
fit

## ----confint-bootstrap--------------------------------------------------------
cpt_confint(fit, method = "bootstrap", B = 100, seed = 1)

## ----confint-native, eval = requireNamespace("stepR", quietly = TRUE)---------
sm <- smuce_wrapper(x)
cpt_confint(sm)

## ----nsp, eval = has_nsp------------------------------------------------------
res_nsp <- nsp_wrapper(x, alpha = 0.1, M = 200, seed = 1)
cpt_regions(res_nsp)

## ----nsp-plot, eval = has_nsp, fig.alt = "Series with shaded vertical bands marking NSP significance regions"----
autoplot(res_nsp)

## ----nsp-ar, eval = has_nsp---------------------------------------------------
y_ar <- as.numeric(stats::arima.sim(list(ar = 0.6), 300)) +
  rep(c(0, 3), each = 150)
cpt_regions(nsp_wrapper(y_ar, variant = "ar", ord = 1, M = 200, seed = 1))

## ----test---------------------------------------------------------------------
suppressWarnings(cpt_test(fit))

## ----test-native, eval = requireNamespace("strucchange", quietly = TRUE)------
suppressWarnings(cpt_test(strucchange_wrapper(x)))

## ----confint-nsp, eval = has_nsp----------------------------------------------
cpt_confint(fit, method = "nsp", level = 0.9, seed = 1)

## ----select-------------------------------------------------------------------
sel <- cpt_select(x, criterion = "mbic", k_max = 8)
sel

## ----select-criterion, fig.alt = "Criterion value against the number of changepoints, with the chosen model highlighted"----
autoplot(sel)

## ----select-ladder, fig.height = 7, fig.alt = "Small multiples showing how the segmentation coarsens as the number of changepoints falls"----
autoplot(sel, plot_type = "ladder", max_facets = 6)

## ----select-cv, eval = has_cvcp-----------------------------------------------
cpt_select(x, criterion = "cv", k_max = 8)$k

## ----influence----------------------------------------------------------------
inf <- cpt_influence(fit, engine = if (has_infl) "auto" else "recompute",
                     subset = if (has_infl) NULL else seq(1, 360, by = 6))
inf

## ----influence-overview, fig.alt = "Series with each observation sized and coloured by its influence on the segmentation"----
autoplot(inf)

## ----influence-map, eval = has_infl, fig.alt = "Influence map: perturbed observation on the horizontal axis, position on the vertical, segment-parameter shift as fill"----
autoplot(inf, plot_type = "map")

## ----leverage-----------------------------------------------------------------
head(cpt_leverage(inf), 5)

## ----sensitivity, fig.height = 7, fig.alt = "One facet per penalty setting, each showing the changepoints that setting finds"----
sens <- cpt_sensitivity(x, method = "pelt",
                        over = list(penalty = c(2, 8, 20, 60)))
sens
autoplot(sens)

## ----statistic, eval = has_mosum, fig.alt = "Two-panel display: the series above, the MOSUM statistic against its threshold below"----
res_mosum <- cpt_detect(x, method = "mosum")
autoplot(res_mosum, type = "statistic")

## ----ggcpt-statistic, eval = has_mosum, fig.alt = "The same two-panel statistic display, drawn by ggcpt_statistic() directly"----
ggcpt_statistic(res_mosum)

## ----scale-space, eval = has_mosum, fig.alt = "Heatmap of the MOSUM statistic by location and bandwidth, with accepted changepoints marked"----
ggcpt_scale_space(res_mosum, bandwidths = c(15, 30, 60, 90))

## ----scale-space-data, eval = has_mosum, warning = FALSE----------------------
ss <- cpt_scale_space(x, bandwidths = c(15, 30, 60, 90))
aggregate(significant ~ bandwidth, data = ss, FUN = sum)

## ----path, eval = has_wbs, fig.alt = "Solution path: each candidate changepoint against the step at which it entered, with the proposing interval drawn"----
ggcpt_solution_path(cpt_detect(x, method = "wbs"), max_steps = 20)

## ----solution-path-data, eval = has_wbs---------------------------------------
head(cpt_solution_path(cpt_detect(x, method = "wbs")), 5)

## ----statistic-error, error = TRUE--------------------------------------------
try({
cpt_statistic(fit)
})

## ----report-------------------------------------------------------------------
cat(head(cpt_report(fit, session = FALSE), 20), sep = "\n")

