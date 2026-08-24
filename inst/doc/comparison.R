## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6,
  message = FALSE,
  warning = FALSE,
  fig.alt = "ggchangepoint plot comparing changepoint detection methods on a time series"
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_light())

# Optional (Suggests) engines: gate the chunks that need them so the
# vignette builds on any installation.
has_fpop        <- requireNamespace("fpop", quietly = TRUE)
has_wbs         <- requireNamespace("wbs", quietly = TRUE)
has_stepR       <- requireNamespace("stepR", quietly = TRUE)
has_strucchange <- requireNamespace("strucchange", quietly = TRUE)

# The comparison and benchmarking sections run whichever detectors are
# installed. The fallback set comes entirely from the changepoint package (a
# hard dependency), so both branches compare multiple-changepoint detectors
# and the narrative below holds either way.
cmp_methods <- if (has_fpop && has_wbs) {
  c("pelt", "binseg", "fpop", "wbs")
} else {
  c("pelt", "binseg", "segneigh")
}

## ----compare-facet------------------------------------------------------------
set.seed(2024)
x <- c(rnorm(150, 0), rnorm(150, 3), rnorm(200, 1))
ggcpt_compare(x, methods = cmp_methods)

## ----compare-nochange---------------------------------------------------------
set.seed(7)
x_null <- rnorm(300)
ggcpt_compare(x_null, methods = c("pelt", "binseg"))

## ----compare-overlay----------------------------------------------------------
ggcpt_compare(x, methods = cmp_methods, layout = "overlay")

## ----compare-table------------------------------------------------------------
ggcpt_compare_table(x, methods = cmp_methods)

## ----metrics-basic------------------------------------------------------------
# perfect detection
cpt_metrics(pred = c(150, 300), truth = c(150, 300), n = 500)

# near misses within the margin still match one-to-one
cpt_metrics(pred = c(148, 305), truth = c(150, 300), n = 500, margin = 5)

# three predictions around one truth: one TP, precision 1/3
cpt_metrics(pred = c(148, 150, 152), truth = c(150), n = 500, margin = 5)

## ----metrics-edges------------------------------------------------------------
# a correct "no change" answer is rewarded
cpt_metrics(pred = integer(0), truth = integer(0), n = 300)

# empty prediction against one true change at the midpoint: the trivial
# one-segment partition still overlaps half the series, so covering is 0.5
cpt_metrics(pred = integer(0), truth = c(150), n = 300)

## ----metrics-outofrange, warning = TRUE---------------------------------------
# index 700 cannot be a changepoint of a length-500 series
cpt_metrics(pred = c(100, 700), truth = c(100, 300), n = 500)

## ----annotated----------------------------------------------------------------
annotations <- list(
  ann1 = c(150, 300),
  ann2 = c(152, 301),
  ann3 = c(149)          # a third annotator missed the second change
)
cpt_metrics_annotated(pred = c(150, 300), annotations, n = 500, margin = 5)

## ----eval-plot, fig.alt = "Series with predictions coloured as true positives and false positives, shaded tolerance windows around each true changepoint, and missed truths as dashed rules"----
truth <- c(150, 300)
pred <- c(151, 240)      # one hit, one false alarm, one miss
ggcpt_eval(pred, truth, data_vec = x, margin = 5)
cpt_metrics(pred, truth, n = length(x), margin = 5)

## ----smuce-ci, eval = has_stepR, fig.alt = "Series with the SMUCE step fit overlaid and confidence-interval whiskers for each estimated changepoint location"----
# res_smuce <- smuce_wrapper(x)
# tidy(res_smuce)
# autoplot(res_smuce, show_ci = TRUE, show_fit = TRUE)

## ----strucchange-ci, eval = has_strucchange-----------------------------------
# res_bp <- strucchange_wrapper(x)
# tidy(res_bp)

## ----stability, fig.alt = "Bootstrap detection-frequency profile along the series index, with the original changepoints marked as dashed rules"----
st <- cpt_stability(x, method = "pelt", B = 30, seed = 1)
st
autoplot(st)

## ----crops--------------------------------------------------------------------
path <- cpt_crops(x)
path

## ----crops-elbow, fig.alt = "CROPS cost elbow: segmentation cost plotted against the number of changepoints for each solution on the penalty path"----
path_wide <- cpt_crops(x, pen_min = 4)
autoplot(path_wide)                          # cost elbow

## ----crops-segs, fig.alt = "The candidate CROPS segmentations, faceted by number of changepoints, each panel showing the series with that solution's changepoints"----
autoplot(path_wide, type = "segmentations")

## ----signals, fig.height = 4, fig.alt = "Donoho-Johnstone blocks test signal with its eleven true changepoints marked by vertical rules"----
blocks <- signal_blocks(n = 500, seed = 3)
ggplot(blocks, aes(index, value)) +
  geom_line(colour = "grey40") +
  geom_vline(xintercept = attr(blocks, "true_changepoints"),
             colour = "blue", linewidth = 0.3) +
  labs(title = "signal_blocks(): Donoho-Johnstone blocks with true changepoints")

## ----benchmark----------------------------------------------------------------
methods <- cmp_methods[1:3]
n_rep <- 10
regimes <- list(
  "high SNR (jump 2.5 sd)" = c(0, 2.5, 0.5),
  "low SNR (jump 1.0 sd)"  = c(0, 1.0, 0.3)
)

results <- do.call(rbind, lapply(names(regimes), function(g) {
  do.call(rbind, lapply(seq_len(n_rep), function(r) {
    dat <- cpt_simulate(400, changepoints = c(130, 260), change_in = "mean",
                        params = regimes[[g]], sd = 1, seed = 100 + r)
    truth <- attr(dat, "true_changepoints")
    do.call(rbind, lapply(methods, function(m) {
      fit <- cpt_detect(dat$value, method = m)
      score <- cpt_metrics(fit$changepoints$cp, truth, n = 400, margin = 5)
      cbind(tibble::tibble(regime = g, rep = r, method = m),
            score[, c("precision", "recall", "f1", "covering")])
    }))
  }))
}))

# average over replications, within regime
res_summary <- aggregate(cbind(precision, recall, f1, covering) ~ method + regime,
                         data = results, FUN = mean)
knitr::kable(res_summary, digits = 3,
             caption = "Mean accuracy over 10 replications per regime (margin = 5).")

