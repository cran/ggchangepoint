## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 6,
  fig.alt = "ggchangepoint plot comparing changepoint detection methods on a time series"
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_light())

# `fpop` lives in Suggests; fall back to core engines when it is unavailable
# so the vignette builds in any environment.
has_fpop <- requireNamespace("fpop", quietly = TRUE)
cmp_methods <- if (has_fpop) c("pelt", "binseg", "fpop") else c("pelt", "binseg", "amoc")

## -----------------------------------------------------------------------------
set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1), rnorm(100, 5, 2))

## ----fig-1--------------------------------------------------------------------
ggcpt_compare(x, methods = cmp_methods, layout = "facet")

## ----fig-2--------------------------------------------------------------------
ggcpt_compare(x, methods = cmp_methods, layout = "overlay")

## -----------------------------------------------------------------------------
ggcpt_compare_table(x, methods = cmp_methods)

## -----------------------------------------------------------------------------
truth <- c(100, 200)

# PELT
res_pelt <- cpt_detect(x, method = "pelt", change_in = "meanvar")
generics::tidy(res_pelt)$cp

# Metrics for BinSeg
res_binseg <- cpt_detect(x, method = "binseg", change_in = "meanvar")
cpt_metrics(generics::tidy(res_binseg)$cp, truth, n = length(x))

## ----fig-3--------------------------------------------------------------------
ggcpt_eval(pred = c(100), truth = c(100, 200), data_vec = x)

## ----eval-study---------------------------------------------------------------
set.seed(42)
signals <- list(
  blocks = signal_blocks(512),
  fms    = signal_fms(512),
  mix    = signal_mix(512),
  teeth  = signal_teeth(512),
  stairs = signal_stairs(512)
)
methods <- c("pelt", "binseg", "fpop", "wbs", "not")
margin <- 5

results <- do.call(rbind, lapply(names(signals), function(nm) {
  sig <- signals[[nm]]
  truth <- attr(sig, "true_changepoints")
  do.call(rbind, lapply(methods, function(m) {
    res <- tryCatch(
      cpt_detect(sig$value, method = m, change_in = "mean"),
      error = function(e) NULL
    )
    if (is.null(res)) return(NULL)
    pred <- generics::tidy(res)$cp
    met <- cpt_metrics(pred, truth, n = length(sig$value), margin = margin)
    data.frame(signal = nm, method = m, met)
  }))
}))
results[, c("signal", "method", "precision", "recall", "f1", "covering",
            "hausdorff")]

## -----------------------------------------------------------------------------
set.seed(1)
dat <- cpt_simulate(200, changepoints = c(60, 120),
                    change_in = "mean", params = c(0, 8, 3), sd = 1)
truth <- attr(dat, "true_changepoints")

res <- cpt_detect(dat$value, method = "pelt", change_in = "mean")
pred <- generics::tidy(res)$cp

cpt_metrics(pred, truth, n = length(dat$value))

## -----------------------------------------------------------------------------
cpt_metrics_annotated(pred = c(100, 200),
                      annotations = list(c(100, 200), c(105, 198)),
                      n = 300)

## ----eval=FALSE---------------------------------------------------------------
# future::plan(future::multisession, workers = 2)
# ggcpt_compare(x, methods = c("pelt", "binseg", "wbs", "fpop", "not"),
#               seed = 1)

