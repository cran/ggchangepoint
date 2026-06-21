## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 8,
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  comment = "#>",
  fig.alt = "ggchangepoint plot of a time series with detected changepoints"
)
# The wrappers for the optional engines live in Suggests; gate the chunks that
# need them so the vignette still builds when an engine is not installed.
has_fpop <- requireNamespace("fpop", quietly = TRUE)
has_wbs  <- requireNamespace("wbs", quietly = TRUE)
has_not  <- requireNamespace("not", quietly = TRUE)

## -----------------------------------------------------------------------------
library(ggchangepoint)
library(ggplot2)
library(generics)
theme_set(theme_light())

set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))

res <- cpt_detect(x, method = "pelt", change_in = "mean")
class(res)

## -----------------------------------------------------------------------------
print(res)

## -----------------------------------------------------------------------------
res$cp_convention

## -----------------------------------------------------------------------------
generics::tidy(res)

## -----------------------------------------------------------------------------
generics::glance(res)

## -----------------------------------------------------------------------------
generics::augment(res)

## -----------------------------------------------------------------------------
cpt_detect(x, method = "pelt", change_in = "mean")

## ----eval = has_fpop----------------------------------------------------------
cpt_detect(x, method = "fpop", change_in = "mean")

## -----------------------------------------------------------------------------
cpt_detect(x, method = "pelt", change_in = "mean", penalty = "BIC")

## ----eval = has_fpop----------------------------------------------------------
cpt_detect(x, method = "fpop", change_in = "mean", penalty = 2 * log(200))

## -----------------------------------------------------------------------------
ggplot2::autoplot(res)

## -----------------------------------------------------------------------------
ggplot2::autoplot(res, show_segments = TRUE)

## -----------------------------------------------------------------------------
ggcptplot(x)
ggecpplot(x, min_size = 10)

## -----------------------------------------------------------------------------
ggplot(data.frame(t = seq_along(x), y = x), aes(t, y)) +
  geom_line() +
  geom_changepoint(data = generics::tidy(res), aes(xintercept = cp))

## -----------------------------------------------------------------------------
seg <- res$segments
ggplot(data.frame(t = seq_along(x), y = x), aes(t, y)) +
  geom_line() +
  geom_cpt_segment(data = seg,
                   aes(x = start, xend = end, y = param_estimate,
                       yend = param_estimate),
                   colour = "steelblue", linewidth = 1.2)

## -----------------------------------------------------------------------------
ggplot(data.frame(t = seq_along(x), y = x), aes(t, y)) +
  geom_line() +
  stat_changepoint(method = "pelt", change_in = "mean")

## -----------------------------------------------------------------------------
x3 <- c(rnorm(100, 0, 1), rnorm(100, 10, 1), rnorm(100, 5, 2))
cmp_methods <- if (has_fpop) c("pelt", "binseg", "fpop") else c("pelt", "binseg", "amoc")

ggcpt_compare(x3, methods = cmp_methods, layout = "facet")

## -----------------------------------------------------------------------------
ggcpt_compare(x3, methods = cmp_methods, layout = "overlay")

## -----------------------------------------------------------------------------
ggcpt_compare_table(x3, methods = cmp_methods)

## ----eval = FALSE-------------------------------------------------------------
# future::plan(future::multisession, workers = 2)
# ggcpt_compare(x, methods = c("pelt", "binseg", "fpop", "wbs", "not"),
#               seed = 1)

## -----------------------------------------------------------------------------
cpt_metrics(pred = c(100, 200), truth = c(100, 200), n = 300)

## -----------------------------------------------------------------------------
cpt_metrics(pred = c(105, 205), truth = c(100, 200), n = 300, margin = 10)

## -----------------------------------------------------------------------------
cpt_metrics_annotated(pred = c(100, 200),
                      annotations = list(c(100, 200), c(105, 198)),
                      n = 300)

## -----------------------------------------------------------------------------
ggcpt_eval(pred = c(100, 200), truth = c(100, 200), data_vec = x)

## -----------------------------------------------------------------------------
seg_params <- list(
  list(mean = 0, sd = 1),
  list(mean = 10, sd = 1),
  list(mean = 5, sd = 0.5),
  list(mean = -2, sd = 1)
)
dat <- cpt_simulate(200, changepoints = c(50, 100, 150),
                    change_in = "meanvar",
                    params = seg_params)

## -----------------------------------------------------------------------------
attr(dat, "true_changepoints")

## -----------------------------------------------------------------------------
blocks  <- signal_blocks(512)
fms     <- signal_fms(512)
mix     <- signal_mix(512)
teeth   <- signal_teeth(512)
stairs  <- signal_stairs(512)

## -----------------------------------------------------------------------------
set.seed(1)
sig <- signal_blocks(512)
truth <- attr(sig, "true_changepoints")

x_noisy <- sig$value + rnorm(512, 0, 0.5)

## -----------------------------------------------------------------------------
methods_cs <- c("pelt", "binseg", "amoc")
if (has_fpop) methods_cs <- c(methods_cs, "fpop")
if (has_wbs)  methods_cs <- c(methods_cs, "wbs")
if (has_not)  methods_cs <- c(methods_cs, "not")

metrics <- do.call(rbind, lapply(methods_cs, function(m) {
  res  <- cpt_detect(x_noisy, method = m, change_in = "mean")
  pred <- generics::tidy(res)$cp
  data.frame(method = m, cpt_metrics(pred, truth, n = 512, margin = 5))
}))
metrics[, c("method", "n_pred", "precision", "recall", "f1", "covering")]

## -----------------------------------------------------------------------------
pred_pelt <- generics::tidy(cpt_detect(x_noisy, method = "pelt"))$cp
ggcpt_eval(pred = pred_pelt, truth = truth, data_vec = x_noisy)

