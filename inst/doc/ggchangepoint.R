## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_light())

has_fpop   <- requireNamespace("fpop", quietly = TRUE)
has_wbs    <- requireNamespace("wbs", quietly = TRUE)
has_not    <- requireNamespace("not", quietly = TRUE)
has_mosum  <- requireNamespace("mosum", quietly = TRUE)
has_idetect <- requireNamespace("IDetect", quietly = TRUE)
has_breakfast <- requireNamespace("breakfast", quietly = TRUE)

## ----prep---------------------------------------------------------------------
set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))

## ----detect-------------------------------------------------------------------
res <- cpt_detect(x, method = "pelt", change_in = "mean")
res

## ----broom--------------------------------------------------------------------
tidy(res)
glance(res)
augment(res)

## ----s3-----------------------------------------------------------------------
summary(res)
as_tibble(res)
plot(res)

## ----autoplot-----------------------------------------------------------------
autoplot(res, show_segments = TRUE)

## ----geoms, eval = FALSE------------------------------------------------------
# cp_tbl <- tidy(res)
# 
# # Standalone geom
# ggplot(data.frame(index = seq_along(x), value = x), aes(index, value)) +
#   geom_line() +
#   geom_changepoint(data = cp_tbl, aes(xintercept = cp), color = "red")
# 
# # Compute-and-draw stat
# ggplot(data.frame(index = seq_along(x), value = x), aes(index, value)) +
#   geom_line() +
#   stat_changepoint(method = "pelt", color = "red")
# 
# # Segments and CIs
# ggplot(data.frame(index = seq_along(x), value = x), aes(index, value)) +
#   geom_line() +
#   geom_cpt_segment(data = cp_tbl, aes(xintercept = cp))
# 
# # Theming and segment shading
# ggplot(data.frame(index = seq_along(x), value = x), aes(index, value)) +
#   geom_line() +
#   geom_changepoint(data = cp_tbl, aes(xintercept = cp)) +
#   theme_ggcpt() +
#   annotate_segments(cp = cp_tbl$cp, n = length(x))

## ----penalty------------------------------------------------------------------
cpt_penalty("BIC", n = 200)
cpt_penalty("MBIC", n = 200, k = 1)
cpt_penalty("Manual", value = 10)

## ----methods------------------------------------------------------------------
cpt_methods()

## ----wrappers, eval = FALSE---------------------------------------------------
# fpop_wrapper(x, penalty = 2 * log(200))
# wbs_wrapper(x, n_intervals = 2000)
# not_wrapper(x, contrast = "pcwsConstMean")
# mosum_wrapper(x)
# idetect_wrapper(x)
# tguh_wrapper(x)

## ----compare------------------------------------------------------------------
suppressWarnings(
  ggcpt_compare(x, methods = c("pelt", "binseg", "amoc"))
)

## ----compare-table------------------------------------------------------------
ggcpt_compare_table(x, methods = c("pelt", "binseg", "amoc"))

## ----metrics------------------------------------------------------------------
cpt_metrics(pred = c(100), truth = c(100), n = 200, margin = 5)
cpt_metrics_annotated(c(100), list(c(100), c(101), c(99)), n = 200, margin = 5)
ggcpt_eval(pred = c(100), truth = c(100), data_vec = x)

## ----simulate-----------------------------------------------------------------
dat <- cpt_simulate(200, changepoints = c(100), change_in = "mean",
                    params = c(0, 10), sd = 1)
attributes(dat)$true_changepoints
rcpt(200, changepoints = c(100), change_in = "mean", params = c(0, 10))

## ----signals------------------------------------------------------------------
signal_blocks(200)
signal_fms(200)
signal_mix(200)
signal_teeth(200)
signal_stairs(200)

## ----constructors, eval = FALSE-----------------------------------------------
# new_ggcpt(
#   changepoints = tibble::tibble(cp = 100L, cp_value = 5.0),
#   data = tibble::tibble(index = 1:200, value = rnorm(200)),
#   method = "manual"
# )
# is_ggcpt(res)

## ----original, eval = FALSE---------------------------------------------------
# cpt_wrapper(x, change_in = "mean")
# ecp_wrapper(x, algorithm = "divisive")
# ggcptplot(x)
# ggecpplot(x, algorithm = "divisive")

## ----parallel, eval = FALSE---------------------------------------------------
# future::plan("multisession")
# ggcpt_compare(x, methods = c("pelt", "binseg", "fpop", "wbs"))
# future::plan("sequential")

