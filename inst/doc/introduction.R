## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 8,
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  comment = "#>",
  fig.alt = "ggchangepoint plot of a time series with its detected changepoints"
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_light())

# Optional (Suggests) engines: gate the chunks that need them so the
# vignette builds on any installation.
has_stepR       <- requireNamespace("stepR", quietly = TRUE)
has_cpop        <- requireNamespace("cpop", quietly = TRUE)
has_bcp         <- requireNamespace("bcp", quietly = TRUE)
has_ocp         <- requireNamespace("ocp", quietly = TRUE)
has_fpop        <- requireNamespace("fpop", quietly = TRUE)
has_wbs         <- requireNamespace("wbs", quietly = TRUE)
has_not         <- requireNamespace("not", quietly = TRUE)
has_mosum       <- requireNamespace("mosum", quietly = TRUE)
has_idetect     <- requireNamespace("IDetect", quietly = TRUE)
has_breakfast   <- requireNamespace("breakfast", quietly = TRUE)
has_cpm         <- requireNamespace("cpm", quietly = TRUE)
has_decafs      <- requireNamespace("DeCAFS", quietly = TRUE)
has_inspect     <- requireNamespace("InspectChangepoint", quietly = TRUE)
has_strucchange <- requireNamespace("strucchange", quietly = TRUE)
has_segmented   <- requireNamespace("segmented", quietly = TRUE)
has_fastcpd     <- requireNamespace("fastcpd", quietly = TRUE)
has_envcpt      <- requireNamespace("EnvCpt", quietly = TRUE)

## ----contract-----------------------------------------------------------------
set.seed(2022)
x <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
res <- cpt_detect(x, method = "pelt", change_in = "mean")
res
tidy(res)
glance(res)
head(augment(res))

## ----contract-plot------------------------------------------------------------
autoplot(res, show_segments = TRUE)

## ----methods-table------------------------------------------------------------
cpt_methods()

## ----penalty------------------------------------------------------------------
cpt_penalty("BIC", n = 200)
cpt_penalty("MBIC", n = 200)
cpt_penalty("Hannan-Quinn", n = 200)
cpt_penalty("sSIC", n = 200)

## ----tour-data----------------------------------------------------------------
set.seed(2026)
x_mean  <- c(rnorm(100), rnorm(100, 4))                # mean shift at 100
x_multi <- c(rnorm(100), rnorm(100, 3), rnorm(100, -1)) # shifts at 100, 200
x_slope <- cumsum(c(rep(0.4, 100), rep(-0.3, 100))) + rnorm(200) # kink at 100

## ----penalised----------------------------------------------------------------
tidy(cpt_detect(x_multi, method = "pelt"))
tidy(cpt_detect(x_multi, method = "binseg"))

## ----penalised-fpop, eval = has_fpop------------------------------------------
tidy(cpt_detect(x_multi, method = "fpop"))

## ----crops, fig.alt = "CROPS elbow plot: segmentation cost against the number of changepoints"----
path <- cpt_crops(x_multi)
path
autoplot(path)

## ----crops-segmentations, fig.alt = "The series faceted by CROPS solution, each panel showing that solution's changepoints"----
autoplot(path, type = "segmentations")

## ----fastcpd, eval = has_fastcpd----------------------------------------------
# tidy(fastcpd_wrapper(x_multi, family = "mean"))

## ----search-wbs, eval = has_wbs-----------------------------------------------
tidy(wbs_wrapper(x_multi, seed = 1))

## ----search-not, eval = has_not-----------------------------------------------
tidy(not_wrapper(x_multi, seed = 1))

## ----search-mosum, eval = has_mosum-------------------------------------------
tidy(mosum_wrapper(x_multi))

## ----search-others, eval = has_idetect && has_breakfast-----------------------
tidy(idetect_wrapper(x_multi, seed = 1))
tidy(wbs2_wrapper(x_multi))
tidy(tguh_wrapper(x_multi))

## ----smuce, eval = has_stepR, fig.alt = "SMUCE step fit with changepoint-location confidence intervals drawn as horizontal whiskers"----
# res_smuce <- smuce_wrapper(x_multi)
# tidy(res_smuce)
# autoplot(res_smuce, show_ci = TRUE, show_fit = TRUE)

## ----cpop, eval = has_cpop----------------------------------------------------
# res_cpop <- cpop_wrapper(x_slope)
# tidy(res_cpop)
# autoplot(res_cpop, show_fit = TRUE)

## ----not-slope, eval = has_not------------------------------------------------
tidy(cpt_detect(x_slope, method = "not", change_in = "slope"))

## ----bcp, eval = has_bcp, fig.alt = "Two-panel Bayesian display: the series with its posterior mean above, per-location posterior changepoint probability below"----
# res_bcp <- bcp_wrapper(x_mean, seed = 2026)
# tidy(res_bcp)
# ggcpt_posterior(res_bcp)

## ----bocpd, eval = has_ocp, fig.alt = "Run-length heatmap: posterior probability of each run length over time"----
# res_bocpd <- bocpd_wrapper(x_mean)
# tidy(res_bocpd)
# ggcpt_runlength(res_bocpd)

## ----nonparam-----------------------------------------------------------------
set.seed(2022)
tidy(cpt_detect(x_mean, method = "np"))
tidy(cpt_detect(x_mean, method = "ecp", seed = 1))

## ----cpm, eval = has_cpm------------------------------------------------------
# tidy(cpm_wrapper(x_mean, cpm_type = "Mann-Whitney"))

## ----decafs, eval = has_decafs------------------------------------------------
# res_decafs <- decafs_wrapper(x_mean)
# tidy(res_decafs)
# autoplot(res_decafs, show_fit = TRUE)

## ----envcpt, eval = has_envcpt------------------------------------------------
# res_env <- envcpt_wrapper(x_mean, models = c("mean", "meancpt", "trendcpt"))
# glance(res_env)

## ----inspect, eval = has_inspect, fig.alt = "Faceted small-multiples, one panel per coordinate, sharing the detected changepoint rules"----
# set.seed(2026)
# X <- cbind(a = c(rnorm(80), rnorm(80, 3)),
#            b = c(rnorm(80), rnorm(80, -2)),
#            c = rnorm(160))
# res_hd <- inspect_wrapper(X)
# tidy(res_hd)
# autoplot(res_hd)

## ----strucchange, eval = has_strucchange--------------------------------------
# res_bp <- strucchange_wrapper(x_mean)
# tidy(res_bp)
# autoplot(res_bp, show_ci = TRUE)

## ----segmented, eval = has_segmented------------------------------------------
# res_seg <- segmented_wrapper(x_slope, npsi = 1, seed = 1)
# tidy(res_seg)
# autoplot(res_seg, show_fit = TRUE, show_ci = TRUE)

## ----batch, fig.alt = "Small-multiples of a panel of series, each with its own detected changepoints"----
set.seed(2026)
panel <- cbind(shifted = x_mean, quiet = rnorm(200))
batch <- cpt_batch(panel, method = "pelt")
batch
tidy(batch)
autoplot(batch)

## ----stability, fig.alt = "Bootstrap detection-frequency profile across the series, with the original changepoints marked"----
st <- cpt_stability(x_mean, method = "pelt", B = 50, seed = 1)
st
autoplot(st)

## ----cite---------------------------------------------------------------------
cpt_cite("pelt")

