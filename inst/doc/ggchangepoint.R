## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  fig.alt = "ggchangepoint feature tour plot"
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_light())

# Optional engines live in Suggests; every chunk that needs one is guarded so
# the vignette builds on any installation.
has_fpop      <- requireNamespace("fpop", quietly = TRUE)
has_wbs       <- requireNamespace("wbs", quietly = TRUE)
has_breakfast <- requireNamespace("breakfast", quietly = TRUE)
has_not       <- requireNamespace("not", quietly = TRUE)
has_mosum     <- requireNamespace("mosum", quietly = TRUE)
has_idetect   <- requireNamespace("IDetect", quietly = TRUE)
has_stepR     <- requireNamespace("stepR", quietly = TRUE)
has_cpop      <- requireNamespace("cpop", quietly = TRUE)
has_bcp       <- requireNamespace("bcp", quietly = TRUE)
has_ocp       <- requireNamespace("ocp", quietly = TRUE)
# Rbeast (<= 1.0.2) can crash the vignette-building subprocess on Windows
# (its C internals have known memory-state issues); run it elsewhere only.
has_rbeast    <- requireNamespace("Rbeast", quietly = TRUE) &&
  .Platform$OS.type != "windows"
has_cpm       <- requireNamespace("cpm", quietly = TRUE)
has_kcprs     <- requireNamespace("kcpRS", quietly = TRUE)
has_cptnonpar <- requireNamespace("CptNonPar", quietly = TRUE)
has_decafs    <- requireNamespace("DeCAFS", quietly = TRUE)
has_snseg     <- requireNamespace("SNSeg", quietly = TRUE)
has_inspect   <- requireNamespace("InspectChangepoint", quietly = TRUE)
has_geomcp    <- requireNamespace("changepoint.geo", quietly = TRUE)
has_struc     <- requireNamespace("strucchange", quietly = TRUE)
has_segmented <- requireNamespace("segmented", quietly = TRUE)
has_envcpt    <- requireNamespace("EnvCpt", quietly = TRUE)
has_fastcpd   <- requireNamespace("fastcpd", quietly = TRUE)
has_plotly    <- requireNamespace("plotly", quietly = TRUE)

## ----data---------------------------------------------------------------------
set.seed(2026)
x <- c(rnorm(100, 0, 1), rnorm(100, 6, 1))    # one mean shift at t = 100
x3 <- c(rnorm(100), rnorm(100, 4), rnorm(100, 1))  # shifts at 100, 200

## ----class--------------------------------------------------------------------
res <- cpt_detect(x, method = "pelt", change_in = "mean")
is_ggcpt(res)
print(res)

## ----constructor--------------------------------------------------------------
manual <- new_ggcpt(
  changepoints = tibble::tibble(cp = 100L, cp_value = x[100]),
  data = tibble::tibble(index = seq_along(x), value = x),
  method = "manual"
)
is_ggcpt(manual)

## ----broom--------------------------------------------------------------------
tidy(res)
glance(res)
head(augment(res))

## ----s3-----------------------------------------------------------------------
summary(res)
as_tibble(res)
head(as.data.frame(res))
format(res)

## ----plot-fallback------------------------------------------------------------
plot(res)

## ----detect-------------------------------------------------------------------
cpt_detect(x, method = "binseg", change_in = "mean")

## ----detect-dots, eval = has_not----------------------------------------------
tidy(cpt_detect(x, method = "not", contrast = "pcwsLinMean"))

## ----methods------------------------------------------------------------------
print(cpt_methods(), n = Inf)

## ----penalty------------------------------------------------------------------
cpt_penalty("BIC", n = 200)
cpt_penalty("MBIC", n = 200)
cpt_penalty("Manual", value = 10)

## ----classic------------------------------------------------------------------
cpt_wrapper(x, change_in = "mean", cp_method = "PELT")
ecp_wrapper(x, algorithm = "divisive", seed = 1)

## ----fpop, eval = has_fpop----------------------------------------------------
fpop_wrapper(x, penalty = 2 * log(length(x)))

## ----wbs, eval = has_wbs------------------------------------------------------
wbs_wrapper(x, n_intervals = 2000, seed = 1)

## ----wbs2, eval = has_breakfast-----------------------------------------------
wbs2_wrapper(x)

## ----not, eval = has_not------------------------------------------------------
not_wrapper(x, contrast = "pcwsConstMean", seed = 1)

## ----mosum, eval = has_mosum--------------------------------------------------
mosum_wrapper(x)
mosum_wrapper(x3, multiscale = TRUE)

## ----idetect, eval = has_idetect----------------------------------------------
idetect_wrapper(x, seed = 1)

## ----tguh, eval = has_breakfast-----------------------------------------------
tguh_wrapper(x)

## ----smuce, eval = has_stepR--------------------------------------------------
# res_smuce <- smuce_wrapper(x, alpha = 0.5)
# tidy(res_smuce)

## ----cpop, eval = has_cpop----------------------------------------------------
# y_slope <- cumsum(c(rep(0.4, 100), rep(-0.3, 100))) + rnorm(200)
# res_cpop <- cpop_wrapper(y_slope)
# tidy(res_cpop)

## ----cpop-fallback, include = FALSE, eval = !has_cpop-------------------------
# y_slope is reused by the segmented example below
y_slope <- cumsum(c(rep(0.4, 100), rep(-0.3, 100))) + rnorm(200)

## ----bcp, eval = has_bcp------------------------------------------------------
# res_bcp <- bcp_wrapper(x, seed = 1)
# tidy(res_bcp)

## ----bocpd, eval = has_ocp----------------------------------------------------
# res_bocpd <- bocpd_wrapper(x)
# tidy(res_bocpd)

## ----beast, eval = has_rbeast-------------------------------------------------
# res_beast <- beast_wrapper(x, seed = 1)
# tidy(res_beast)

## ----cpm, eval = has_cpm------------------------------------------------------
# tidy(cpm_wrapper(x, cpm_type = "Mann-Whitney"))

## ----kcp, eval = has_kcprs----------------------------------------------------
# tidy(kcp_wrapper(x, running_stat = "mean", nperm = 100, seed = 1))

## ----npmojo, eval = has_cptnonpar---------------------------------------------
# tidy(npmojo_wrapper(x))

## ----decafs, eval = has_decafs------------------------------------------------
# tidy(decafs_wrapper(x))

## ----sn, eval = has_snseg-----------------------------------------------------
# tidy(sn_wrapper(x3, parameter = "mean"))

## ----envcpt, eval = has_envcpt------------------------------------------------
# res_env <- envcpt_wrapper(x, models = c("mean", "meancpt", "trendcpt"))
# tidy(res_env)
# res_env$penalty$type   # criterion and winning model

## ----hd-data------------------------------------------------------------------
set.seed(1)
X <- cbind(a = c(rnorm(80), rnorm(80, 4)),
           b = c(rnorm(80), rnorm(80, -3)),
           c = rnorm(160))

## ----inspect, eval = has_inspect----------------------------------------------
# res_hd <- inspect_wrapper(X)
# tidy(res_hd)

## ----ocd, eval = FALSE--------------------------------------------------------
# # Online detection: the reported locations are declaration times (the change
# # plus the detection delay), in a `declared_at` column. Monte Carlo threshold
# # calibration makes this the slowest wrapper, so it is shown but not run
# # here. It needs at least two coordinates and rejects a univariate series.
# res_ocd <- ocd_wrapper(X, mc_reps = 100)
# tidy(res_ocd)

## ----geomcp, eval = has_geomcp------------------------------------------------
# tidy(geomcp_wrapper(X))

## ----ecp-mv-------------------------------------------------------------------
tidy(cpt_detect(X, method = "ecp", seed = 1))

## ----strucchange, eval = has_struc--------------------------------------------
# tidy(strucchange_wrapper(x))

## ----segmented, eval = has_segmented------------------------------------------
# tidy(segmented_wrapper(y_slope, npsi = 1, seed = 1))

## ----fastcpd, eval = has_fastcpd----------------------------------------------
# tidy(fastcpd_wrapper(x, family = "mean"))

## ----crops--------------------------------------------------------------------
path <- cpt_crops(x3)
path
tidy(path)

## ----crops-elbow--------------------------------------------------------------
autoplot(path)                          # the cost elbow

## ----crops-path---------------------------------------------------------------
autoplot(path, type = "path")           # changepoints vs penalty

## ----crops-seg----------------------------------------------------------------
autoplot(path, type = "segmentations")  # the candidate models themselves

## ----autoplot-----------------------------------------------------------------
autoplot(res, show_segments = TRUE, cptline_color = "firebrick",
         cptline_type = "dashed", cptline_linewidth = 0.8)

## ----autoplot-ci, eval = has_stepR--------------------------------------------
# autoplot(res_smuce, show_ci = TRUE, show_fit = TRUE)

## ----autoplot-mv, eval = has_inspect, fig.height = 6--------------------------
# autoplot(res_hd)

## ----geoms--------------------------------------------------------------------
cp_tbl <- tidy(res)
df <- data.frame(index = seq_along(x), value = x)

ggplot(df, aes(index, value)) +
  annotate_segments(cp = cp_tbl$cp, n = length(x)) +
  geom_line() +
  geom_changepoint(data = cp_tbl, aes(xintercept = cp), color = "red") +
  geom_cpt_segment(
    data = res$segments,
    aes(x = start, xend = end, y = param_estimate, yend = param_estimate),
    inherit.aes = FALSE, color = "darkred", linewidth = 1
  ) +
  theme_ggcpt()

## ----stat---------------------------------------------------------------------
ggplot(df, aes(index, value)) +
  geom_line() +
  stat_changepoint(method = "pelt", color = "blue")

## ----geom-ci, eval = has_stepR------------------------------------------------
# ci_tbl <- tidy(res_smuce)
# ci_tbl$y_pos <- min(x) - 1
# ggplot(df, aes(index, value)) +
#   geom_line(color = "grey60") +
#   geom_cpt_ci(
#     data = ci_tbl,
#     aes(y = y_pos, xmin = ci_lower, xmax = ci_upper),
#     width = 0.6, color = "blue", inherit.aes = FALSE
#   ) +
#   geom_changepoint(data = ci_tbl, aes(xintercept = cp), color = "blue")

## ----original-plots-----------------------------------------------------------
ggcptplot(x, change_in = "mean", cp_method = "PELT")

## ----ggecpplot----------------------------------------------------------------
ggecpplot(x, algorithm = "divisive", seed = 1)

## ----posterior, eval = has_bcp, fig.height = 6--------------------------------
# ggcpt_posterior(res_bcp)

## ----runlength, eval = has_ocp------------------------------------------------
# ggcpt_runlength(res_bocpd)

## ----interactive, eval = has_plotly-------------------------------------------
class(ggcpt_interactive(res))   # requires plotly

## ----compare, fig.height = 6--------------------------------------------------
ggcpt_compare(x, methods = c("pelt", "binseg", "amoc"))

## ----compare-overlay----------------------------------------------------------
ggcpt_compare(x, methods = c("pelt", "binseg"), layout = "overlay")

## ----compare-table------------------------------------------------------------
ggcpt_compare_table(x, methods = c("pelt", "binseg", "amoc"))

## ----batch--------------------------------------------------------------------
XB <- cbind(shifted = x, pure_noise = rnorm(200))
batch <- cpt_batch(XB, method = "pelt")
batch
tidy(batch)
autoplot(batch)

## ----stability----------------------------------------------------------------
st <- cpt_stability(x, method = "pelt", B = 30, seed = 1)
st
autoplot(st)

## ----metrics------------------------------------------------------------------
truth <- 100
pred <- tidy(res)$cp
cpt_metrics(pred, truth, n = length(x), margin = 5)
cpt_metrics_annotated(pred, list(100, 101, 99), n = length(x))

## ----eval-plot----------------------------------------------------------------
ggcpt_eval(pred, truth, x, margin = 5)

## ----simulate-----------------------------------------------------------------
sim <- cpt_simulate(300, changepoints = c(100, 200), change_in = "mean",
                    params = c(0, 5, 1), seed = 1)
attr(sim, "true_changepoints")
sim2 <- rcpt(300, changepoints = 150, params = c(0, 3), seed = 2)  # the alias
attr(sim2, "true_changepoints")

signals <- list(blocks = signal_blocks(1024, seed = 1),
                fms    = signal_fms(500, seed = 1),
                mix    = signal_mix(500, seed = 1),
                teeth  = signal_teeth(400, seed = 1),
                stairs = signal_stairs(500, seed = 1))
vapply(signals, function(s) length(attr(s, "true_changepoints")), integer(1))

## ----blocks-plot--------------------------------------------------------------
blocks <- signals$blocks
ggplot(blocks, aes(index, value)) +
  geom_line(color = "grey50") +
  geom_vline(xintercept = attr(blocks, "true_changepoints"),
             color = "blue", linewidth = 0.3) +
  labs(title = "The blocks test signal with its true changepoints")

## ----cite---------------------------------------------------------------------
cpt_cite("pelt")
cpt_cite(res)

