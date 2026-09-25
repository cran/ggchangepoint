## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5,
  dpi = 72,
  message = FALSE,
  warning = FALSE,
  fig.alt = "ggchangepoint monitoring plot"
)
library(ggchangepoint)
library(ggplot2)
theme_set(theme_ggcpt())

has_cpm <- requireNamespace("cpm", quietly = TRUE)
has_ocd <- requireNamespace("ocd", quietly = TRUE)

## ----build--------------------------------------------------------------------
set.seed(2026)
baseline <- rnorm(200)
mon <- cpt_monitor("edetector", baseline = baseline, alpha = 0.002)
mon

## ----in-control---------------------------------------------------------------
set.seed(5201)
mon <- cpt_update(mon, rnorm(120))
mon

## ----change-------------------------------------------------------------------
set.seed(5202)
mon <- cpt_update(mon, rnorm(80, mean = 2))
alarms(mon)

## ----delay--------------------------------------------------------------------
d <- cpt_delay(mon, truth = 121)
d

## ----delay-tidy---------------------------------------------------------------
glance(d)

## ----timeline, fig.alt = "Monitored series with dashed vertical lines at each alarm"----
autoplot(mon)

## ----delay-plot, fig.height = 3.5, fig.alt = "Bar chart of the detection delay at the true changepoint"----
autoplot(d)

## ----replay-------------------------------------------------------------------
set.seed(5203)
stream <- c(rnorm(200), rnorm(200, mean = 2))
rep_e <- cpt_replay(stream, method = "edetector")
alarms(rep_e)

## ----replay-delay-------------------------------------------------------------
glance(cpt_delay(rep_e, truth = 200))

## ----deltas-------------------------------------------------------------------
set.seed(2026)
delay_at <- function(deltas, shift, reps = 8, n = 200) {
  v <- vapply(seq_len(reps), function(i) {
    s <- c(rnorm(n), rnorm(n, mean = shift))
    cpt_delay(cpt_replay(s, method = "edetector", deltas = deltas),
              truth = n)$median_delay
  }, numeric(1))
  median(v, na.rm = TRUE)
}
data.frame(
  shift = c(0.75, 3),
  mixture_0.5_1_2 = c(delay_at(c(0.5, 1, 2), 0.75), delay_at(c(0.5, 1, 2), 3)),
  only_0.5 = c(delay_at(0.5, 0.75), delay_at(0.5, 3)),
  only_3 = c(delay_at(3, 0.75), delay_at(3, 3))
)

## ----cpm, eval = has_cpm------------------------------------------------------
set.seed(5204)
mon_cpm <- cpt_monitor("cpm", arl0 = 500)
mon_cpm <- cpt_update(mon_cpm, rnorm(120))
mon_cpm <- cpt_update(mon_cpm, rnorm(80, mean = 2))
alarms(mon_cpm)
glance(cpt_delay(mon_cpm, truth = 121))

## ----alpha, warning = TRUE----------------------------------------------------
set.seed(5205)
ic <- rnorm(400)
vapply(c(0.05, 0.01, 0.001),
       function(a) nrow(alarms(cpt_replay(ic, method = "edetector",
                                          alpha = a))),
       numeric(1))
nrow(alarms(cpt_replay(ic, method = "edetector", arl0 = 5000)))

## ----ocd-error, error = TRUE--------------------------------------------------
try({
set.seed(5206)
cpt_monitor("ocd", baseline = rnorm(100))
})

## ----ocd, eval = has_ocd------------------------------------------------------
set.seed(11)
base_mv <- matrix(rnorm(200 * 3), ncol = 3)
mon_ocd <- cpt_monitor("ocd", baseline = base_mv, patience = 200,
                       mc_reps = 30)
stream_mv <- rbind(matrix(rnorm(60 * 3), ncol = 3),
                   matrix(rnorm(60 * 3, mean = 1.2), ncol = 3))
mon_ocd <- cpt_update(mon_ocd, stream_mv)
glance(cpt_delay(mon_ocd, truth = 61))

## ----delay-study--------------------------------------------------------------
set.seed(2026)
median_delay <- function(shift, method, reps = 10, n = 200) {
  d <- vapply(seq_len(reps), function(i) {
    s <- c(rnorm(n), rnorm(n, mean = shift))
    cpt_delay(cpt_replay(s, method = method), truth = n)$median_delay
  }, numeric(1))
  median(d, na.rm = TRUE)
}
grid <- expand.grid(shift = c(1, 2, 3),
                    method = if (has_cpm) c("edetector", "cpm") else "edetector",
                    stringsAsFactors = FALSE)
grid$median_delay <- mapply(median_delay, grid$shift, grid$method)
grid

## ----assumptions--------------------------------------------------------------
set.seed(3)
false_alarms <- function(method, gen, reps = 5, n = 400) {
  mean(vapply(seq_len(reps),
              function(i) nrow(alarms(cpt_replay(gen(n), method = method))),
              numeric(1)))
}
iid <- function(n) rnorm(n)
ar1 <- function(n) as.numeric(stats::arima.sim(list(ar = 0.7), n))
methods <- if (has_cpm) c("edetector", "cpm") else "edetector"
data.frame(
  method = methods,
  iid = vapply(methods, false_alarms, numeric(1), gen = iid),
  ar1_rho_0.7 = vapply(methods, false_alarms, numeric(1), gen = ar1),
  row.names = NULL
)

## ----relearn------------------------------------------------------------------
nrow(alarms(cpt_replay(stream, method = "edetector", relearn = 20)))
nrow(alarms(cpt_replay(stream, method = "edetector", relearn = 0)))

## ----report-------------------------------------------------------------------
glance(cpt_delay(rep_e, truth = 200))

