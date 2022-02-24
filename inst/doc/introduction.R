## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  fig.width = 8, 
  fig.height = 5,
  message = FALSE,
  warning = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggchangepoint)
library(ggplot2)
theme_set(theme_light())

## -----------------------------------------------------------------------------
set.seed(10)
m.data <- c(rnorm(100, 0, 1), 
            rnorm(100, 1, 1), 
            rnorm(100, 0, 1),
            rnorm(100, 0.2, 1))

## -----------------------------------------------------------------------------
m.pelt <- cpt_wrapper(m.data, change_in = "mean", cp_method = "PELT")

m.pelt 

## -----------------------------------------------------------------------------
ggcptplot(m.data, change_in = "mean", cp_method = "PELT") +
  labs(title = "Changes in mean (PELT)")

## -----------------------------------------------------------------------------
m.binseg <- cpt_wrapper(m.data, change_in = "mean", cp_method = "BinSeg")

m.binseg

## -----------------------------------------------------------------------------
ggcptplot(m.data, change_in = "mean", cp_method = "BinSeg") +
  labs(title = "Changes in mean (BinSeg)")

## -----------------------------------------------------------------------------
m.pm <- cpt_wrapper(m.data, 
                    change_in = "mean", 
                    penalty = "Manual", 
                    pen.value = "1.5 * log(n)")

m.pm

## -----------------------------------------------------------------------------
ggcptplot(m.data, 
          change_in = "mean", 
          penalty = "Manual", 
          pen.value = "1.5 * log(n)",
          cptline_color = "red",
          cptline_size = 2) +
  labs(title = "Changes in mean (PELT with Manual penalty)")

## -----------------------------------------------------------------------------
m.bsm <- cpt_wrapper(m.data, 
                     change_in = "mean", 
                     cp_method = "BinSeg",
                     penalty = "Manual", 
                     pen.value = "1.5 * log(n)")

m.bsm

## -----------------------------------------------------------------------------
ggcptplot(m.data, 
          change_in = "mean", 
          cp_method = "BinSeg",
          penalty = "Manual", 
          pen.value = "1.5 * log(n)",
          cptline_size = 2) +
  labs(title = "Changes in mean (BinSeg with Manual penalty)")

## -----------------------------------------------------------------------------
data("Lai2005fig4", package = "changepoint")

cpt_wrapper(Lai2005fig4$GBM29, change_in = "mean")

## -----------------------------------------------------------------------------
ggcptplot(Lai2005fig4$GBM29, change_in = "mean") +
  labs(title = "GBM29 Changes in mean (PELT)")

## -----------------------------------------------------------------------------
set.seed(2022)
ecp_wrapper(Lai2005fig4$GBM29)

## -----------------------------------------------------------------------------
set.seed(2022)
ggecpplot(Lai2005fig4$GBM29) +
  labs(title = "GBM29 Changes using the ecp package")

## -----------------------------------------------------------------------------
data("wind", package = "gstat")

## -----------------------------------------------------------------------------
wind.bs <- cpt_wrapper(diff(wind[, 11]), change_in = "var", cp_method = "BinSeg")

wind.bs

## -----------------------------------------------------------------------------
ggcptplot(diff(wind[, 11]), change_in = "var", cp_method = "BinSeg")

## -----------------------------------------------------------------------------
data("discoveries", package = "datasets")

## -----------------------------------------------------------------------------
dis.pelt <- cpt_wrapper(discoveries, test.stat = "Poisson")

dis.pelt

## -----------------------------------------------------------------------------
ggcptplot(discoveries, test.stat = "Poisson",
          cptline_color = "red",
          cptline_size = 2) +
  ggtitle("The Discoveries dataset with identified changepoints (PELT)")

## -----------------------------------------------------------------------------
ggcptplot(discoveries, test.stat = "Poisson",
          cp_method = "BinSeg",
          cptline_color = "red",
          cptline_size = 2) +
  ggtitle("The Discoveries dataset with identified changepoints (BinSeg)")

