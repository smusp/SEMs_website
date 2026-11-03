

## Berry, D., & Willoughby, M. (2016). On the practical interpretability 
## of cross-lagged panel models: Rethinking a developmental workhorse.
## Child Development, 88(4), 1186-1206.

## Load package
library(lavaan)

## Get the data
cor <- c(
   1.00,
   0.58, 1.00,
   0.61, 0.68, 1.00,
   0.39, 0.45, 0.51, 1.00,
   0.30, 0.16, 0.23, 0.09, 1.00,
   0.30, 0.20, 0.28, 0.12, 0.56, 1.00,
   0.28, 0.24, 0.28, 0.10, 0.56, 0.55, 1.00,
   0.15, 0.18, 0.17, 0.08, 0.32, 0.37, 0.36, 1.00)

sds <- c(0.62, 0.61, 0.59, 0.55, 0.18, 0.17, 0.18, 0.23)
means <- c(1.73, 1.66, 1.61, 1.36, 0.28, 0.28, 0.28, 0.33)
n <- 290

## Get the variable names
names <- c(paste0("PD", c(10, 11, 12, 15)), paste0("AB", c(10, 11, 12, 15)))

## Get the co/variance matrix
cov <- lavaan::lav_getcov(cor, sds = sds, names = names)
