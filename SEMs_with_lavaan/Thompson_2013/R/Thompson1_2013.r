## Thompson, M. & Green, S. (2013). Evaluating between-group differences
## in latent variable means. In G. Hancock & R. Mueller (Eds.), *Structural
## equation modeling: A second course* (2nd ed., pp. 163-218). Charlotte, NC:
## Information Age Publishing.

## Load package
library(lavaan)

## Get data from Table 5.1
# Group 1 - Day-care
cov1 <- c(
 138.00,
  45.58,  80.49,
  35.19,  23.56,  56.34,
  45.13,  32.00,  16.64, 232.17,
  35.33,  10.49,  10.56,  79.74, 149.16,
  73.34,  28.73,  33.21, 117.30,  79.90, 324.36)

means1 <- c(50.40, 79.60, 98.88, 74.06, 49.12, 120.10)
n1 <- 200

# Group 2 - Home-care
cov2 <- c(
 127.61,
  58.49,  76.81,
  29.09,  19.72,  54.29,
  45.84,  28.94,  31.82, 223.09,
  20.33,  13.27,   5.81,  62.25, 135.72,
  50.64,  55.45,  30.15, 126.74,  62.16, 337.37)

means2 <- c(53.70, 81.32, 101.82, 77.69, 51.62, 123.37)
n2 <- 200

## Get the variable names from Table 5.1
names <- c("V1", "V2", "V3", "V4", "V5", "V6")

## Combine into lists
cov <- list("Day-care" = cov1, "Home-care" = cov2)
means <- list(means1, means2)
n <- list(n1, n2)

## Get the co/variance matrices
cov <- lapply(cov, lav_getcov, names = names)


## The single-group model
model <- "
   # Measurement Model
   Academic =~ 1*V1 + V2 + V3
   Social =~ NA*V4 + V5 + 1*V6
"

## Fit the model and get the summary
#  Compare with Fig 5.1 (and fit measures on page 173)
m1_g1_fit <- sem(model, sample.cov = cov[[1]], sample.nobs = n[[1]],
   sample.mean = means[[1]])
summary(m1_g1_fit, fit.measures = TRUE, remove.unused = FALSE)


## The two-group model
model <- "
   # Measurement Model
   Academic =~ 1*V1 + V2 + V3
   Social =~ NA*V4 + V5 + 1*V6
"

## Fit the model and get the summary
#  Compare with Fig 5.2
m2_strong_fit <- sem(model, sample.cov = cov, sample.nobs = n,
   sample.mean = means, group.equal = c("loadings", "intercepts"))
summary(m2_strong_fit, fit.measures = TRUE, remove.unused = FALSE)


## A function to extract fit measure
GetFit <- function(fit, ...) {
   tab = fitMeasures(fit, ...)
   tab = round(tab, 3)
   return(tab)
}

## Select measures
measures = c("chisq", "df", "pvalue", "cfi", "srmr",
   "rmsea", "rmsea.ci.lower", "rmsea.ci.upper")

## Fit the model to Group 2
m1_g2_fit <- sem(model, sample.cov = cov[[2]], sample.nobs = n[[2]],
   sample.mean = means[[2]])

# Get fit measures for group 1 and group 2 models
# Compare with fit measures on page 176
GetFit(m1_g1_fit, measures)
GetFit(m1_g2_fit, measures)


## Comparing model assuming configural invariance with model assuming strong invariance
## Fit model assuming configural invariance
m2_config_fit <- sem(model, sample.cov = cov, sample.nobs = n, sample.mean = means)

## Get fit measures for the two models
#  Compare with fit measures on page 176
GetFit(m2_config_fit, measures)
GetFit(m2_strong_fit, measures)

## Compare the two models
#  Compare with chi-square difference test on page 177
anova(m2_config_fit, m2_strong_fit)


## Models constraining means to equality:
#    The Academic construct
Academic_equal <- "
   # Measurement Model
   Academic =~ 1*V1 + V2 + V3
   Social   =~ NA*V4 + V5 + 1*V6

   # Means
   Academic ~ c(0,0)*1
   Social   ~ c(0,NA)*1
"

#    The Social construct
Social_equal <- "
   # Measurement Model
   Academic =~ 1*V1 + V2 + V3
   Social   =~ NA*V4 + V5 + 1*V6

   # Means
   Academic ~ c(0,NA)*1
   Social   ~ c(0,0)*1
"

## Fit the models
Academic_equal_fit <- sem(Academic_equal, sample.cov = cov, sample.nobs = n,
     sample.mean = means, group.equal = c("loadings", "intercepts"))

Social_equal_fit <- sem(Social_equal, sample.cov = cov, sample.nobs = n,
     sample.mean = means, group.equal = c("loadings", "intercepts"))

## Get the fit measures
#  Compare with fit measures at bottom of page 177
measures = c("chisq", "df", "pvalue")
GetFit(Academic_equal_fit, measures)
GetFit(Social_equal_fit, measures)

## Compare fit for each model with m2_strong_fit
#  Compare with difference tests at bottom of page 177
anova(Academic_equal_fit, m2_strong_fit)
anova(Social_equal_fit, m2_strong_fit)


## Get effect sizes (Day-care group as reference-group)
#  Compare with effect sizes of page 178
summary(m2_strong_fit, remove.unused = FALSE)
ES_Acad <- (3.566 - 0) / sqrt(73.69); ES_Acad
ES_Soc <- (3.885 - 0) / sqrt(141.75); ES_Soc

## Get effect sizes (pooled variance)
summary(m2_strong_fit)
ES_Acad <- (3.566 - 0) / sqrt((80.52 + 73.69)/2); ES_Acad
ES_Soc <- (3.885 - 0) / sqrt((133.43 + 141.75)/2); ES_Soc


## Extract means and variances from fitted lavaan objects
#  Get estimates in a list
estimates <- lavInspect(m2_strong_fit, "est"); estimates


## Get effect sizes (pooled variance)
#  Get difference in means
means <- abs(Reduce('-', lapply(estimates, '[[', 'alpha'))); means

#  Get average of variances
var <- lapply(lapply(estimates, '[[', 'psi'), diag); var
pool <- Reduce(function(x,y) (x + y)/2, var); pool

#  Get effect sizes
ES <- means/sqrt(pool)
colnames(ES) <- "ES"
ES


## Get effect sizes (reference group variance)
#  Get difference in means
means <- abs(Reduce('-', lapply(estimates, '[[', 'alpha'))); means

# Get the variances
var <- lapply(lapply(estimates, '[[', 'psi'), diag); var

# Effect size (1st group is reference group)
ES <- means / sqrt(var[[1]])
colnames(ES) <- "ES"
ES




