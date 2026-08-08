## Thompson, M. & Green, S. (2013). Evaluating between-group differences
## in latent variable means. In G. Hancock & R. Mueller (Eds.), *Structural
## equation modeling: A second course* (2nd ed., pp. 163-218). Charlotte, NC:
## Information Age Publishing.

## Load package
library(lavaan)

## Get data from Table 5.2
# Group 1 - Day-care
cov1 <- c(
 154.54,
  44.75,  90.23,
  40.98,  22.77,  78.76,	
  41.35,   2.83,   7.92, 220.12,
  23.28,   9.12,   0.75,  61.46, 159.88,
  47.08,  22.88,  16.05, 125.08,  84.31, 332.26
)

means1 <- c(49.14, 82.60, 104.95, 78.58, 54.95, 119.91)
n1 <- 250

# Group 2 - Home-care
cov2 <- c(
 124.93,
  52.19,  80.67,
  64.45,  42.85,  83.56,
  59.95,  33.10,  38.34, 290.65,
  32.32,  16.09,  18.29, 124.71, 169.17,
  87.15,  39.70,  51.82, 174.20, 108.39, 355.22
)

means2 <- c(55.01, 81.22, 97.01, 72.67, 46.57, 124.28)
n2 <- 150

# Get the variable names
names <- c("V1", "V2", "V3", "V4", "V5", "V6")

## Combine into lists
cov <- list("Day-care" = cov1, "Home-care" = cov2)
means <- list(means1, means2)
n <- list(n1, n2)

## Get the co/variance matrices
cov <- lapply(cov, lav_getcov, names = names)


## Step 1 - equivalence of forms - configural invariance
# Model statement
m1 <- "
   # Measurement Model
   Academic =~ V1 + V2 + V3
   Social =~ V4 + V5 + V6
"

# Fit the model to each group separately
m1_g1_fit <- sem(m1, sample.cov = cov[[1]], sample.nobs = n[[1]],
   sample.mean = means[[1]], std.lv = TRUE)
summary(m1_g1_fit, remove.unused = FALSE)

m1_g2_fit <- sem(m1, sample.cov = cov[[2]], sample.nobs = n[[2]],
   sample.mean = means[[2]], std.lv = TRUE)
summary(m1_g2_fit, remove.unused = FALSE)

# A function to extract fit measure from output
GetFit <- function(fit, ...) {
    fitMeasures(fit, ...)}
measures = c("chisq", "df", "pvalue", "cfi", "srmr", "rmsea")

# Get fit measures for each group
# Compare with Step 1 in Table 5.4
GetFit(m1_g1_fit, measures)
GetFit(m1_g2_fit, measures)

# Fit the two-group model
# Compare with Step 1 in Table 5.4
config_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE)
summary(config_fit, remove.unused = FALSE)
GetFit(config_fit, measures)


## Step 2 - equivalence of loadings - metric (or weak) invariance

# Fit the model
# Compare with Step 2 in Table 5.4
metric_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings"))
summary(metric_fit, remove.unused = FALSE)
GetFit(metric_fit, measures)

# Compare the fits of two models
anova(config_fit, metric_fit)

# Function to get difference in fit statistics
FitDiff = function(fit1, fit2) {
     call <- as.list(match.call())
     tab = cbind(data.frame(GetFit(fit1, measures), GetFit(fit2, measures)))[4:6, ]
     names(tab) = c(call$fit1, call$fit2)
     tab$diff = tab[, 2] - tab[, 1]
     return(round(tab, 3))
     }
FitDiff(config_fit, metric_fit)

# lavTestScore() function - test for releasing equality constraints
lavTestScore(metric_fit)

# Get parameter table
tab <- parTable(metric_fit)
# Print the part with loadings and constraints
tab[tab$op %in% c("=~","=="), ]
# Allow loadings for V3 to vary across groups


## Step 2a - relax equality constraint for V3 loading on Academic

# Fit the model
# Compare with Step 2a in Table 5.4
metric_m2a_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings"), group.partial = c("Academic =~ V3"))
summary(metric_m2a_fit, remove.unused = FALSE)
GetFit(metric_m2a_fit, measures)

anova(metric_fit, metric_m2a_fit)
FitDiff(metric_fit, metric_m2a_fit)

lavTestScore(metric_m2a_fit)
tab <- parTable(metric_m2a_fit)
tab[tab$op %in% c("=~","=="), ]
# Allow loadings for V6 to vary across groups


## Step 2b - relax equality constraint for V6 loading on Social

# Fit the model
# Compare with Step 2b in Table 5.4
metric_m2b_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings"), group.partial = c("Academic =~ V3", "Social =~ V6"))
summary(metric_m2b_fit, remove.unused = FALSE)

GetFit(metric_m2b_fit, measures)
anova(metric_m2a_fit, metric_m2b_fit)
FitDiff(metric_m2a_fit, metric_m2b_fit)

lavTestScore(metric_m2b_fit)
tab <- parTable(metric_m2a_fit)
tab[tab$op %in% c("=~","=="), ]
# Allow loadings for V1 to vary across groups


## Step 2c - relax equality constraint for V1 loading on Social

# Fit the model
# Compare with Step 2c in Table 5.4
metric_m2c_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings"), group.partial = c("Academic =~ V3", "Social =~ V6", "Academic =~ V1"))
summary(metric_m2c_fit, remove.unused = FALSE)

GetFit(metric_m2c_fit, measures)
anova(metric_m2b_fit, metric_m2c_fit)
FitDiff(metric_m2b_fit, metric_m2c_fit)


## Step 3 -  equivalence of loadings & intercepts - scalar (strong) invariance

# Fit the model
# Compare with Step 3 in Table 5.4
scalar_m3_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings", "intercepts"),
   group.partial = c("Academic =~ V3", "Social =~ V6", "V3 ~ 1", "V6 ~ 1"))
summary(scalar_m3_fit, remove.unused = FALSE)
GetFit(scalar_m3_fit, measures)

anova(metric_m2b_fit, scalar_m3_fit)
FitDiff(metric_m2b_fit, scalar_m3_fit)

lavTestScore(scalar_m3_fit)
tab <- parTable(scalar_m3_fit)
tab[tab$op %in% c("=~","~1","=="), ]


## Step 3a - relax equality constraint for V1 intercept

# Fit the model
# Compare with Step 3a in Table 5.4
scalar_m3a_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings", "intercepts"),
   group.partial = c("Academic =~ V3", "Social =~ V6", "V3 ~ 1", "V6 ~ 1", "V1 ~ 1"))
summary(scalar_m3a_fit, remove.unused = FALSE)
GetFit(scalar_m3a_fit, measures)

anova(scalar_m3a_fit, scalar_m3_fit)
FitDiff(scalar_m3_fit, scalar_m3a_fit)

lavTestScore(scalar_m3a_fit)
tab <- parTable(scalar_m3a_fit)
tab[tab$op %in% c("=~","~1","=="), ]


## Step 3b - relax equality constraint for V1 intercept

# Fit the model
# Compare with Step 3b in Table 5.4
scalar_m3b_fit <- sem(m1, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings", "intercepts"),
   group.partial = c("Academic =~ V3", "Social =~ V6", "V3 ~ 1", "V6 ~ 1", "V1 ~ 1", "V4 ~ 1"))
summary(scalar_m3b_fit, remove.unused = FALSE)
GetFit(scalar_m3b_fit, measures)

anova(scalar_m3a_fit, scalar_m3b_fit)
FitDiff(scalar_m3a_fit, scalar_m3b_fit)


## Step 4a - constrain Academic mean to zero in Group 2
#  ie, Academic means constrained to equality

# Model statement
m4a <- "
  # Measurement Model
  Academic =~ V1 + V2 + V3
  Social =~ V4 + V5 + V6

  Academic ~ c(0,0)*1
  Social ~ c(0,NA)*1
"

scalar_m4a_fit <- sem(m4a, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings", "intercepts"),
   group.partial = c("Academic =~ V3", "Social =~ V6", "V3 ~ 1", "V6 ~ 1", "V1 ~ 1", "V4 ~ 1"))
summary(scalar_m4a_fit, remove.unused = FALSE)

GetFit(scalar_m4a_fit, measures)
anova(scalar_m3b_fit, scalar_m4a_fit)
FitDiff(scalar_m3b_fit, scalar_m4a_fit)


## Step 4b - constrain Social mean to zero in Group 2
#  ie, Social means constrained to equality

# Model statement
m4b <- "
  # Measurement Model
  Academic =~ V1 + V2 + V3
  Social =~ V4 + V5 + V6

  Academic ~ c(0,NA)*1
  Social ~ c(0,0)*1
"

scalar_m4b_fit <- sem(m4b, sample.cov = cov, sample.nobs = n,
   sample.mean = means, std.lv = TRUE,
   group.equal = c("loadings", "intercepts"),
   group.partial = c("Academic =~ V3", "Social =~ V6", "V3 ~ 1", "V6 ~ 1", "V1 ~ 1", "V4 ~ 1"))
summary(scalar_m4b_fit, remove.unused = FALSE)

GetFit(scalar_m4b_fit, measures)
anova(scalar_m3b_fit, scalar_m4b_fit)
FitDiff(scalar_m3b_fit, scalar_m4b_fit)


## Get effect sizes (Day-care group as reference-group)
# Get means and variances from summary for scalar_m3b_fit
# Compare with effect sizes on p. 195
summary(scalar_m3b_fit)

ES_Acad <- (-0.266 - 0) / sqrt(1); ES_Acad
ES_Soc <- (-1.348 - 0) / sqrt(1); ES_Soc

## Get effect sizes (pooled variance)
summary(scalar_m3b_fit)

ES_Acad <- (-0.266 - 0) / sqrt(((n1-1)*1 + (n2-1)*1.092)/(n1+n2-2)); ES_Acad
ES_Soc <- (-1.348 - 0) / sqrt(((n1-1)*1 + (n2-1)*1.986)/(n1+n2-2)); ES_Soc

## Extract means and variances from fitted lavaan object
#  Get estimates in a list
estimates <- lavInspect(scalar_m3b_fit, "est"); estimates

## Get effect sizes (pooled variances)
#  Get difference in means
means <- abs(Reduce('-', lapply(estimates, '[[', 'alpha'))); means

#  Get pooled variance
var <- lapply(lapply(estimates, '[[', 'psi'), diag); var
pool <- Reduce(function(x,y) ((n[[1]] - 1)*x + (n[[2]] - 1)*y) / (n[[1]] + n[[2]] - 2), var); pool

#  Get effect sizes
ES <- means/sqrt(pool)
colnames(ES) <- "ES"
ES




