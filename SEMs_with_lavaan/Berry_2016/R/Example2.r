

## Berry, D., & Willoughby, M. (2016). On the practical interpretability 
## of cross-lagged panel models: Rethinking a developmental workhorse.
## Child Development, 88(4), 1186-1206.

## lavaan code for Example 2 from Berry & Willoughby (BW).
## Using data from Family Life Project


## Load packages
library(lavaan)


## Get the data from BR's Online Supplemental Material: Appendix S3 (p. 10).
cor <- c(
   1.000, 
   0.646, 1.000, 
   0.571, 0.642, 1.000, 
   0.483, 0.531, 0.572, 1.000, 
   0.203, 0.189, 0.187, 0.208, 1.000, 
   0.139, 0.234, 0.204, 0.219, 0.553, 1.000, 
   0.141, 0.167, 0.227, 0.241, 0.518, 0.570, 1.000, 
   0.141, 0.177, 0.226, 0.318, 0.498, 0.555, 0.570, 1.000)

means <- c(1.233, 1.248, 1.045, 0.635, 0.622, 0.429, 0.377, 0.369)
sds <- c(1.366, 1.348, 1.308, 1.031, 0.416, 0.341, 0.352, 0.385)
n <- 1018


## Get the variable names
(names <- c(paste0("PD", c(3, 4, 5, 7)), paste0("AB", c(3, 4, 5, 7))))


## Get the co/variance matrix
cov <- lavaan::lav_getcov(cor, sds = sds, names = names)


## ARCL
ARCL <- "
# Latent intercepts & slopes are estimated
# but their variances and thus their covariances are not.
# Intercepts for observed variables are not estimated, 
# but means for latent intercepts and slopes are estimated.
  # Intercepts
  i.AB =~ 1*AB3 + 1*AB4 + 1*AB5 + 1*AB7
  i.PD =~ 1*PD3 + 1*PD4 + 1*PD5 + 1*PD7

  # Slopes
  s.AB =~ 0*AB3 + 1*AB4 + 2*AB5 + 3*AB7
  s.PD =~ 0*PD3 + 1*PD4 + 2*PD5 + 3*PD7

  # Intercept and slope means
  i.AB + i.PD + s.AB + s.PD ~ 1

# Estimate latent structured residuals
  # Structured residuals
  r.AB3 =~ 1*AB3
  r.AB4 =~ 1*AB4
  r.AB5 =~ 1*AB5
  r.AB7 =~ 1*AB7

  r.PD3 =~ 1*PD3
  r.PD4 =~ 1*PD4
  r.PD5 =~ 1*PD5
  r.PD7 =~ 1*PD7

# Regressions - note pattern of equality constraints
  # Stability (autoregressive) effects
  r.PD7 ~ b1*r.PD5
  r.PD5 ~ b1*r.PD4
  r.PD4 ~ b1*r.PD3

  r.AB7 ~ b2*r.AB5
  r.AB5 ~ b2*r.AB4
  r.AB4 ~ b2*r.AB3

  # Cross-lagged effects
  r.AB7 ~ b3*r.PD5
  r.AB5 ~ b3*r.PD4
  r.AB4 ~ b3*r.PD3

  r.PD7 ~ b4*r.AB5
  r.PD5 ~ b4*r.AB4
  r.PD4 ~ b4*r.AB3

# Variances of observed intercepts are not estimated 
# but variances of latent structured residuals are estimaed.
# Variances at age 3 is freely estimated,
# but variances at ages 4, 5, and 7 are constrained to equality.
  # Variances of structured residuals
  r.AB3 ~~ r.AB3
  r.AB4 ~~ s1*r.AB4
  r.AB5 ~~ s1*r.AB5
  r.AB7 ~~ s1*r.AB7

  r.PD3 ~~ r.PD3
  r.PD4 ~~ s2*r.PD4
  r.PD5 ~~ s2*r.PD5
  r.PD7 ~~ s2*r.PD7

# Covariances between concurrent structured residuals are estimated.
# Covariance at age 3 is freely estimated,
# but covariances at ages 4, 5, and 7 are constrained to equality.
  # Covariances between concurrent structured residuals
  r.PD3 ~~ r.AB3
  r.PD4 ~~ s5*r.AB4
  r.PD5 ~~ s5*r.AB5
  r.PD7 ~~ s5*r.AB7
"

fit.ARCL <- lavaan(ARCL, sample.mean = means, sample.nobs = n, sample.cov = cov)
summary(fit.ARCL, fit.measures = TRUE, standardized = TRUE)


## ALT-SR model (Autoregressive Latent Trajectory with Structured Residuals)
ALTSR <- "
# Latent intercepts & latent slopes are estimated.
# Latent intercepts are random (ie, variances are estimated),
# but latent slopes are not random (ie, variances not estimated - see Fig 4, p. 4).
# Covariance between latent intercepts is estimated.
# Intercepts for observed variables are not estimated, 
# but means for latent intercepts and slopes are estimated.
  # Intercepts
  i.AB =~ 1*AB3 + 1*AB4 + 1*AB5 + 1*AB7
  i.PD =~ 1*PD3 + 1*PD4 + 1*PD5 + 1*PD7

  # Slopes
  s.AB =~ 0*AB3 + 1*AB4 + 2*AB5 + 3*AB7
  s.PD =~ 0*PD3 + 1*PD4 + 2*PD5 + 3*PD7

  # Variances and covariance between latent intercepts
  i.AB ~~ i.AB
  i.PD ~~ i.PD
  i.AB ~~ i.PD

  #  Intercept and slope means
  i.AB + i.PD + s.AB + s.PD ~ 1

# Estimate latent structured residuals
  # Structured residuals
  r.AB3 =~ 1*AB3
  r.AB4 =~ 1*AB4
  r.AB5 =~ 1*AB5
  r.AB7 =~ 1*AB7

  r.PD3 =~ 1*PD3
  r.PD4 =~ 1*PD4
  r.PD5 =~ 1*PD5
  r.PD7 =~ 1*PD7

# Regressions - note pattern of equality constraints
  # Stability (autoregressive) effects
  r.PD7 ~ b1*r.PD5
  r.PD5 ~ b1*r.PD4
  r.PD4 ~ b1*r.PD3

  r.AB7 ~ b2*r.PD5
  r.AB5 ~ b2*r.PD4
  r.AB4 ~ b2*r.PD3

  # Cross-lagged effects
  r.PD7 ~ b3*r.AB5
  r.PD5 ~ b3*r.AB4
  r.PD4 ~ b3*r.AB3

  r.AB7 ~ b4*r.AB5
  r.AB5 ~ b4*r.AB4
  r.AB4 ~ b4*r.AB3

# Variances of observed intercepts are not estimated 
# but variances of latent structured residuals are estimaed.
# Variances at age 3 is freely estimated,
# but variances at ages 4, 5, and 7 are constrained to equality.
  # Variances of structured residuals
  r.AB3 ~~ r.AB3
  r.AB4 ~~ s1*r.AB4
  r.AB5 ~~ s1*r.AB5
  r.AB7 ~~ s1*r.AB7

  r.PD3 ~~ r.PD3
  r.PD4 ~~ s2*r.PD4
  r.PD5 ~~ s2*r.PD5
  r.PD7 ~~ s2*r.PD7

# Covariances between concurrent structured residuals are estimated.
# Covariance at age 3 is freely estimated,
# but covariances at ages 4, 5, and 7 are constrained to equality.
  # Covariances between concurrent structured residuals
  r.PD3 ~~ r.AB3
  r.PD4 ~~ s5*r.AB4
  r.PD5 ~~ s5*r.AB5
  r.PD7 ~~ s5*r.AB7
"

fit.ALTSR <- lavaan(ALTSR, sample.mean = means, sample.nobs = n, sample.cov = cov)
summary(fit.ALTSR, fit.measures = TRUE, standardized = TRUE)
