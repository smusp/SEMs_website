
## ARCL
ARCL <- "
  # Intercepts
  i.AB =~ 1*AB10 + 1*AB11 + 1*AB12 + 1*AB15
  i.PD =~ 1*PD10 + 1*PD11 + 1*PD12 + 1*PD15

  # Slopes
  s.AB =~ 0*AB10 + 1*AB11 + 2*AB12 + 3*AB15
  s.PD =~ 0*PD10 + 1*PD11 + 2*PD12 + 3*PD15

  # Intercept and slope means
  i.AB + i.PD + s.AB + s.PD ~ 1

  # Structured residuals
  r.AB10 =~ 1*AB10
  r.AB11 =~ 1*AB11
  r.AB12 =~ 1*AB12
  r.AB15 =~ 1*AB15

  r.PD10 =~ 1*PD10
  r.PD11 =~ 1*PD11
  r.PD12 =~ 1*PD12
  r.PD15 =~ 1*PD15

  # Stability (autoregressive) paths
  r.PD15 ~ b1*r.PD12
  r.PD12 ~ b1*r.PD11
  r.PD11 ~ b1*r.PD10

  r.AB15 ~ b2*r.AB12
  r.AB12 ~ b2*r.AB11
  r.AB11 ~ b2*r.AB10

  # Cross-lagged paths
  r.PD15 ~ b3*r.AB12
  r.PD12 ~ b3*r.AB11
  r.PD11 ~ b3*r.AB10

  r.AB15 ~ b2*r.PD12
  r.AB12 ~ b2*r.PD11
  r.AB11 ~ b2*r.PD10

  # Variances
  r.PD10 ~~ r.PD10
  r.PD11 ~~ s1*r.PD11
  r.PD12 ~~ s1*r.PD12
  r.PD15 ~~ s1*r.PD15

  r.AB10 ~~ r.AB10
  r.AB11 ~~ s2*r.AB11
  r.AB12 ~~ s2*r.AB12
  r.AB15 ~~ s2*r.AB15

  # Covariances
  r.PD10 ~~ r.AB10
  r.PD11 ~~ s12*r.AB11
  r.PD12 ~~ s12*r.AB12
  r.PD15 ~~ s12*r.AB15
"

fit.ARCL <- lavaan(ARCL, sample.mean = means, sample.nobs = n, sample.cov = cov)
summary(fit.ARCL, fit.measures = TRUE, standardized = TRUE)
