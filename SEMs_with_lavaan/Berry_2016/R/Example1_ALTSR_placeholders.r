
## ALT-SR
## with latent placeholders for ages 13 and 14 to create equal longitudinal spacing of observations
EqualLags<- "
# Latent intercepts are random (ie, variances are estimated),
# but latent slopes are not random (ie, variances not estimated - see Fig 4).

  # Latent variable placeholders For equal lags
  AB13 =~ 0*AB13
  AB14 =~ 0*AB14
  PD13 =~ 0*PD13
  PD14 =~ 0*PD14

  # Intercepts
  i.AB =~ 1*AB10 + 1*AB11 + 1*AB12 + 1*AB13 + 1*AB14 + 1*AB15
  i.PD =~ 1*PD10 + 1*PD11 + 1*PD12 + 1*PD13 + 1*PD14 + 1*PD15

  # Slopes
  s.AB =~ 0*AB10 + 1*AB11 + 2*AB12 + 3*AB13 + 4*AB14 + 5*AB15
  s.PD =~ 0*PD10 + 1*PD11 + 2*PD12 + 3*PD13 + 4*PD14 + 5*PD15

  # Variances and covariance - latent intercepts
  i.AB ~~ i.AB
  i.PD ~~ i.PD
  i.AB ~~ i.PD

  #  Intercept and slope means
  i.AB + i.PD + s.AB + s.PD ~ 1

  # Structured residuals
  r.AB10 =~ 1*AB10
  r.AB11 =~ 1*AB11
  r.AB12 =~ 1*AB12
  r.AB13 =~ 1*AB13
  r.AB14 =~ 1*AB14
  r.AB15 =~ 1*AB15

  r.PD10 =~ 1*PD10
  r.PD11 =~ 1*PD11
  r.PD12 =~ 1*PD12
  r.PD13 =~ 1*PD13
  r.PD14 =~ 1*PD14
  r.PD15 =~ 1*PD15

  # Stability (autoregressive) paths
  r.PD15 ~ b1*r.PD14
  r.PD14 ~ b1*r.PD13
  r.PD13 ~ b1*r.PD12
  r.PD12 ~ b1*r.PD11
  r.PD11 ~ b1*r.PD10

  r.AB15 ~ b2*r.AB14
  r.AB14 ~ b2*r.AB13
  r.AB13 ~ b2*r.AB12
  r.AB12 ~ b2*r.AB11
  r.AB11 ~ b2*r.AB10

  # Cross-lagged paths
  r.AB15 ~ b3*r.PD14
  r.AB14 ~ b3*r.PD13
  r.AB13 ~ b3*r.PD12
  r.AB12 ~ b3*r.PD11
  r.AB11 ~ b3*r.PD10

  r.PD15 ~ b4*r.AB14
  r.PD14 ~ b4*r.AB13
  r.PD13 ~ b4*r.AB12
  r.PD12 ~ b4*r.AB11
  r.PD11 ~ b4*r.AB10

  # Variances - structured residuals
  r.PD10 ~~ r.PD10
  r.PD11 ~~ s1*r.PD11
  r.PD12 ~~ s1*r.PD12
  r.PD13 ~~ s1*r.PD13
  r.PD14 ~~ s1*r.PD14
  r.PD15 ~~ s1*r.PD15

  r.AB10 ~~ r.AB10
  r.AB11 ~~ s2*r.AB11
  r.AB12 ~~ s2*r.AB12
  r.AB13 ~~ s2*r.AB13
  r.AB14 ~~ s2*r.AB14
  r.AB15 ~~ s2*r.AB15

  # Covariances - structured residuals
  r.PD10 ~~ r.AB10
  r.PD11 ~~ s12*r.AB11
  r.PD12 ~~ s12*r.AB12
  r.PD13 ~~ s12*r.AB13
  r.PD14 ~~ s12*r.AB14
  r.PD15 ~~ s12*r.AB15
"

fit.EqualLags <- lavaan(EqualLags, sample.mean = means, sample.nobs = 290, sample.cov = cov)
summary(fit.EqualLags, fit.measures = TRUE, standardized = TRUE)
