
## CLPM
CLPM <- "
  # Stability paths
  PD11 ~ b1*PD10
  PD12 ~ b1*PD11
  PD15 ~ b1*PD12
  
  AB11 ~ b2*AB10
  AB12 ~ b2*AB11
  AB15 ~ b2*AB12

  # Cross-lagged paths
  AB11 ~ b3*PD10
  AB12 ~ b3*PD11
  AB15 ~ b3*PD12

  PD11 ~ b4*AB10
  PD12 ~ b4*AB11
  PD15 ~ b4*AB12

  # Variances
  PD10 ~~ PD10
  PD11 ~~ s1*PD11
  PD12 ~~ s1*PD12
  PD15 ~~ s1*PD15
  
  AB10 ~~ AB10 
  AB11 ~~ s2*AB11 
  AB12 ~~ s2*AB12 
  AB15 ~~ s2*AB15    

  # Covariances
  PD10 ~~ AB10
  PD11 ~~ s12*AB11
  PD12 ~~ s12*AB12
  PD15 ~~ s12*AB15  
"

## Fit the model and get the summary
fit.CLPM <- lavaan(CLPM, sample.cov = cov, sample.nobs = n)
summary(fit.CLPM, standardized = TRUE, fit.measures = TRUE)


##  Counting df
##  Information in the co/variance matrix
##               (8 x 9)/2 = 36

##  Unique parameters
##  stability    2
##  cross-lagged 2
##  variances    4
##  covariances  2
## TOTAL        10

## df           36 - 10 = 26
