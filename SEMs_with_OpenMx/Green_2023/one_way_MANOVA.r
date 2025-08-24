
## One-way MANOVA
##
## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.

# This example shows the SEM approach to a one-way MANOVA.
# Results presented in Table 21.5 (p. 399).

# The data are discussed on page 397.
# Data are available in "satisfactionII.r".

# The variables used here are:
#   x - Coping Strategy (a - No strategy; b - Discussion; c - Exercise)
#   y1, y2, y3, y4 - multiple dependent variables (Life-Satisfaction)

## Load packages
library(OpenMx)

## Get the data
source("satisfactionII.r")
head(df)

## Two models:
# "Less Constrained" model - means allowed to differ across the groups;
# "More Constrained" model - means constrained to equality across the groups.
# Variances and covariances are constrained to equality across the groups.

## Get data into OpenMx format for each group
dataA <- mxData(observed = df[df$x == "a", c("x", "y1", "y2", "y3", "y4")], type = "raw")
dataB <- mxData(observed = df[df$x == "b", c("x", "y1", "y2", "y3", "y4")], type = "raw")
dataC <- mxData(observed = df[df$x == "c", c("x", "y1", "y2", "y3", "y4")], type = "raw")

### "Less Constrained" model
## Means for each group - Means differ across the groups
manifest <- c("y1", "y2", "y3", "y4")
meanA <- mxPath(from = "one", to = manifest , arrows = 1,
   values = 0.5, label = c("a1y1", "a1y2", "a1y3", "a1y4"))

meanB <- mxPath(from = "one", to = manifest , arrows = 1,
   values = 0.5, label = c("a2y1", "a2y2", "a2y3", "a2y4"))

meanC <- mxPath(from = "one", to = manifest , arrows = 1,
   values = 0.5, label = c("a3y1", "a3y2", "a3y3", "a3y4"))

## Residual variances/covariances - Constrained to equality across the groups
var <- mxPath(from = manifest, to = manifest, connect = "unique.pairs", arrows = 2,
   values = c(
   1, 0.5, 0.5, 0.5,
   1, 0.5, 0.5,
   1, 0.5,
   1),
   labels = 
   c("e1", "e12", "e13", "e14",
     "e2", "e23", "e24",
	"e3", "e34",
	"e4"))

## Setup the group models
modA <- mxModel("GrA", type = "RAM",
   manifestVars = manifest, dataA, meanA, var)

modB <- mxModel("GrB", type = "RAM",
   manifestVars = manifest, dataB, meanB, var)

modC <- mxModel("GrC", type = "RAM",
   manifestVars = manifest, dataC, meanC, var)

## Combine the three models
fun <- mxFitFunctionMultigroup(c("GrA", "GrB", "GrC"))
modelLC <- mxModel("LC", modA, modB, modC, fun)

## Run the LC model and get the summary
fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))

## Get the means, and compare with SEM results in Table 21.5
meansLC <- coef(fitLC)[grepl("^a", names(coef(fitLC)))]
matrix(meansLC, byrow = TRUE, nrow = 3, dimnames = list(c("a", "b", "c"), manifest))

## Get the error SSCP matrix - compare with Table 21.5
## error SSCP = variance/covariance matrix X sample size  
thetaLC <- coef(fitLC)[grepl("^e", names(coef(fitLC)))]; thetaLC

eLC <- matrix( , 4, 4)                            # Empty matrix
eLC[upper.tri(eLC, diag = TRUE)] <- thetaLC       # Fill the upper triangle
eLC <- pmax(eLC, t(eLC), na.rm = TRUE)            # Fill the lower triangle
eLC <- eLC * 200
matrix(eLC, 4, 4, dimnames = list(manifest, manifest))

### "More Constrained" model
# Constraints
C1 <- mxConstraint(a1y1 == a2y1)
C2 <- mxConstraint(a2y1 == a3y1)

C3 <- mxConstraint(a1y2 == a2y2)
C4 <- mxConstraint(a2y2 == a3y2)

C5 <- mxConstraint(a1y3 == a2y3)
C6 <- mxConstraint(a2y3 == a3y3)

C7 <- mxConstraint(a1y4 == a2y4)
C8 <- mxConstraint(a2y4 == a3y4)

## Add them to "Less Constrained" model
modelMC <- mxModel(modelLC, C1, C2, C3, C4, C5, C6, C7, C8)
modelMC <- mxModel(modelMC, name = "MC")       # Change its name 

## Run the MC model and get the summary
fitMC <- mxRun(modelMC)
summary(fitMC, refModels = mxRefModels(fitMC, run = TRUE))

## Get the means, and compare with SEM results in Table 21.5
meansMC <- coef(fitMC)[grepl("^a", names(coef(fitLC)))]
matrix(meansMC, byrow = TRUE, nrow = 3, dimnames = list(c("a", "b", "c"), manifest))

## Get the error SSCP matrix - Table 21.5
## error SSCP = variance/covariance matrix X sample size
thetaMC <- coef(fitMC)[grepl("^e", names(coef(fitLC)))]; thetaMC

eMC <- matrix( , 4, 4)                              # Empty matrix
eMC[upper.tri(eMC, diag = TRUE)] <- thetaMC         # Fill the upper triangle
eMC <- pmax(eMC, t(eMC), na.rm = TRUE)              # Fill the lower triangle
eMC <- eMC*200
matrix(eMC, 4, 4, dimnames = list(manifest, manifest))

## Contrast the two models, and campare with chi sq test in Table 21.5
anova(fitLC, fitMC)


### Relax homogeneity of variances and covariances assumption. 
### See discussion in section headed "Avoiding OLS Assumptions for 
### ANOVA/MANOVA Designs Using SEM" (pp. 389-401)
### "Less Constrained" model
# Variance and covariances - differ across the groups
varA <- mxPath(from = manifest, to = manifest, connect = "unique.pairs", arrows = 2,
   values = c(
   1, 0.5, 0.5, 0.5,
        1, 0.5, 0.5,
             1, 0.5,
                  1),
   labels = 
   c("e11a1", "e12a1", "e13a1", "e14a1",
              "e22a1", "e23a1", "e24a1",
	                  "e33a1", "e34a1",
	                           "e44a1"))

varB <- mxPath(from = manifest, to = manifest, connect = "unique.pairs", arrows = 2,
   values = c(
   1, 0.5, 0.5, 0.5,
   1, 0.5, 0.5,
   1, 0.5,
   1),
   labels = 
   c("e11a2", "e12a2", "e13a2", "e14a2",
     "e22a2", "e23a2", "e24a2",
	"e33a2", "e34a2",
	"e44a2"))

varC <- mxPath(from = manifest, to = manifest, connect = "unique.pairs", arrows = 2,
   values = c(
   1, 0.5, 0.5, 0.5,
   1, 0.5, 0.5,
   1, 0.5,
   1),
   labels = 
   c("e11a3", "e12a3", "e13a3", "e14a3",
     "e22a3", "e23a3", "e24a3",
	"e33a3", "e34a3",
	"e44a3"))

## Everything else stays the same

## Setup the group models
modA <- mxModel("GrA", type = "RAM",
   manifestVars = manifest, dataA, meanA, varA)

modB <- mxModel("GrB", type = "RAM",
   manifestVars = manifest, dataB, meanB, varB)

modC <- mxModel("GrC", type = "RAM",
   manifestVars = manifest, dataC, meanC, varC)

## Combine the three models
fun <- mxFitFunctionMultigroup(c("GrA", "GrB", "GrC"))
modelLC <- mxModel("LC", modA, modB, modC, fun)

## Run the LC model and get the summary
fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))

## Get the means
meansLC = coef(fitLC)[grepl("^a", names(coef(fitLC)))]
matrix(meansLC, byrow = TRUE, nrow = 3, dimnames = list(c("a", "b", "c"), manifest))

## Get variance/covariance matrix
thetaLC = coef(fitLC)[grepl("^e", names(coef(fitLC)))]; thetaLC

### "More Constrained" model
## Constraints - same as before
C1; C2; C3; C4; C5; C6; C7; C8

## Add them to less constrained model
modelMC <- mxModel(modelLC, C1, C2, C3, C4, C5, C6, C7, C8)
modelMC <- mxModel(modelMC, name = "MC")       # Change its name 

## Run the MC model and get the summary
fitMC <- mxRun(modelMC)
summary(fitMC, refModels = mxRefModels(fitMC, run = TRUE))

## Get the means
meansMC <- coef(fitMC)[grepl("^a", names(coef(fitLC)))]
matrix(meansMC, byrow = TRUE, nrow = 3, dimnames = list(c("a", "b", "c"), manifest))

## Get the variance/covariance matrix
thetaMC <- coef(fitMC)[grepl("^e", names(coef(fitLC)))]; thetaMC

## Contrast the two models, and campare with chi sq test on page 401
anova(fitLC, fitMC)



#### lavaan
library(lavaan)

# Variances and covariances (for both models)
vcov <- "
   y1 ~~ y1 + y2 + y3 + y4
   y2 ~~ y2 + y3 + y4
   y3 ~~ y3 + y4
   y4 ~~ y4"

models <- list(

"Less Constrained" =  c(
# Means
   "y1 ~ c(a1, b1, c1)*1
    y2 ~ c(a2, b2, c2)*1
    y3 ~ c(a3, b3, c3)*1
    y4 ~ c(a4, b4, c4)*1",
	
    vcov),

"More Constrained" = c(
# Means
   "y1 ~ c(a1, a1, a1)*1
    y2 ~ c(a2, a2, a2)*1
    y3 ~ c(a3, a3, a3)*1
    y4 ~ c(a4, a4, a4)*1",
	
	vcov)
)

# Fit the models 
fit <- lapply(models, sem, data = df, group = "x")

# Get model summaries
lapply(fit, summary)[[1]]

# Contrast model fits
Reduce(anova, fit)
