
## One-Way ANCOVA
##
## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.

# This example show the SEM approach to a one-way ANCOVA.
# Results presented in Table 21.2 (p. 393).

# Means and variances for exogenous variables - the covariate (preC) - are not
# normally included in SEM diagrams, and they are not shown in these SEM diagrams.
# However, OpenMX requires them in its model statements.

# Data are available in "satisfactionI.r"
# "ANOVA_data.r" rearranges the data 
#   - from "long" to "wide",
#   - centering the Pre-Life_Satisfaction scores.

# The variables used here are:
#   x - Coping Strategy (a - No strategy; b - Discussion; c - Exercise)
#   y - dependent variable (Life-Satisfaction) 
#   preC - Pre-Life-Satisfaction scores centred on the grand mean

## Load packages
library(OpenMx)

## Get the data
source("satisfactionI.r")
head(df)

## Rearrange the data file
source("ANOVA_data.r")
head(df)

## Two models:
# "Less Constrained" model - means allowed to differ across the groups;
# "More Constrained" model - means constrained to equality across the groups.
# To be consistent with ANCOVA's assumptions of homogeneity of variances,
# and homogeneity of regression slopes,
# the residual variances and regression slopes are constrained to equality
# across the groups.

## Get data into OpenMx format for each group
dataA <- mxData(observed = df[df$x == "a", c("x","y", "preC")], type = "raw")
dataB <- mxData(observed = df[df$x == "b", c("x","y", "preC")], type = "raw")
dataC <- mxData(observed = df[df$x == "c", c("x","y", "preC")], type = "raw")

### "Less Constrained" model
## Means for each group - 
## "y" means differ across the groups.
## The means for the covariate ("preC") need to be included in the model.
manifest <- c("y", "preC")
meanA <- mxPath(from = "one", to = manifest, arrows = 1,
   values = 0.5, label = c("a1", "cov1"))
meanB <- mxPath(from = "one", to = manifest, arrows = 1,
   values = 0.5, label = c("a2", "cov2"))
meanC <- mxPath(from = "one", to = manifest, arrows = 1,
  values = 0.5 , label = c("a3", "cov3"))

## Regression slopes - Constrained to equality across the groups
reg <- mxPath(from = "preC", to = "y", arrows = 1,
   values = 0.5, label = "b")

## Variances - 
## Residual variances - Constrained to equality across the groups
## The variances for the covariate ("preC") need to be included in the model
varA <- mxPath(from = manifest, arrows = 2,
   values = 1, label = c("e", "cov1var")) 
   
varB <- mxPath(from = manifest, arrows = 2,
   values = 1, label = c("e", "cov2var"))
   
varC <- mxPath(from = manifest, arrows = 2,
   values = 1, label = c("e", "cov3var"))

## Setup the group models
modA <- mxModel("GrA", type = "RAM",
   manifestVars = manifest, dataA, meanA, reg, varA)

modB <- mxModel("GrB", type = "RAM",
   manifestVars = manifest, dataB, meanB, reg, varB)

modC <- mxModel("GrC", type = "RAM",
   manifestVars = manifest, dataC, meanC, reg, varC)

## Combine the three models
fun <- mxFitFunctionMultigroup(c("GrA.fitfunction", "GrB.fitfunction", "GrC.fitfunction"))
modelLC <- mxModel("LC", modA, modB, modC, fun)

## Run the LC model and get the summary
fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))

## Get the means and error variance
## Compare with SEM results in Table 21.2
meansLC <- coef(fitLC)[c("a1", "a2", "a3")]; meansLC
thetaLC <- coef(fitLC)["e"]; thetaLC

## Get regression slopes and compare with Table 21.2 footnote
slopeLC <- coef(fitLC)["b"]; slopeLC

### "More Constrained" model
## Constraints
C1 <- mxConstraint(a1 == a2)
C2 <- mxConstraint(a2 == a3)

## Add them to "Less Constrained" model
modelMC <- mxModel(modelLC, C1, C2)
modelMC <- mxModel(modelMC, name = "MC")       # Change its name 

## Run the MC model and get the summary
fitMC <- mxRun(modelMC)
summary(fitMC, refModels = mxRefModels(fitMC, run = TRUE))
coef(fitMC)

## Get the means and error variance
## Compare with SEM results in Table 21.2
meansMC <- coef(fitMC)[c("a1", "a2", "a3")]; meansMC
thetaMC <- coef(fitMC)["e"]; thetaMC

## Get regression slopes and compare with Table 21.2 footnote
slopeMC <- coef(fitMC)["b"]; slopeMC

## Contrast the two models, and campare with chi sq test in Table 21.2
anova(fitMC, fitLC)

## Get R square and compare with result given on page 394.
## R square formula given in Equation 21.9 (p. 394).
Rsquare <- (thetaMC - thetaLC)/thetaMC; Rsquare
