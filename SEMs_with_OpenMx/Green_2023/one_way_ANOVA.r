
## One-Way ANOVA
##
## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.

# This example shows the SEM approach to a one-way ANOVA. 
# Results are presented in Table 21.1 (p. 389).

# The data are discussed on page 388.
# Data are available in "satisfactionI.r"
# "ANOVA_data.r" rearranges the data - from "long" to "wide".

# The variables used here are:
#   x - Coping Strategy (a - No strategy; b - Discussion; c - Exercise)
#   y - dependent variable (Life-Satisfaction) 

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
# To be consistent with ANOVA's assumption of homogeneity of variances, 
# the residual variances are constrained to equality across the groups.

## Get data into OpenMx format for each group
dataA <- mxData(observed = df[df$x == "a", c("x","y")], type = "raw")
dataB <- mxData(observed = df[df$x == "b", c("x","y")], type = "raw")
dataC <- mxData(observed = df[df$x == "c", c("x","y")], type = "raw")

### "Less Constrained" model
## Means for each group - Means differ across the groups
meanA <- mxPath(from = "one", to = "y", values = 0.5, arrows = 1, label = "a1")
meanB <- mxPath(from = "one", to = "y", values = 0.5, arrows = 1, label = "a2")
meanC <- mxPath(from = "one", to = "y", values = 0.5, arrows = 1, label = "a3")
   
## Residual variances - Constrained to equality across the groups
var <- mxPath(from = "y", values = 1, arrows = 2, label = "e") 

## Setup the group models
modA <- mxModel("GrA", type = "RAM",
   manifestVars = "y", dataA, meanA, var)

modB <- mxModel("GrB", type = "RAM",
   manifestVars = "y", dataB, meanB, var)

modC <- mxModel("GrC", type = "RAM",
   manifestVars = "y", dataC, meanC, var)

## Combine the three models
fun <- mxFitFunctionMultigroup(c("GrA", "GrB", "GrC"))
modelLC <- mxModel("LC", modA, modB, modC, fun)

## Run the LC model and get the summary
fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))

## Get the means and error variance
## Compare with SEM results in Table 21.1
meansLC <- coef(fitLC)[c("a1", "a2", "a3")]; meansLC
thetaLC <- coef(fitLC)["e"]; thetaLC

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
## Compare with SEM results in Table 21.1
meansMC <- coef(fitMC)[c("a1", "a2", "a3")]; meansMC
thetaMC <- coef(fitMC)["e"]; thetaMC

## Contrast the two models, and campare with chi sq test in Table 21.1
anova(fitMC, fitLC)

## Get R square and compare with result given on page 390.
## R square formula given in Equation 21.4 (p. 390).
Rsquare <- (thetaMC - thetaLC)/thetaMC; Rsquare

## The warning message at the bottom of the summary might be disconcerting for some. 
# One way to avoid the message is not to run the reference models.
# Without the reference models, there will be no chi sq tests, 
# but that's okay; I'm not interested in the chi squares for each separate model.
# The model comparison is what I want, and it is still available.

summary(fitLC)
summary(fitMC)
anova(fitMC, fitLC)

## But where does the warning come from.
# First, some counting of degrees of freedom.

# There is 1 variable per group;
# thus, the co/variance matrix contains (1 X 2) / 2 = 1 piece of information,
# plus the mean; that's 2 pieces of information per group.
# There are 3 groups, thus 6 pieces of information submitted to the model.

# For the LC model, 
# 1 mean per group is estimated = 3 means, and
# 1 variance in total (it is constrainted to equality across the groups)
# giving 4 quantities estimated.

# Resulting is 2 degrees of freedom for the LC model.

# When chi squares are required, and thus when reference models are included,
# two additional models are estimated:
# saturated model and independence model.

# The saturated model estimates means and variances, 
# but does not constrain variances to equality across the groups.
# There are 6 quantities estimated (3 means and 3 variances),
# leaving 0 degrees of freedom (hence the name, "saturated").
# The saturated model's log likelihood is used in the chi square calculation.
# The chi square value and df are correct (check with the values given in Table 21.1).

# The independence model sets all correlations to 0, 
# and estimates just the variances.
# But with just 1 variable, there is only 1 variance to be estimated
# (plus the means), and as a consequence, the independence model is
# the same as the saturated model, and indeed it generates the same
# log likelihood as the saturated model.
# This means that the independence model too is a perfect fit to the data.
# This, I think, is the basis of OpenMx's complaint, and the origin of the
# warning message. The independence model is supposed to be the
# worst fitting model, but in this situation it ends up fitting the data
# perfectly; that is, better, not worse, than the preferred model;

# Therefore, the warning message can be safely ignored - everything is
# correctly estimated (and an overly cautious OpenMx alerts you to the fact that
# the preferred model fits the data worse than the independence model).


### Relax homogeneity of variances assumption
### The "Less Constrained" model

## Residual variances - differ across the groups
varA <- mxPath(from = "y", arrows = 2,
   free = TRUE, values = 1, label = "e1")

varB <- mxPath(from = "y", arrows = 2,
   free = TRUE, values = 1, label = "e2")

varC <- mxPath(from = "y", arrows = 2,
   free = TRUE, values = 1, label = "e3")

## Everythiing else stays the same

## Setup the group models
modA <- mxModel("GrA", type = "RAM",
   manifestVars = "y", dataA, meanA, varA)

modB <- mxModel("GrB", type = "RAM",
   manifestVars = "y", dataB, meanB, varB)

modC <- mxModel("GrC", type = "RAM",
   manifestVars = "y", dataC, meanC, varC)

## Combine the three models
fun <- mxFitFunctionMultigroup(c("GrA", "GrB", "GrC"))
modelLC <- mxModel("LC", modA, modB, modC, fun) 

## Run the LC model and get the summary
fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))
coef(fitLC)

# Note: the LC model is the same as the saturated and, in turn, the independence model.
# All three generate the same log likelihood.

## Get the means and error variances
meansLC <- coef(fitLC)[c("a1", "a2", "a3")]; meansLC
thetaLC <- coef(fitLC)[c("e1", "e2", "e3")]; thetaLC

### The "More Constrained" model
## Constraints - same as before
C1; C2

## Add them to "Less Constrained" model
modelMC <- mxModel(modelLC, C1, C2)
modelMC <- mxModel(modelMC, name = "MC")       # Change its name 

## Run the MC model and get the summary
fitMC <- mxRun(modelMC)
summary(fitMC, refModels = mxRefModels(fitMC, run = TRUE))
coef(fitMC)

## Get the means and error variances
meansMC <- coef(fitMC)[c("a1", "a2", "a3")]; meansMC
thetaMC <- coef(fitMC)[c("e1", "e2", "e3")]; thetaMC

## Contrast the two models
anova(fitMC, fitLC)