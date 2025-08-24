
## Two-Way ANOVA
##
## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.

# This example shows the SEM approach to a two-way ANOVA.
# Results presented in Table 21.4 (p. 396).

# Data are available in "satisfactionI.r"
# "ANOVA_data.r" rearranges the data 
#   - from "long" to "wide",
#   - sets up the Gender X Coping Strategy interaction

# The variables used here are:
#   x - Coping Strategy (a - No strategy; b - Discussion; c - Exercise)
#   y - dependent variable (Life-Satisfaction) 
#   g - Gender
#   sg - Gender X Coping Strategy interaction

## Load packages
library(OpenMx)

## Get the data
source("satisfactionI.r")
head(df)

## Rearrange the data file
source("ANOVA_data.r")
head(df)

## Code to get the  prelimnary results presented Table 21.3 (p. 395) 
## is not shown here

## Two-way ANOVA
# There are six groups in the model, and thus there are six means.
# They are represented by the labels am, af, ..., cf in the model diagram.
# The "Less Constrained" model allows the six means to differ. 
#
# There are three "More Constrained" models to test for:
#   Gender main effect,
#   Coping Strategy main effect, and
#   Gender X Coping Strategy interaction.
# These effects can be tested for unweighted means and weighted means.
# One page 394, TLG discuss when to use weighted and unweighted means.

# To be consistent with ANOVA's assumption of homogeneity of variances,
# the variances are constrained to equality aross the groups.

# Table 21.4 (p. 396) shows the results for the test of the
# "Coping Strategy" main effect for weighted means.
# Here, tests for both main effects and the interaction
# for weighted and unweighted means are shown.

## Get data into OpenMx format for each group
dataAM <- mxData(observed = df[df$sg == "am", c("sg","y")], type = "raw")
dataAF <- mxData(observed = df[df$sg == "af", c("sg","y")], type = "raw")
dataBM <- mxData(observed = df[df$sg == "bm", c("sg","y")], type = "raw")
dataBF <- mxData(observed = df[df$sg == "bf", c("sg","y")], type = "raw")
dataCM <- mxData(observed = df[df$sg == "cm", c("sg","y")], type = "raw")
dataCF <- mxData(observed = df[df$sg == "cf", c("sg","y")], type = "raw")

### "Less Constrained" model
## Means for each group - Means differ across the groups
meanAM <- mxPath(from = "one", to = "y", arrows = 1, values = 0.5, label = "am")
meanAF <- mxPath(from = "one", to = "y", arrows = 1, values = 0.5, label = "af")
meanBM <- mxPath(from = "one", to = "y", arrows = 1, values = 0.5, label = "bm")
meanBF <- mxPath(from = "one", to = "y", arrows = 1, values = 0.5, label = "bf")
meanCM <- mxPath(from = "one", to = "y", arrows = 1, values = 0.5, label = "cm")
meanCF <- mxPath(from = "one", to = "y", arrows = 1, values = 0.5, label = "cf")

## Residual variance - Constrained to equality across the groups
var <- mxPath(from = "y", arrows = 2, values = 1, label = "e")

## Setup the group models
modAM <- mxModel("GrAM", type = "RAM",
   manifestVars = "y", dataAM, meanAM, var)

modAF <- mxModel("GrAF", type = "RAM",
   manifestVars = "y", dataAF, meanAF, var)

modBM <- mxModel("GrBM", type = "RAM",
   manifestVars = "y", dataBM, meanBM, var)

modBF <- mxModel("GrBF", type = "RAM",
   manifestVars = "y", dataBF, meanBF, var)

modCM <- mxModel("GrCM", type = "RAM",
   manifestVars = "y", dataCM, meanCM, var)

modCF <- mxModel("GrCF", type = "RAM",
   manifestVars = "y", dataCF, meanCF, var)

## Combine the six models
fun <- mxFitFunctionMultigroup(c(
   "GrAM", "GrAF", "GrBM", "GrBF", "GrCM", "GrCF"))
modelLC <- mxModel("LC", modAM, modAF, modBM, modBF, modCM, modCF, fun)

## Run the LC model and get the summary
fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))

## Get the means and error variance
## Compare with SEM results in Table 21.4
meansLC <- coef(fitLC)[c("am", "bm", "cm", "af", "bf", "cf")]; meansLC
thetaLC <- coef(fitLC)["e"]; thetaLC

### Gender main effect - unweighted means
## To test the Gender main effect (applied to unweighted means), 
## constrain the mean for males to equal the mean for females. 
## But there are three means for males and three means for females. 
## Simply constrain the sum of the three means for males to equal 
## the sum of the three means for females.

## Constraint
conGU <- mxConstraint(af + bf + cf == am + bm + cm)

## Add it to "Less Constrained" model
modelGU <- mxModel(modelLC, conGU)
modelGU <- mxModel(modelGU, name = "Gender Unweighted")       # Change its name 

## Run the MC model and get the summary
fitGU <- mxRun(modelGU)
summary(fitGU, refModels = mxRefModels(fitGU, run = TRUE))

## Get the means and error variance
meansGU <- coef(fitGU)[c("am", "bm", "cm", "af", "bf", "cf")]; meansGU
thetaGU <- coef(fitGU)["e"]; thetaGU

## Contrast the two models
anova(fitGU, fitLC)

### Coping Strategy main effect - unweighted means
## To test for the "Coping Strategy" main effect, 
## restrict the mean for "a" strategy to equal the mean for "b" strategy
## to equal the mean for "c" strategy. That is, constrain 
## the sum of the two "a" means to equal 
## the sum of the two "b" means; and 
## the sum of the two "b" means to equal 
## the sum of the two "c" means.

## Constraints
conCU1 <- mxConstraint(af + am == bf + bm)
conCU2 <- mxConstraint(af + am == cf + cm)

## Add them to "Less Constrained" model
modelCU <- mxModel(modelLC, conCU1, conCU2)
modelCU <- mxModel(modelCU, name = "Coping Unweighted")       # Change its name 

## Run the MC model and get the summary
fitCU <- mxRun(modelCU)
summary(fitCU, refModels = mxRefModels(fitCU, run = TRUE))

## Get the means and error variance
meansCU <- coef(fitCU)[c("am", "bm", "cm", "af", "bf", "cf")]; meansCU
thetaCU <- coef(fitCU)["e"]; thetaCU

## Contrast the two models
anova(fitCU, fitLC)

### Gender main effect - weighted means
## To test for the main effects applied to weighted means,
## the constraints are set the same way as before except this time
## the means are weighted in proportion to the cell frequencies.

# Constraint
freq <- table(df$g, df$x); freq      # cell frequencies
conGW <- mxConstraint((3*af + 3*bf + 6*cf)/12 == (6*am + 3*bm + 3*cm)/12)

## Add it to "Less Constrained" model
modelGW <- mxModel(modelLC, conGW)
modelGW <- mxModel(modelGW, name = "Gender Weighted")       # Change its name

## Run the MC model and get the summary
fitGW <- mxRun(modelGW)
summary(fitGW, refModels = mxRefModels(fitGW, run = TRUE))

## Get the means and error variance
meansGW <- coef(fitGW)[c("am", "bm", "cm", "af", "bf", "cf")]; meansGW
thetaGW <- coef(fitGW)["e"]; thetaGW

## Contrast the two models
anova(fitGW, fitLC)

### Coping Strategy main effect - weighted means
# Compare with SEM section in Table 21.4

## Constraints
freq <- table(df$g, df$x); freq      # cell frequencies
conCW1 <- mxConstraint((3*af + 6*am)/9 == (3*bf + 3*bm)/6 )
conCW2 <- mxConstraint((3*bf + 3*bm)/6 == (6*cf + 3*cm)/9)

## Add them to "Less Constrained" model
modelCW <- mxModel(modelLC, conCW1, conCW2)
modelCW <- mxModel(modelCW, name = "Coping Weighted")       # Change its name

## Run the MC model and get the summary
fitCW <- mxRun(modelCW)
summary(fitCW, refModels = mxRefModels(fitCW, run = TRUE))

## Get the means and error variance
## Compare with SEM results in Table 21.4
meansCW <- coef(fitCW)[c("am", "bm", "cm", "af", "bf", "cf")]; meansCW
thetaCW <- coef(fitCW)["e"]; thetaCW

## Contrast the two models
anova(fitCW, fitLC)

### Gender X Coping Strategy interaction
## To test for the Gender X Coping Strategy interaction,
## the "More Constrained" model needs the means to be constrained so that
## the difference between the mean for "female" and the mean for "male" 
## remains constant across levels of "Coping Strategy". That is:

## the difference between "female" mean and "male" mean for the "a" strategy equals
## the difference between "female" mean and "male" mean for the "b" strategy; and
## the difference between "female" mean and "male" mean for the "b" strategy equals
## the difference between "female" mean and "male" mean for the "c" strategy.

## Constraints
conI1 <- mxConstraint((af - am) == (bf - bm))
conI2 <- mxConstraint((bf - bm) == (cf - cm))

## Add them to "Less Constrained" model
modelI <- mxModel(modelLC, conI1, conI2)
modelI <- mxModel(modelI, name = "Interaction")       # Change its name

## Run the MC model and get the summary
fitI <- mxRun(modelI)
summary(fitI, refModels = mxRefModels(fitI, run = TRUE))

## Get the means and error variance
meansI <- coef(fitI)[c("am", "bm", "cm", "af", "bf", "cf")]; meansI
thetaI <- coef(fitI)["e"]; thetaI

## Contrast the two models
anova(fitI, fitLC)
