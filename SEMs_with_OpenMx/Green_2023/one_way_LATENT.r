
## One-way ANOVA of latent variable
##
## Thompson, M., Lie, Y. & Green, S. (2023). Flexible structural equation modeling
## approaches for analyzing means. In R. Hoyle (Ed.), Handbook of structural
## equation modeling (2nd ed., pp. 385-408). New York, NY: Guilford Press.

# This example shows the SEM approach to a one-way ANOVA of latent means.
# Results presented in Table 21.6 (p. 404).

# Data are available in "satisfactionII.r".

# The variables used here are:
#   x - Coping Strategy (a - No strategy; b - Discussion; c - Exercise)
#   y1, y2, y3, y4 - multiple dependent variables (Life-Satisfaction) 

## Load packages
library(OpenMx)

## Get the data
source("satisfactionII.r")
head(df)

## Comparisons of latent means assume some level of measurement invariance.
# Thompson, Lie, and Green (TLG) assume strict measurement invariance:
# loadings, intercepts, and residual variances and covariances are constrainted
# to equality across the groups (covariances are zero, and thus they are equal).
# TLG also constrain the latent error variance to equality across the groups.

# Latent variables require constraints for the purposes of identification and scaling.
# TLG claim the loading for the 4th indicator is constrained to 1,
# but I think that is a typo. TLG constrain the loading of the 1st indicator to 1,
# and because of measurement invariance, this constraint applies to all groups.
# Also, TLG constrain the latent mean to 0 in the first group only.

## Two models:
# "Less Constrained" model - latent means are freely estimated in the other two groups;
# "More Constrained" model - latent means constrained to equality across the groups;
# that is, they are all constrained to 0.

## Get data into OpenMx format for each group
dataA <- mxData(observed = df[df$x == "a", c("x", "y1", "y2", "y3", "y4")], type = "raw")
dataB <- mxData(observed = df[df$x == "b", c("x", "y1", "y2", "y3", "y4")], type = "raw")
dataC <- mxData(observed = df[df$x == "c", c("x", "y1", "y2", "y3", "y4")], type = "raw")

### "Less Constrained" model
# Factor loadings - equal across groups
manifest <- c("y1", "y2", "y3", "y4")
loadings <- mxPath(from = "LS", to = manifest, arrows = 1,
   free = c(FALSE, TRUE, TRUE, TRUE), values = 1,   # First loading constrained to 1
   labels = c("l1", "l2", "l3", "l4"))

## Factor variances - equal across groups
varFac <- mxPath(from = "LS", arrows = 2,
   free = TRUE, values = 1, labels = "d")

## Residual variances - equal across groups
varRes <- mxPath(from = manifest, arrows = 2,
   free = TRUE, values = 1, 
   labels = c("e1", "e2", "e3", "e4"))

## Intercepts - equal across groups
intercepts <- mxPath(from = "one", to = manifest, arrows = 1,
   free = TRUE, values = 1,
   labels = c("t1", "t2", "t3", "t4"))

## Factor means - differs across the groups
meanA <- mxPath(from = "one", to = "LS", arrows = 1,
   free = FALSE, values = 0, labels = "a1")
meanB <- mxPath(from = "one", to = "LS", arrows = 1,
   free = TRUE, values = 1, labels = "a2")
meanC <- mxPath(from = "one", to = "LS", arrows = 1,
   free = TRUE, values = 1, labels = "a3")

## Setup the group models
modA <- mxModel("GrA", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataA, loadings, varFac, varRes, intercepts, meanA)

modB <- mxModel("GrB", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataB, loadings, varFac, varRes, intercepts, meanB)

modC <- mxModel("GrC", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataC, loadings, varFac, varRes, intercepts, meanC)

## Combine the three models
fun <- mxFitFunctionMultigroup(c("GrA", "GrB", "GrC"))
modelLC <- mxModel("LC", modA, modB, modC, fun)

## Run the LC model and get the summary
fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))

## Trouble!! 
## Try letting OpenMx select starting values
modelLC = mxAutoStart(modelLC)

## Try again
fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))
## All good

## Get latent means and variance, and compare with "All measures" row in Table 21.6
estimates <- coef(fitLC)

latentMeans = coef(fitLC)[c("a2", "a3")]; latentMeans
latentVar = coef(fitLC)["d"]; latentVar

### "More Constrained" model
## Constraints
C1 <- mxConstraint(a2 == 0)
C2 <- mxConstraint(a3 == 0)

## Add them to "Less Constrained" model
modelMC <- mxModel(modelLC, C1, C2)
modelMC <- mxModel(modelMC, name = "MC")       # Change its name 

## Run the MC model and get the summary
fitMC <- mxRun(modelMC)
summary(fitMC, refModels = mxRefModels(fitMC, run = TRUE))

## Contrast the two models, and campare with chi sq test in Table 21.6
anova(fitLC, fitMC)

## Effect sizes, and compare with values on p. 405
# Cut-and-paste means and variances to get effect sizes
d1 <- (0.6638 - 0) / sqrt(8.1346); d1    # "no strategy" vs "discussion"
d2 <- (1.9446 - 0) / sqrt(8.1346); d2    # "no strategy" vs "exercise"

## Better - extract latent means and error variances from "Less Constrained" model
## Already extracted above
d1 <- (latentMeans[1] - 0) / sqrt(latentVar); d1   # "no strategy" vs "discussion"
d2 <- (latentMeans[2] - 0) / sqrt(latentVar); d2   # "no strategy" vs "exercise"


#### Minimal constraints
# This section applies to the 2nd and 3rd rows in Table 21.6.
# And see the discussion at the end of page 406, and top of page 407.

## Get data into OpenMx format for each group
dataA <- mxData(observed = df[df$x == "a", c("x", "y1", "y2", "y3", "y4")], type = "raw")
dataB <- mxData(observed = df[df$x == "b", c("x", "y1", "y2", "y3", "y4")], type = "raw")
dataC <- mxData(observed = df[df$x == "c", c("x", "y1", "y2", "y3", "y4")], type = "raw")


### ANOVA model for 2nd row in Table 21.6
# Constrain 1st loading only to equality across groups
# and constrain 1st intercept only to equality across groups.

## "Less Constrained" model

manifest <- c("y1", "y2", "y3", "y4")

# Factor loadings - 1st loading constrained to 1
#                 - other loadings freely estimated across groups
loadsA <- mxPath(from = "LS", to = manifest, arrows = 1,
   free = c(FALSE, TRUE, TRUE, TRUE), values = 1,
   labels = c("l1", "l2A", "l3A", "l4A"))

loadsB <- mxPath(from = "LS", to = manifest, arrows = 1,
   free = c(FALSE, TRUE, TRUE, TRUE), values = 1,
   labels = c("l1", "l2B", "l3B", "l4B"))

loadsC <- mxPath(from = "LS", to = manifest, arrows = 1,
   free = c(FALSE, TRUE, TRUE, TRUE), values = 1,
   labels = c("l1", "l2C", "l3C", "l4C"))

# Factor variances - freely estimated across groups	
varFacA <- mxPath(from = "LS", arrows = 2,
   free = TRUE, values = 1, labels = "dA")

varFacB <- mxPath(from = "LS", arrows = 2,
   free = TRUE, values = 1, labels = "dB")

varFacC <- mxPath(from = "LS", arrows = 2,
   free = TRUE, values = 1, labels = "dC")

# Residual variances - freely estimated across groups
varResA <- mxPath(from = manifest, arrows = 2,
   free = TRUE, values = 1, 
   labels = c("e1A", "e2A", "e3A", "e4A"))

varResB <- mxPath(from = manifest, arrows = 2,
   free = TRUE, values = 1, 
   labels = c("e1B", "e2B", "e3B", "e4B"))

varResC <- mxPath(from = manifest, arrows = 2,
   free = TRUE, values = 1,
   labels = c("e1C", "e2C", "e3C", "e4C"))

# Intercepts - 1st intercept constrained to equality across groups
#            - other intercepts freely estimated across groups
interceptsA <- mxPath(from = "one", to = manifest, arrows = 1,
   free = TRUE, values = 1,
   labels = c("t1", "t2A", "t3A", "t4A"))

interceptsB <- mxPath(from = "one", to = manifest, arrows = 1,
   free = TRUE, values = 1,
   labels = c("t1", "t2B", "t3B", "t4B"))

interceptsC <- mxPath(from = "one", to = manifest, arrows = 1,
   free = TRUE, values = 1,
   labels = c("t1", "t2C", "t3C", "t4C"))

# Factor means - differ across the groups
meanA <- mxPath(from = "one", to = "LS", arrows = 1,
   free = FALSE, values = 0, labels = "aA")
meanB <- mxPath(from = "one", to = "LS", arrows = 1,
   free = TRUE, values = 1, labels = "aB")
meanC <- mxPath(from = "one", to = "LS", arrows = 1,
   free = TRUE, values = 1, labels = "aC")

# Setup the group models
modA <- mxModel("GrA", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataA, loadsA, varFacA, varResA, interceptsA, meanA)

modB <- mxModel("GrB", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataB, loadsB, varFacB, varResB, interceptsB, meanB)

modC <- mxModel("GrC", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataC, loadsC, varFacC, varResC, interceptsC, meanC)

# Combine the three models
fun <- mxFitFunctionMultigroup(c("GrA", "GrB", "GrC"))
modelLC <- mxModel("LC", modA, modB, modC, fun)

# Run the LC model and get the summary
fitLC <- mxRun(modelLC)

# Again trouble, but let OpenMx choose starting values
modelLC = mxAutoStart(modelLC)

fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))
# All good

## More Consttained model
# Constraints
C1 = mxConstraint(aB == 0)
C2 = mxConstraint(aC == 0)

# Add them to less constrained model
modelMC = mxModel(modelLC, C1, C2)
modelMC = mxModel(modelMC, name = "MC")       # Change its name

# Run the MC model and get the summary
fitMC <- mxRun(modelMC)
summary(fitMC, refModels = mxRefModels(fitMC, run = TRUE))

## Contrast the two models, and campare with chi sq test in 2dn row in Table 21.6
anova(fitLC, fitMC)

## Get latent means and variances, and compare with "All measures" row in Table 21.6
estimates <- coef(fitLC)

latentMeans <- coef(fitLC)[c("aB", "aC")]; latentMeans
latentVar <- coef(fitLC)[c("dA", "dB", "dC")]; latentVar


## Section of Table 21.6 headed "Results for measures with invariant parameters"
# Sample statistics - get means and variances of 1st indicator for the 3 groups
dfY1 <- split(df$y1, df$x)
meansY1 <- do.call(cbind, lapply(dfY1, mean)); meansY1
varY1 <- do.call(cbind, lapply(dfY1, var)); varY1

# Note: means agree with means in Table 21.6, but variances don't.
# To get TLG variances, the denominator in the variance formula needs to be n, not (n-1)

# Get the n's
n <- do.call(cbind, lapply(dfY1, length)); n

# Adjust variance calculation
varY1 <- varY1 * (n - 1) / n; varY1   # All good


# Differences between y1 means
meansY1[2] - meansY1[1]
meansY1[3] - meansY1[1]

# These equal the latent means; compare with latent means
latentMeans

# Alternatively, the y1 intercepts (which are constrained to equality)
# added to the latent means give the y1 Means
intercepts <- coef(fitLC)["t1"]; intercepts
intercepts + latentMeans; meansY1


# Extract residual variances for y1 from estimates
residVarY1 <- coef(fitLC)[c("e1A", "e1B", "e1C")]   # Compare with 2nd row in Table 21.6

# Differences between y1 variances and y1 residual variances
varY1 - residVarY1

# These are latent variances - Compare with the latent variances
latentVar




## ANOVA model for 3rd row in Table 21.6
# Constrain 2nd loading to equality across groups
# and constrain 2nd intercept to equality across groups

## "Less Constrained" model

manifest <- c("y1", "y2", "y3", "y4")

# Factor loadings
loadsA <- mxPath(from = "LS", to = manifest, arrows = 1,
   free = c(TRUE, FALSE, TRUE, TRUE), values = 1,
   labels = c("l1A", "l2", "l3A", "l4A"))

loadsB <- mxPath(from = "LS", to = manifest, arrows = 1,
   free = c(TRUE, FALSE, TRUE, TRUE), values = 1,
   labels = c("l1B", "l2", "l3B", "l4B"))

loadsC <- mxPath(from = "LS", to = manifest, arrows = 1,
   free = c(TRUE, FALSE, TRUE, TRUE), values = 1,
   labels = c("l1C", "l2", "l3C", "l4C"))

# Factor variances - equal across groups
varFacA <- mxPath(from = "LS", arrows = 2,
   free = TRUE, values = 1, labels = "dA")

varFacB <- mxPath(from = "LS", arrows = 2,
   free = TRUE, values = 1, labels = "dB")

varFacC <- mxPath(from = "LS", arrows = 2,
   free = TRUE, values = 1, labels = "dC")

# Residual variances - equal across groups
varResA <- mxPath(from = manifest, arrows = 2,
   free = TRUE, values = 1, 
   labels = c("e1A", "e2A", "e3A", "e4A"))

varResB <- mxPath(from = manifest, arrows = 2,
   free = TRUE, values = 1, 
   labels = c("e1B", "e2B", "e3B", "e4B"))

varResC <- mxPath(from = manifest, arrows = 2,
   free = TRUE, values = 1, 
   labels = c("e1C", "e2C", "e3C", "e4C"))

# Intercepts - equal across groups
interceptsA <- mxPath(from = "one", to = manifest, arrows = 1,
   free = TRUE, values = 1,
   labels = c("t1A", "t2", "t3A", "t4A"))

interceptsB <- mxPath(from = "one", to = manifest, arrows = 1,
   free = TRUE, values = 1,
   labels = c("t1B", "t2", "t3B", "t4B"))

interceptsC <- mxPath(from = "one", to = manifest, arrows = 1,
   free = TRUE, values = 1,
   labels = c("t1C", "t2", "t3C", "t4C"))

# Factor mean - differ across the groups
meanA <- mxPath(from = "one", to = "LS", arrows = 1,
   free = FALSE, values = 0, labels = "aA")
meanB <- mxPath(from = "one", to = "LS", arrows = 1,
   free = TRUE, values = 1, labels = "aB")
meanC <- mxPath(from = "one", to = "LS", arrows = 1,
   free = TRUE, values = 1, labels = "aC")

# Setup the group models
modA <- mxModel("GrA", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataA, loadsA, varFacA, varResA, interceptsA, meanA)

modB <- mxModel("GrB", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataB, loadsB, varFacB, varResB, interceptsB, meanB)

modC <- mxModel("GrC", type = "RAM",
   manifestVars = manifest, latentVars = "LS",
   dataC, loadsC, varFacC, varResC, interceptsC, meanC)

# Combine the three models
fun <- mxFitFunctionMultigroup(c("GrA", "GrB", "GrC"))
modelLC <- mxModel("LC", modA, modB, modC, fun)

# Run the LC model and get the summary
fitLC <- mxRun(modelLC)

# Again, let OpenMx choose starting values
modelLC = mxAutoStart(modelLC)

fitLC <- mxRun(modelLC)
summary(fitLC, refModels = mxRefModels(fitLC, run = TRUE))
# All good

## More Consttained model
# Constraints
C1 <- mxConstraint(aB == 0)
C2 <- mxConstraint(aC == 0)

# Add them to less constrained model
modelMC <- mxModel(modelLC, C1, C2)
modelMC <- mxModel(modelMC, name = "MC")       # Change its name 

# Run the MC model and get the summary
fitMC <- mxRun(modelMC)
summary(fitMC, refModels = mxRefModels(fitMC, run = TRUE))

## Contrast the two models, and campare with chi sq test in 2nd row in Table 21.6
anova(fitLC, fitMC)

## Get latent means and variance, and compare with "All measures" row in Table 21.6
estimates <- coef(fitLC)

latentMeans <- coef(fitLC)[c("aB", "aC")]; latentMeans
latentVar <- coef(fitLC)[c("dA", "dB", "dC")]; latentVar


## Section of Table 21.6 headed "Results for measures with invariant parameters"
# Sample statistics - get means and variances of 1st indicator for the 3 groups
dfY2 <- split(df$y2, df$x)
meansY2 <- do.call(cbind, lapply(dfY2, mean)); meansY2
varY2 <- do.call(cbind, lapply(dfY2, var)); varY2

n <- do.call(cbind, lapply(dfY2, length)); n

# Adjust variance calculation
varY2 <- varY2 * (n - 1) / n; varY2   # All good

# Differences between y1 means
meansY2[2] - meansY2[1]
meansY2[3] - meansY2[1]

# These equal the latent means; compare with latent means
latentMeans

# Alternatively, the y1 intercepts (which are constrained to equality)
# added to the latent means give the y2 Means
intercepts <- coef(fitLC)["t2"]; intercepts
intercepts + latentMeans; meansY2

# Extract residual variances for y1 from estimates
residVarY2 <- coef(fitLC)[c("e2A", "e2B", "e2C")]   # Compare with 2nd row in Table 21.6

# Differences between y2 variances and y2 residual variances
varY2 - residVarY2

# These are the latent variances - Compare with the latent variances
latentVar
