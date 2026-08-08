
## Getting summary data (covariances, variances, and means) for combined group
## from Table 5.1 (p. 169) of:

## Thompson, M. & Green, S. (2013). Evaluating between-group differences
## in latent variable means. In G. Hancock & R. Mueller (Eds.), Structural
## equation modeling: A second course (2nd Ed., pp. 163-218). Charlotte, NC:
## Information Age Publishing.


### Using tabulapdf package ###
# I don't use this data - 
# the purpose is to show that tabulapdf struggles here.

##  Load required packages
library(tabulapdf)


## Table 5.1 is contained in Thompson.pdf.
## The combined data is in the lower part of Table 5.1.

## Get the data table
pdf <- "Thompson.pdf"
tab <- extract_tables(pdf, pages = 7)[[1]]
print(tab, n = 30)

## Note that in the 'combined' section,
## the row containing V7 has not been extracted.
## I could use locate_areas(). When I do, on my machine, the numbers in the 
## resulting table have been rounded up. 

locate_areas(pdf, widget = "native")
tab <- extract_tables(pdf, guess = FALSE,
   area = list(c(334.60719,  58.82145, 411.75006, 365.46436)))[[7]]
print(tab)
