
## Getting summary data (correlations, means, and standard deviations)
## from Table 1 (p. 50) of:

## Kurbanoglu, N. & Takunyaci, M. (2021). A structural equation modeling 
## on relationship between self-efficacy, physics laboratory anxiety
## and attitudes. Journal of Family, Counseling and Education, 6(1), 47-56.

### Using tabulapdf package ###

## First, set Java environment in current directory.
## Install rJavaEnv package (if not already installed),
## then run following r code:
##    rJavaEnv::java_quick_install(version = 21)
##
## Undo with: 
##    rJavaEnv::java_clear("project")
##    rJavaEnv::java_clear("installed")
##    rJavaEnv::java_clear("distrib")
##    rJavaEnv::java_env_unset()


##  Required package
library(tabulapdf)


## Table 1 is in Kurbanoglu.pdf

## extract_tables() (from tablaupdf) extracts a table from the given page.
## Table 1 is on page 4 of the pdf.

pdf <- "Kurbanoglu.pdf"
(tab <- extract_tables(pdf, pages = 4)[[1]])

## Returns a tibble as an element of a list of one. 
## [[1]] extracts the tibble from the list.
## The numeric parts of the table are in columns 2 to 4.
## However, the asterisks mean that columns 2 and 3 are character columns.


## Get means and standard deviations.
## Means are in row 4, columns 2 to 4.
## Standard deviations are in row 5, columns 2 to 4.
## In each, convert characters to numeric.
(means <- as.numeric(as.character(tab[4, 2:4])))
(sd <- as.numeric(as.character(tab[5, 2:4])))


## Get correlations.
## Correlations are in row 1 to 3, columns 2 to 4.
(cor <- tab[1:3, 2:4])


## Convert tibble to vector, row-wise;
## Drop the NAs;
## Drop the asterisks;
## convert to numeric.
(cor <- c(t(cor)))
(cor <- cor[!is.na(cor)])
(cor <- gsub("\\*", "", cor))
(cor <- as.numeric(as.character(cor))) 
