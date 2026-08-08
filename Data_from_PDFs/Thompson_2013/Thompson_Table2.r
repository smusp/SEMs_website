
## Getting summary data (covariances, variances, and means) for each of two groups
## from Table 5.2 (p. 183) of:

## Thompson, M. & Green, S. (2013). Evaluating between-group differences
## in latent variable means. In G. Hancock & R. Mueller (Eds.), Structural
## equation modeling: A second course (2nd Ed., pp. 163-218). Charlotte, NC:
## Information Age Publishing.


### Using pdftools package ###

## Load required package
library(pdftools)


## Table 5.2 is conained in Thompson.pdf.

## pdf_text() (from pdftools package) creates a vector with one string per page.
## Select the page containing Table 5.2 - the 21st page of the pdf.

## strsplit() splits the string at the line breaks to create a vector with 
## one string per line.
## Select the lines that contain the table.
## Grouped data are in lines 7 to 12, and lines 14 to 19.
## Note that the covariance matrix contains the lower triangle of the 
## covariances with the variances along the diagonal.
## The means are to the right.  
pdf <- "Thompson.pdf"
(tab <- pdf_text(pdf)[21])
(tab <- unlist(strsplit(tab, "\\n+")))
(tab <- tab[c(7:12, 14:19)])


## For each line, trim off the leading white space, and
## reduce internal white space to a single space.
## strsplit() - each line becomes an element in a list, 
## and each string is split at the space into separate strings.
tab <- trimws(gsub("\\s+", " ", tab))
(tab <- strsplit(tab, split = " "))


## Variable names are the first element in each line.
## Select the first element in each line - each line is an element 
## of the list, so lapply() will do the selecting.
## Unlist the list, 
## select the first 6 names.
names <- lapply(tab, "[[", 1)
names <- unlist(names)
(names <- names[1:6])


## Means are the last element in each line.
## Select the last element in each line (list element) using lapply()
## Unlist the list,
## convert characters to numeric,
## split the vector into a two-element list:
## the 1st element contains means for Group 1,
## the 2nd contains means for group 2.
means <- lapply(tab, function(x) x[length(x)])
means <- unlist(means)
means <- as.numeric(as.character(means))
(means <- split(means, cut(seq_along(means), 2, labels = c("Day-care", "Home-care"))))


## Variances/Covariances
## First strip off the names and the means.
## Unlist the list,
## convert characters to numeric,
## split the vector into a two-element list:
## The 1st contains the (co)variances for Group 1,
## the 2nd contains (co)variances for Group 2.
var <- lapply(tab, function(x) x[-length(x)])
var <- lapply(var, function(x) x[-1])
var <- unlist(var)
var <- as.numeric(as.character(var))
(var <- split(var, cut(seq_along(var), 2, labels = c("Day-care", "Home-care"))))





### Using tabulapdf package ###

## First, set Java environment in current directory
## Install rJavaEnv package (if not already installed),
## then run following r code:
##    rJavaEnv::java_quick_install(version = 21)


##  Load required packages
library(tabulapdf)


## Get the data table
pdf <- "Thompson.pdf"
tab <- extract_tables(pdf, pages = 21)[[1]]
print(tab, n = 40)
# Note: Tables 5.1 and 5.2 have identical layouts, 
# yet for Table 5.1, extract_tables() combined everything into one column; 
# and here with Table 5.2, extract_tables() combined the first two columns into one column,
# then after that, separate columns, including a column of 'NAs'.
# 
# I'll not persevere with tabulapdf.  I'm happy with pdftools.
