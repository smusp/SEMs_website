
## Getting summary data (correlations, means, and standard deviations)
## from Table 1 (p. 50) of:

## Kurbanoglu, N. & Takunyaci, M. (2021). A structural equation modeling 
## on relationship between self-efficacy, physics laboratory anxiety
## and attitudes. Journal of Family, Counseling and Education, 6(1), 47-56.

### Using pdftools package ###

## Required package
library(pdftools)


## Table 1 is in Kurbanoglu.pdf

## pdf_text() (from pdftools package) creates a vector with one string per page.
## Select the page containing Table 1 - the 4th page of the pdf.

## strsplit() splits the string at the line breaks to create a vector with 
## one string for each line (note: split is a regex - it searches for one or more line breaks).
##
## Select the lines that contain Table 1, or rather the part of Table 1
## that contains the correlations, means, and standard deviations - select lines 13 to 17.
pdf <- "Kurbanoglu.pdf"
(tab <- pdf_text(pdf)[4])
(tab = unlist(strsplit(tab, split = "\\n+")))
(tab <- tab[13:17])


## Two regexes
## First, for each string, drop the text and the white space before the table elements.
## The regex searches: 
##    ^  from the beginning of the string;
##    .+  for any character any number of times;
##    \\s{25,}  followed by at least 25 spaces.
(tab <- gsub("^.+\\s{25,}", "", tab))


## Second, for each string, reduce white space inside each string to just one space.
## The second regex searches:
##    \\s+  for one or more spaces.
(tab <- gsub("\\s+", " ", tab))


## strsplit()  - each string becomes an element in a list, 
## and each element is split at the space into separate strings.
(tab <- strsplit(tab, split = " "))


## Get means
## Means are in the 4th element of the list.
## Convert strings to numeric.
(means <- as.numeric(as.character(tab[[4]])))


# Get standard deviations
## Standard deviations are in the 5th element of the list.
## Convert strings to numeric.
(sd <- as.numeric(as.character(tab[[5]])))


## Get correlations
## Correlations are in the first three elements of the list.
## Unlist the three elements,
## remove the asterisks,
## convert the strings to numeric.
cor <- unlist(tab[c(1:3)])
cor <- gsub("\\*", "", cor) 
(cor <- as.numeric(as.character(cor)))
