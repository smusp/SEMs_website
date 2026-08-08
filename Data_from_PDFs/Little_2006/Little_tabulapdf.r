
## Getting summary data (correlations, means, and standard deviations)
## Appendix A (pp. 71-72) of:

## Little, T., Slegers, D., & Card, N. (2006). A non-arbitrary method of 
## identifying and scaling latent variables in SEM and MACS models. 
## Structural Equation Modeling, 13(1), 59-72.

### Using tabulapdf package ###


## Appendix A is in Little.pdf.

## First, set Java environment in current directory
## Install rJavaEnv package (if not already installed),
## then run following r code:
##    rJavaEnv::java_quick_install(version = 21)


##  Load required packages
library(tabulapdf)

## Get the summary data.
## There is no "table" in the Appendix,
## and as a result, extract_tables() will return an empty list.
## Use extract_text() instead.
## Select the pages containing Appendix A - pages 13 and 14 of the pdf.

## strsplit() splits the string at the line breaks - \r\n
pdf = "Little.pdf"
tab <- extract_text(pdf, pages = c(13,14))
(tab <- unlist(strsplit(tab[c(1,2)], split = "\\r\\n")))

## Select the relevant rows
(tab <- tab[c(41, 30, 32, 34:39, 58, 60, 62:67)])


## The steps are more-or-less the same as with the pdftools package. 
## strsplit() - each line becomes an element in a list, 
## and each string is split at the space into separate strings.
(tab <- strsplit(tab, split = " "))


## Variable names in element 1
names <- tab[[1]]
(names <- gsub("AFF", "", names))


## Means in elements 2 and 10
means <- tab[c(2, 10)]
means <- unlist(means)
means <- as.numeric(as.character(means))
(means <- split(means, cut(seq_along(means), 2, labels = c("Grade 7", "Grade 8"))))


## Standard Deviations in elements 3 and 11
sd <- tab[c(3, 11)]
sd <- unlist(sd)
sd <- as.numeric(as.character(sd))
(sd <- split(sd, cut(seq_along(sd), 2, labels = c("Grade 7", "Grade 8"))))


## Correlations in elements 4 to 9, and elements 12 to 17
cor <- tab[c(4:9, 12:17)]
cor <- lapply(cor, function(x) gsub("—", "-", x))
cor <- unlist(cor)
cor <- as.numeric(as.character(cor))
(cor <- split(cor, cut(seq_along(cor), 2, labels = c("Grade 7", "Grade 8"))))
