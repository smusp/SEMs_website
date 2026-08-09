
## Getting means and standard deviations from Table 1 (p. 228) 
## and correlations from Table 2 (p. 229) of:

## Lansford, E., Criss, M., Laird, R., Shaw, D., Pettit, G., Bates, J., 
## & Dodge, K. (2011). Reciprocal relations between parents' physical 
## discipline and children's externalizing behavior during middle childhood
## and adolescence. Development and Psychopathology, 23, 225–238.


### Using pdftools package ###

## Required package
library(pdftools)


## The tables are in Lansford.pdf.

## pdf_text() (from pdftools package) creates a vector with one string per page.
## Select the pages with Table 1 (means and standard deviations) and Table 2 (correlations) - 
## the 4th and 5th pages of the pdf.
## Then select the 1st string, containing Table 1
##
## strsplit() splits the string at the line breaks to create a vector with 
## one string per line.
## Select the lines that contain Table 1
pdf <- "Lansford.pdf"
(tab <- pdf_text(pdf)[c(4,5)])
(tab1 = tab[1])
(tab1 = unlist(strsplit(tab1, split = "\\n+")))
(tab1 <- tab1[c(8:11, 13, 15:17)])

## Two regexes
## First, for each string, drop the text, including variable names and the white space 
## before the table elements.
## The regex searches: 
##    ^  from the beginning of the string;
##    .+  for any character any number of times;
##    \\s{20,}  followed by at least 20 spaces.
(tab1 <- gsub("^.+\\s{20,}", "", tab1))


## Second, for each string, reduce white space inside each string to just one space.
## The second regex searches:
##    \\s+  for one or more spaces.
(tab1 <- gsub("\\s+", " ", tab1))


## strsplit()  - each line becomes an element in a list, 
## and each string is split at the space into separate strings.
(tab1 <- strsplit(tab1, split = " "))


## Means are 2nd in each list element.
## Select the 2nd element in each list element using sapply()
## convert strings to numeric.
means <- sapply(tab1, "[[", 2)
(means <- as.numeric(as.character(means)))

## Standard deviations are 3rd in each list element.
## Select the 3rd element in each list element using sapply()
## convert strings to numeric.
sd <- sapply(tab1, "[[", 3)
(sd <- as.numeric(as.character(sd)))


## Create variable names (before I forget)
(names <- c(paste0("PD", 6:9), paste0("EXT", 6:9)))


## Correlations
## Remember: tab contained two strings.
## Select the string with Table 2 (correlations) - the 2nd string.
## strsplit() creates a vector with one string per line.
## Select the lines that contain Table 2 (I don't need the demographic characteristics). 
(tab2 = tab[2])
(tab2 <- unlist(strsplit(tab2, split = "\\n+")))
(tab2 <- tab2[c(5:8, 10:13)])


## Two regexes
## First, for each string, drop the text, including variable names and the white space 
## before the table elements.
## The regex searches: 
##    ^ from the beginning of the string;
##    .+ for any character any number of times;
##    \\s{25,} followed by at least 20 spaces.
(tab2 <- gsub("^.+\\s{20,}", "", tab2))


## Second, for each string, reduce white space inside each string to just one space.
## The second regex searches:
##    \\s+  for one or more spaces.
(tab2 <- gsub("\\s+", " ", tab2))


## strsplit() - each line becomes an element in a list, 
## and each string is split at the space into separate strings.
(tab2 <- strsplit(tab2, split = " "))


## Strip off the last three elements in each line (don't need demographics)
## Unlist,
## drop the asterisks,
## convert the strings to numeric.
cor <- lapply(tab2, function(x) {
       len = length(x)
	  i = c(len - 2, len - 1, len)
	  x[-i]
	  })
(cor <- unlist(cor))
cor <- gsub("\\*+", "", cor) 
(cor <- as.numeric(as.character(cor)))


## Get the correlation matrix
#  The vector of correlations is the upper triangle of correlations,
#  entered row by row.
#  The vector is also the lower triangle entered column by column.
#  But without the diagonal.
#  I want lower triangle row by row, plus the diagonal of ones.

#  Therefore, setup an empty matrix,
#  Feed in the correlation matrix, in the 'lower.tri' position;
#  But there are no diagonal elements - therefore 'diag = FALSE'.
#  Fill in the upper triangle.
#  Finally, put ones along the diagonal.
mcor <- matrix( , 8, 8)                           # Empty matrix
mcor[lower.tri(mcor, diag = FALSE)] <- cor        # Fill the lower triangle
mcor <- pmax(mcor, t(mcor), na.rm = TRUE)         # Fill the upper triangle
diag(mcor) = 1                                    # Ones along the diagonal 

# The upper triangle (column by column) is the required lower triangle (row by row).
# Extract the upper triangle, along with the diagonal.
vcor <- mcor[upper.tri(mcor, diag = TRUE)]


## OR Get variance-covariance matrix
(mcov <- outer(sd, sd) * mcor)

# Name the rows and columns
dimnames(mcov) <- list(names, names); mcov
