
## LowerTri - Get lower triangle of correlations or (co)variances from vector
## vector - vector of correlations or (co)variances. 
## Assumes lower triangle of correlations or (co)variances.
## Assumes ones along the diagonal (correlations)
## or variances along the diagonal ((co)variances).
##
## n_variables - number of variables - 3
##
## n_decimals - number of decimal places
##
## n_char - Number of characters; 
## for instance, -0.42 has 5 characters, plus one character for a space, 
## giving a total of 6 characters. 

LowerTri <- function(vector, n_variables, n_decimals, n_char) {
k <- 0

paste(
cat("c(\n"),

for (i in 1:n_variables)
  {
  for (j in 1:i)
    {
    k <- k + 1
    if (k < length(vector)) { 
    cat(sprintf(paste0("%*.", n_decimals, "f,"), n_char, vector[k]))
    } else {
    cat(sprintf(paste0("%*.", n_decimals, "f"), n_char, vector[k]))
    }
    }
  cat("\n")
  }, 
  
cat(")\n")
)
}

