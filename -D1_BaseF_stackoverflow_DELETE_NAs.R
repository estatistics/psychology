#
# Removing so many rows with NAs in a dataset. 
# How many rows keep with NAs data in a dataset: 0 rows with NAs; 1; 2; 3; 
#
# Function found on Stackoverflow # 4862178 by "Pierre Lafortune"
#

delete.na <- function(df, n=0) {
  df[ rowSums(is.na( df )) <= n, ]
}
