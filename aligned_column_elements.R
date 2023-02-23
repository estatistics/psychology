df <- list(structure(list(ID_no = c(2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 
                                    4L), Variable = c("practice", "", "", "usefullness", "", "", 
                                                      "stress", "", ""), Levels = structure(c(3L, 4L, 5L, 3L, 4L, 5L, 
                                                                                              3L, 4L, 5L), .Label = c("female", "male", "maybe", "no", "yes"
                                                                                              ), class = "factor"), Frequency = c(7, 4, 4, 2, 11, 2, 3, 6, 
                                                                                                                                  6), Percentage = c(46.67, 26.67, 26.67, 13.33, 73.33, 13.33, 
                                                                                                                                                     20, 40, 40), y_place = c(76.5, 39.5, 13, 92.5, 49.5, 6.5, 90, 
                                                                                                                                                                              60, 20)), .Names = c("ID_no", "Variable", "Levels", "Frequency", 
                                                                                                                                                                                                   "Percentage", "y_place"), row.names = 3:11, class = "data.frame"))


ID_no    Variable Levels Frequency Percentage y_place
3      2    practice  maybe         7      46.67    76.5
4      2                 no         4      26.67    39.5
5      2                yes         4      26.67    13.0
6      3 usefullness  maybe         2      13.33    92.5
7      3                 no        11      73.33    49.5
8      3                yes         2      13.33     6.5
9      4      stress  maybe         3      20.00    90.0
10     4                 no         6      40.00    60.0
11     4                yes         6      40.00    20.0


Aligned_Column_Join <- function(df, column_1, column_2) {
  
  # Producing some spaces in order to make nice looking printed datasets
  df_col1      <- df[,column_1]
  count_chars      <- nchar(as.character(df_col1))
  max_count      <- max( count_chars )
  spaces      <- unlist( lapply( max_count - count_chars, function(x) paste0(if (x==0) { "" } else { rep(" ", x) }, collapse="") ))
  df_aligned_col1    <- paste0( spaces, df_col1 ) 
  
  df_col2       <- df[,column_2]
  count_chars      <- nchar(as.character(df_col2))
  max_count      <- max( count_chars )
  spaces      <- unlist(lapply( max_count - count_chars, function(x) paste0(if (x==0) { "" } else { rep(" ", x) }, collapse="") ) )
  df_aligned_col2    <- paste0(spaces, df_col2 ) 
  
  
  # Frequency and percentage columns made together
  aligned_elements <-    unname(as.data.frame(
    paste(df_aligned_col1, " (",  
          df_aligned_col2, ")" ))) 
  
  
  aligned_elements
}