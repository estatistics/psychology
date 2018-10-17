##########################################################
####             Dependencies                        #####
##########################################################

# Depends on Base R only

##########################################################
####             Example DF                          #####
##########################################################

ex_df           <- cbind(c( seq(1, 10, 1), rep("NA", 0), seq(1,10, 1) ), 
                         c( seq(1, 7, 1),  rep("NA", 3), seq(1, 12, 1) ), 
                         c( seq(1, 3, 1),  rep("NA", 7), seq(1, 5, 1), rep("NA", 5) ))

# Making colnames and rownames
colnames(ex_df) <- 1:dim(ex_df)[2]
rownames(ex_df) <- 1:dim(ex_df)[1]

# Making an unequal list of dfs, 
# without a common id column
list_of_df      <- apply(ex_df=="NA", 2, ( table) )

##########################################################
####             The function                        #####
##########################################################

# The function to rbind it
rbind_null_df_lists <- function ( list_of_dfs ) {
  
  length_df     <- do.call(rbind, (lapply( list_of_dfs, function(x) length(x))))
  max_df        <- length_df[max(length_df),]
  name_df       <- names(max_df)
  names_list    <- names(list_of_dfs[ name_df][[1]])
  
  df_dfs <- list()
  for (i in 1:max_df[[1]] ) {
    
    df_dfs[[i]]            <- do.call(rbind, lapply(1:length(list_of_dfs), function(x) list_of_dfs[[x]][i]))
    
  }
  
  df_cbind               <- do.call( cbind, df_dfs )
  rownames( df_cbind )   <- rownames (length_df)
  colnames( df_cbind )   <- names_list
  
  df_cbind
  
}
  
##########################################################
####             Running the example                 #####
##########################################################

# rbind_null_df_lists ( list_of_df )

