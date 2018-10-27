
##########################################################
####             Description                        #####
##########################################################

# A function to rbind list of dataframes 
# with no equal columns
# and with no common id value


##########################################################
####             Dependencies                        #####
##########################################################

# Depends on Base R only


##########################################################
####             Example DF                          #####
##########################################################

# Example df
ex_df           <- cbind(c( seq(1, 10, 1), rep("NA", 0), seq(1,10, 1) ), 
                         c( seq(1, 7, 1),  rep("NA", 3), seq(1, 12, 1) ), 
                         c( seq(1, 3, 1),  rep("NA", 7), seq(1, 5, 1), rep("NA", 5) ))

# Making colnames and rownames
colnames(ex_df) <- 1:dim(ex_df)[2]
rownames(ex_df) <- 1:dim(ex_df)[1]

# Making an unequal list of dfs, 
# without a common id column
list_df      <- apply(ex_df=="NA", 2, ( table) )

##########################################################
####            Arguments of the Function            #####
##########################################################

# A list of dataframes 


##########################################################
####             The function                        #####
##########################################################

# The function to rbind it
rbind_null_df_lists <- function ( list_of_dfs ) {
  
  # check code
  # tables_ln
  # list_of_dfs <-   list_df 

  
  if ( length(names(list_of_dfs)) == 0 ) {
    
    names(list_of_dfs) <- 1:length(list_of_dfs )
    length_df     <- do.call(rbind, (lapply( list_of_dfs, function(x) length(x))))
    max_no        <- max(length_df[,1])
  
    if ( length(names(length_df)) == 0 ) {

       names_list<- ""
    }

    if ( length(names(length_df)) != 0 ) {

    max_df        <- length_df[max(length_df),]
    name_df       <- names(length_df[length_df== max_no][1])
    names_list    <- names(list_of_dfs[ name_df][[1]])
    
   }
    
  }

  
   
  if ( length(names(list_of_dfs)) != 0 ) {
    length_df     <- do.call(rbind, (lapply( list_of_dfs, function(x) length(x))))
    max_no        <- max(length_df[,1])
   
     if ( length(names(length_df)) == 0 ) {
      
       names_list<- ""
    }
    
    if ( length(names(length_df)) != 0 ) {
      
      max_df        <- length_df[max(length_df),]
      name_df       <- names(length_df[length_df== max_no][1])
      names_list    <- names(list_of_dfs[ name_df][[1]])
      
    }
    
  }
  
  
  df_dfs <- list()
  for (i in 1:max_no ) {
    
    df_dfs[[i]]            <- do.call(rbind, lapply(1:length(list_of_dfs), function(x) list_of_dfs[[x]][i]))
    
  }
  
  
  df_cbind               <- do.call( cbind, df_dfs )
  
  if ( length(rownames( df_cbind ) ) !=   length(rownames (length_df) ))  {
    
    rownames (length_df) <- rownames (length_df)
  }
  
  if ( length(rownames( df_cbind ) ) ==  length(rownames (length_df) ))  {
    
    rownames( df_cbind )   <- rownames (length_df)
  }
  
  
  if( names_list != "" ) {
    
    colnames( df_cbind )   <- names_list
    
  }
  
  
  df_cbind
  
}



##########################################################
####             Running the example                 #####
##########################################################

# Uncomment the below line in order to run the example 

# rbind_null_df_lists ( list_of_df )
