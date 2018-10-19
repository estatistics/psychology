
##########################################################
####             Description                         #####
##########################################################

# A function that summarize the maximum 
# misising values percentage per dataset
# in a single dataframe 

##########################################################
####             Dependencies                        #####
##########################################################

# Depends on Base R only


##########################################################
####             Example DF                          #####
##########################################################

# Example df
ex_df           <- cbind(c( seq(1, 10, 1), rep(NA, 0), seq(1,10, 1) ), 
                         c( seq(1, 7, 1),  rep(NA, 3), seq(1, 12, 1) ), 
                         c( seq(1, 3, 1),  rep(NA, 7), seq(1, 5, 1), rep(NA, 5) ))

# Making colnames and rownames
colnames(ex_df) <- 1:dim(ex_df)[2]
rownames(ex_df) <- 1:dim(ex_df)[1]


##########################################################
####            Arguments of the Function            #####
##########################################################

# Datasets given as list
# e.g. list ( df ) or list ( df1, df2, df3 )


##########################################################
####           The Function                          #####
##########################################################


descriptive_missing <- function (  df_missing_list_df  ) {
  
  # ### Test section
  df_missing_list_df <- list( ex_df )
  

  # rbind_null_df_lists function is included
  rbind_null_df_lists <- function ( list_of_dfs ) {
    
    length_df     <- do.call(rbind, (lapply( list_of_dfs, function(x) length(x))))
    max_no        <- max(length_df[,1])
    max_df        <- length_df[max(length_df),]
    name_df       <- names(length_df[length_df== max_no,][1])
    names_list    <- names(list_of_dfs[ name_df][[1]])
    
    df_dfs <- list()
    for (i in 1:max_no ) {
      
      df_dfs[[i]]            <- do.call(rbind, lapply(1:length(list_of_dfs), function(x) list_of_dfs[[x]][i]))
      
    }
    
    df_cbind               <- do.call( cbind, df_dfs )
    rownames( df_cbind )   <- rownames (length_df)
    colnames( df_cbind )   <- names_list
    
    df_cbind
    
  }
  
  
  # Lists
  var_max_df      <- list()
  case_max_p_df   <- list()
  var_max_p_df    <- list()
  
  # the actual function 
  for (i in 1:length( df_missing_list_df ) ) {
    
    # Variable-wise missing stats
    var_perc_df            <- rbind_null_df_lists( apply(is.na(df_missing_list_df[[i]]),  2, ( table) ) )
    var__miss              <- (round( var_perc_df[,2]  / sum(var_perc_df[[1]]), 3) *100)
    var_max_df             <- var__miss[which (var__miss == max(var__miss, na.rm=TRUE))]
    var_max_p_df[[i]]      <- var_max_df[[1]]
    names( var_max_p_df )  <- paste0( names( var_max_df ), collapse="," )
    
    # Case-wise missing stats
    case_perc_df           <- rbind_null_df_lists( apply(is.na(df_missing_list_df[[i]]),  1, ( table) ) )
    case__miss             <- (round( case_perc_df[,2]  / sum(case_perc_df[[1]]), 3) *100)
    case_max_df            <- case__miss[which (case__miss == max(case__miss, na.rm=TRUE))]
    case_max_p_df[[i]]     <- case_max_df[[1]]
    names( case_max_p_df ) <- paste0( names( case_max_df ), collapse="," )

  }
  
  
  
  var_missing_df          <- do.call( rbind, var_max_p_df ) 
  case_missing_df         <- do.call( rbind, case_max_p_df ) 
  
  descriptive_missing_df             <- cbind( var_missing_df, case_missing_df, rownames(var_missing_df),  rownames(case_missing_df) )
  colnames( descriptive_missing_df ) <- c( "max_var_miss", "max_case_miss", "vars", "cases") 
  rownames( descriptive_missing_df ) <- paste0( "df_", seq(1:length( df_missing_list_df ))  )
  descriptive_missing_df
}


##########################################################
####           Running  The Example                  #####
##########################################################

# Please uncomment the below line/s in order to run the example 

# descriptive_missing ( ex_df  ) 
