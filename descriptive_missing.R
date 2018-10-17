
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

# It is recomended to use the Dataframes produced 
# by rbind_list_dfs_no_id.R 

#### NOTE ###
# You may need " rbind_list_dfs_no_id.R " 
# in order to reproduce the example


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
list_of_df         <- apply(ex_df=="NA", 2, ( table) )

# Merged dataframe of missing values 
# made into list 
df_missing_list    <-  list( rbind_null_df_lists ( list_of_df ) )


##########################################################
####            Arguments of the Function            #####
##########################################################

# A list of dataframes produced by " rbind_list_dfs_no_id.R " 
# See example 


##########################################################
####           The Function                          #####
##########################################################


descriptive_missing <- function (  df_missing_list_df  ) {

   max_df   <- list()
   for (i in 1:length( df_missing_list_df ) ) {
    
      max_df[[i]] <-   max( (round( df_missing_list_df[[i]][,2]  / df_missing_list_df[[i]][,1], 3) *100), na.rm= TRUE) 
    }
   
  descriptive_missing_df  <- do.call(rbind, max_df ) 
  descriptive_missing_df
  colnames( descriptive_missing_df ) <- "perc"
  rownames( descriptive_missing_df ) <- paste0("df_",seq(1:length( df_missing_list_df ))  )
  
  descriptive_missing_df
}


##########################################################
####           Running  The Example                  #####
##########################################################

# Please uncomment the below line/s in order to run the example 

# descriptive_missing ( df_missing_list  ) 

