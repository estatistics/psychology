##########################################################
#### Description                                     #####
##########################################################

# It requires csv files in a directory that are 
# Crosstabulation tables produced from open source statistical program PSPP
# Multiple tables may miss a category, not aligned vertically, in the same column
# This function it tidy up tables to be aligned vertically, with same category 
# in the same column.


##########################################################
#### Dependencies                                   #####
##########################################################

# Depends on Base R / stats only



##########################################################
#### Example DF                                   #####
##########################################################

# SEE at the end of this file


##########################################################
####Arguments of the Function                       #####
##########################################################

# path_of_csvs:  The path where the Crosstabulation Tables from PSPP 
#                are found e.g. "/home/crosstabs_csvs/"


##########################################################
####  The Function                              #####
##########################################################


path="/home/elias/Desktop/erg/argyris/crosstabs_02/"


crosstabs_same_horizontal_categories <- function( path_of_csvs ) {
  
  
  
  #### Making an operator
  # Found on stackoverflow "5831794"
  '%!in%' <- Negate(`%in%`)
  
  
  # Found on stackoverflow: 11433432
  # Reading multiple csv files
  pth        = path_of_csvs
  dirs_tmp   = paste0(pth, list.files(pth, pattern = "*.csv" ) )
  csv_files  = lapply(dirs_tmp, function(x) read.csv(x,  header = FALSE, sep = ",", quote = "" ) )
  
  # Separating Crosstable tables / Names / Pearsons statistics 
  
  rbind_cross <- list()
  rbind_chsq  <- list()
  test_nm  <- list()
  var_names<- list()
  for ( d in 1:length( csv_files ) ) {
    
    crosstbs <- csv_files[[d]]
    crosstbs$id <- 1:(dim( crosstbs )[1])
    
    tbl_st   <- crosstbs[crosstbs$id[ grepl(x=crosstbs[[1]], "total") ] + 2, ] 
    tbl_fn   <- crosstbs[crosstbs$id[ grepl(x=crosstbs[[1]], "Total") ], ]
    test_nm0 <- crosstbs[crosstbs$id[ grepl(x=crosstbs[[1]], " \\* ") ], ]
    test_nm  <- test_nm0[test_nm0$id %!in% test_nm0[grep(x=test_nm0[[1]], "tot"),]$id, ]
    test_id  <- test_nm0[test_nm0$id %in% test_nm0[grep(x=test_nm0[[1]], "tot"),]$id, ]$id+1
    cq_ln    <- crosstbs[crosstbs$id[ grepl(x=crosstbs[[1]], "-Square") ], ]
    
    st_pos  <- 0
    fn_pos  <- unlist( gregexpr(pattern =" \\*", test_nm[[1]]) )
    # nchar(as.matrix( test_nm[1,][[1]][1] ))
    
    tables_ln <- list()
    nm_tbls   <- list()
    chsq_lns  <- list()
    
    for( i in 1:dim( tbl_st )[1] ) {
      crosstbs[tbl_fn$id[i], ] <-""
      tables_ln[[i]]           <- crosstbs[tbl_st$id[i]:tbl_fn$id[i], ]  
      nm_tbls[[i]]             <- tables_ln[[i]][1,]
      chsq_lns[[i]]            <- crosstbs[cq_ln$id[i], ]  
      
    }
    
    ### ==========================================###
    ###   The rbind_null_df_lists function  ###
    ### ========================================= ###
    
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
      
      
      if( names_list != "") {
        
        colnames( df_cbind )   <- names_list
        
      }
      
      
      df_cbind
      
    }
    
    
    
    #### Rbinding the datasets 
    var_names[[d]]   <- trimws(substr( test_nm[[1]], st_pos, fn_pos))
    
    rbind_cross[[d]] <- rbind_null_df_lists( tables_ln )
    rbind_chsq[[d]]  <- rbind_null_df_lists( chsq_lns )
  }
  
  
  ####  Adding names to the dataset
  names( rbind_cross )  <- var_names
  names( rbind_chsq )   <- var_names
  
  
  ### Finding the dataset with the whole "label" categories
  lgth_list     <-  length( rbind_cross )
  rlist_labels  <-  lapply( 1:lgth_list, function(x)  rbind_cross[[x]][1,][ rbind_cross[[x]][1,] != ""] )
  lbls_unique   <-  unique( unlist( rlist_labels ) )
  rlist_length  <-  unlist( lapply( 1:lgth_list, function(x)  length( rbind_cross[[x]][1,][ rbind_cross[[x]][1,] != ""]) ) )
  max_lbls      <-  length( lbls_unique )
  seq_max       <-  1:max_lbls
  
  # rlist_max_lg  <-  which(max( rlist_length ) == rlist_length )[1]
  # rlist_max_labels <-  rlist_labels[[ rlist_max_lg ]]
  # rl_max  <-  rlist_max_labels
  
  # Aligning the variables / columns into the right position 
  # Finding the incorrect position ( pos_st )
  # Finding the correct position( pos_orig )
  pos_orig       <- rbind_null_df_lists( lapply(1:lgth_list, function(a) na.omit(match(as.matrix(rbind_cross[[a]][1,] ), lbls_unique  ))) )
  pos_st         <- rbind_null_df_lists( lapply(1:lgth_list, function(b) unlist(lapply(1:length(lbls_unique), function(y) match(lbls_unique[[y]], rlist_labels[[b]]) )) ))
  
  
  # Aligning columns / variables
  rdt_alignd <- list()
  for (i in 1:lgth_list ) {
    
    rc              <- rbind_cross
    rdt_alignd[[i]] <- cbind(rbind_cross[[i]]$id, rc[[i]][ na.omit(pos_st[i,]) ])
    
    
  }
  
  
  horizontally_aligned   <- do.call(cbind, rdt_alignd )
  vertically_aligned     <- rdt_alignd
  
  
  # Creating an empty file for vertical-type format table
  file.create( paste0(pth, "rbind_cross.csv") )
  
  # Appending the results for vertical-type format table
  lapply( 1:lgth_list, function(x)write.table( vertically_aligned[[x]],  append = TRUE, paste0(pth, "rbind_cross.csv") ) )
  
  # Creating an empty file for horizontal-type format table
  file.create( paste0(pth, "cbind_cross.csv") )
  
  # Appending the results for horizontal-type format table
  write.table( horizontally_aligned,  append = TRUE, paste0(pth, "cbind_cross.csv") )
  
  # Creating an empty file for Pearsons's square results
  file.create( paste0(pth, "pearson_cross.csv") )
  
  # Appending the resultsfor Pearsons's square results
  write.table( rbind_chsq,  append = TRUE, paste0(pth, "pearson_cross.csv") )
  
}




##########################################################
####           Running  The Example                  #####
##########################################################



#### Downloading example crosstab csv files from github of "estatistics/psychology"
#### !!! please uncomment the following lines in order to run the example !!!


# require(RCurl)
# crosstabs_01 <-read.csv(text=getURL("https://raw.githubusercontent.com/estatistics/psychology/master/crosstabs_01.csv" ), 
#                         skip=0, header=F,  sep = "," )
# crosstabs_02 <-read.csv(text=getURL("https://raw.githubusercontent.com/estatistics/psychology/master/crosstabs_02.csv"), 
#                         skip=0, header=F,  sep = ","  )
# 
# 
# 
# Declare the path to save example csv files
# For window users, please change it accordingly...
# path_of_csvs <- "/tmp/"
# 
# 
# # writing the files on the specified path
# write.table( crosstabs_01,  append = F, paste0(path_of_csvs, "cross_01.csv"), sep = "," )
# write.table( crosstabs_02,  append = F, paste0(path_of_csvs, "cross_02.csv"), sep = "," )



##### Please uncomment the below line/s in order to run the example 

# crosstabs_same_horizontal_categories ( path_of_csvs )



