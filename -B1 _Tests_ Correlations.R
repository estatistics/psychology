corrDF=per2[-1]
corrmethod="pearson"
corradjust="holm"
pvalue=.10
round_digits=2
r_value = .200 # = 0 - No replace
replace_rvalue ="^"
diag_which = 1
replace_low=""
replace_diag=""
numbered_horiz_Labels =1
del_empty_cols = 1
write_results = 1
pth_filename = paste0(pth, "datacsv_perioxi_2.csv")


library(psych)


# corr_f(corrDF=per2[-1],              corrmethod="pearson",       corradjust="holm",
#        pvalue=.10,                   round_digits=2,             r_value = .100,
#        replace_rvalue ="^",          diag_which = 1,             replace_low="",       replace_diag="",
#        numbered_horiz_Labels =1,     del_empty_cols = 1,          write_results = 0,
#        pth_filename = paste0(pth, "rdata_corrs.csv") )


cc <- corr_f( corrDF, corrmethod, corradjust,  pvalue, round_digits, r_value, replace_rvalue,
              diag_which, replace_low, replace_diag, numbered_horiz_Labels, del_empty_cols, 
              write_results, pth_filename  )


corr_f <- function( corrDF, corrmethod, corradjust, pvalue, round_digits,  r_value, replace_rvalue,
                    diag_which, replace_low, replace_diag, numbered_horiz_Labels, del_empty_cols,
                    write_results, pth_filename ) {
  
  #
  # Removing so many rows with NAs in a dataset. 
  # How many rows keep with NAs data in a dataset: 0 rows with NAs; 1; 2; 3; 
  #
  # Function found on Stackoverflow # 4862178 by "Pierre Lafortune"
  #
  delete.na <- function(df, n=0) {
    df[ rowSums(is.na( df )) <= n, ]
  }
  
  # Due to ASCII warnings, colnames are replaced with ASCII characters.
  colnames_df <- colnames(corrDF )
  ascii_cols  <- paste0("V", 1:length(colnames(corrDF )))
  colnames(corrDF) <- ascii_cols
  
  # Finding the dimension of the dataset
  ln_df_hor <- dim( corrDF)[2]
  
  # Ensuring that all values are in numerical format
  corrDF_num <- do.call(cbind, lapply(1:ln_df_hor, function(x) as.numeric(as.character( corrDF[,x] )) ) )
  
  # Ensuring that no Null cases exist
  corrDF_no_null             <- as.data.frame(delete.na( corrDF_num, 0))
  
  # Counting Null cases that were removed. 
  number_of_null_cases <- dim(corrDF_num)[1]- dim(corrDF_no_null)[1]
  # Informing about the number of null cases
  print(paste0(number_of_null_cases," Cases had at least 1 Null value/s, and they were removed") )
  
  # Producing the Correlation Coefficient (r) matrix 
  df_r <- round(as.data.frame( corr.test(corrDF_no_null, method = corrmethod, adjust = corradjust )$r), round_digits )
  # Producing the Significant (p) matrix 
  df_p <- round(as.data.frame( corr.test(corrDF_no_null, method = corrmethod, adjust = corradjust )$p), round_digits )
  
  
  # Finding the dimension of the Correlation matrix
  ln_cor <- dim( df_p )[1]
  
  # Replacing r values when p values higher than X value. 
  corr_rdata <-
    do.call(rbind,
            lapply(1:ln_cor, function(h) gsub("FALSE", replace_low, 
                                              do.call(cbind, lapply(1:ln_cor, function(v) gsub("TRUE", df_r[v, h], df_p[v, h] <= pvalue ) )) ) ) )
  
  corr_rdata <- as.matrix( corr_rdata )
  
  # Replacing r values lower than X value
  corr_r_vales <-
    do.call(rbind,
            lapply(1:ln_cor, function(h) gsub("FALSE", replace_rvalue, 
                                              do.call(cbind, lapply(1:ln_cor, function(v) 
                                                if( is.na(as.numeric(corr_rdata[v, h])) == TRUE ) { corr_rdata[v, h] <- corr_rdata[v, h]  } else {  
                                                  gsub("TRUE", corr_rdata[v, h], abs(as.numeric(corr_rdata[v, h] )) > r_value  )  }  )
                                              )) ) )
  
  corr_r_vales <- as.matrix( corr_r_vales )
  
  
  # Original Colnames are added back to df.
  colnames( corr_r_vales) <- colnames_df
  rownames( corr_r_vales) <- colnames_df
  
  
  # Replacing the ( 1 ) Right Top or (2) Left Bottom Diagonial Values
  if ( diag_which == 1 ) {
    
    bottom_left_diag <-
      do.call(cbind, 
              lapply( 1:ln_cor, function( v ) 
                do.call(rbind, lapply( 1:ln_cor, function( h ) 
                  if (  h  > v   )  {     corr_r_vales[ h, v ] <- replace_diag
                  } else {     corr_r_vales[ h, v ] <- corr_r_vales[ h, v ]  } 
                ) ) ) )
    
    diag( bottom_left_diag ) <- replace_diag
    corr_results             <- bottom_left_diag
    
  } 
  
  
  # Replacing the ( 1 ) Rikght Top or (2) Left Bottom Diagonial Values
  if( diag_which == 2 ){
    
    upper_right_diag <-
      do.call(cbind, 
              lapply( 1:ln_cor, function( v ) 
                do.call(rbind, lapply( 1:ln_cor, function( h ) 
                  if (  h  < v   )  {     corr_r_vales[ h, v ] <- replace_diag
                  } else {     corr_r_vales[ h, v ] <- corr_r_vales[ h, v ]  } 
                ) ) ) )
    
    diag( upper_right_diag ) <- replace_diag
    corr_results             <- upper_right_diag
    
  }
  
  
  # Replacing Horizontal Labels with Numbers e.g. to Reduce Column Head Space  
  if ( numbered_horiz_Labels == 1) {
    
    colnames( corr_results ) <- paste0( "[", 1:ln_cor, "] ")
    rownames( corr_results ) <- paste0( "[", 1:ln_cor, "] ", colnames_df )
    
  }
  
  
  if ( numbered_horiz_Labels == 2) {
    
    colnames( corr_results ) <- colnames_df
    rownames( corr_results ) <- colnames_df
    
  }
  
  if ( del_empty_cols ==1 ) {
    
    # Removing empty Columns
    non_empty_cols <- 
      do.call(rbind,
              lapply(1:ln_cor, function(x)
                if( TRUE %in% (abs(as.numeric(corr_results[,x]))>0) ) { x } else {  } ) )
    
    cr_results <- corr_results[,c(non_empty_cols)]
    
  } 
  
  
  
  if ( del_empty_cols == 2 ) { 
    
    cr_results <- corr_results
    
  }  
  
  # Replacing the leading Zero with ""
  cr_results <- sub("0.", ".", cr_results,  fixed = TRUE)
  
  
  if ( write_results ==  1 ) {
    
    write.table( cr_results, pth_filename, append = FALSE, sep = ",", na = "NA", 
                 dec = ".", row.names = TRUE, col.names = NA, fileEncoding = "UTF-16" )
    
  }
  
  if ( write_results ==  2 ) {
    pth_filename=""
    print("data were not written to file")
  }
  
  
  
  # Results
  cr_results
  
}

