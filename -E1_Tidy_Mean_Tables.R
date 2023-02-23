
pth         = "/home/elias/Data/STATISTICS/papaioanou/output/"
file        = "Means.csv"
data_0      = read.csv( paste0(pth, file), header = F, sep = ",", quote = "" )


# In what Row, PSPP tables in csv file start and where they finish
no_tbl_index        <- which( gregexpr( data_0[[1]], pattern = "Table: Αναφορά", fixed = T ) == 1 )
lng_Notbl           <- length( no_tbl_index ) / 2
sq_odds             <- seq( 1, 100000, 2)[ 1:lng_Notbl ]
sq_evens            <- seq( 2, 100000, 2)[ 1:lng_Notbl ]
No_Tbls_start       <- no_tbl_index[ sq_odds ]
No_Tbls_fin         <- no_tbl_index[ sq_evens ]


Descriptive_Table <- list()
Tbls_Names_index  <- list()
for( i in 1:length( No_Tbls_start ) ) {
      
    # The main table
    Tables_Long         <- data_0[ ( No_Tbls_start[i] + 1 ): ( No_Tbls_fin[i] - 2 ), ]
    
    # Extracting the names of variables, as well their levels. 
    tbl_nms_desc_0      <- as.character( unlist(Tables_Long[1,] ) )
    tbl_nms_desc        <- tbl_nms_desc_0[ which( tbl_nms_desc_0 != "") ][-1]
    Tbls_Names          <- as.character( Tables_Long[ 1, 2 ] )
    names(Tables_Long)  <- unlist( Tables_Long[1,] )
    var_levels          <- unique( Tables_Long[ Tbls_Names ] )[-1,]
    var_names           <- unique( Tables_Long[ 1 ] )[-1,]
    Tables_Long         <- Tables_Long[,1:5]
    
    
    r_table             <- rbind( names( Tables_Long[1,tbl_nms_desc] ),
                                             sapply( Tables_Long[ -1, tbl_nms_desc ], function(x) 
                                             round( as.numeric( as.character( x ) ), 1 )  ) )

    Tables_Long[ ,tbl_nms_desc ] <- r_table
    
    
    # Finding where the levels of each variables start and end
    empty_rows          <- which( Tables_Long[,1] == "" )
    full_rows           <- c( which( Tables_Long[,1] != "" ), dim( Tables_Long )[1] )

    # Extracting from the main Table, each Variable X Variable pair
    Subtables_x    <- lapply( 1:( length( full_rows ) - 1  ), function(x) 
                                  Tables_Long[ full_rows[x]:( full_rows[ ( x + 1 ) ] - 1 ), ][-1]  )
    
    # Joining and Transforming the tables
    wide_Table     <- Reduce(function(...) merge(..., by = colnames( Tables_Long[2] ), all = TRUE ), Subtables_x )
    tr_wide_tb     <- t( wide_Table )
    
    # Some usefull indexing numbers: making Mean, N, SD of 1st and then making Mean, N, SD of 2nd etc.. 
    # of the common (1st, 2nd...) levels under investigation 
    grp_desc_nms   <- lapply( 1:length( tbl_nms_desc ), function(x) 
                                grep( pattern =  tbl_nms_desc[x], rownames( tr_wide_tb ) ) )
    transf_numbers <- lapply(1:length( grp_desc_nms[[1]] ), function(x) unlist( lapply( grp_desc_nms, `[[`, x) )  ) 
    
    # The main dataset is ready
    dt_descr       <- do.call( rbind, lapply( 1:length( transf_numbers ), function(y) 
                      do.call( cbind, lapply(1:length( var_levels ), function(x) 
                                t( tr_wide_tb[ transf_numbers[[y]], x] ) )  ) ) )
    rownames( dt_descr ) <- var_names
    
   
    
    lng            <- ( length( colnames( dt_descr ) ) ) / length ( var_levels )
    rep_desc       <- as.character( rep( var_levels, rep( length( tbl_nms_desc ), length( var_levels ) ) )  )
    dt_descr_final <- rbind( rep_desc, dt_descr )
    

    # Saving the results in a list
    Descriptive_Table[[i]] <- dt_descr_final
    Tbls_Names_index[[i]]  <- Tbls_Names
    
}

    
pth_filename = paste0(pth, "Descriptive_Table.csv")
file.create( pth_filename )
for( i in 1:length( Descriptive_Table ) ) {
  
  dt_write                <- cbind( rownames( Descriptive_Table[[i]] ), Descriptive_Table[[i]] )
  colnames( dt_write )[1] <- Tbls_Names_index[[i]]
  colnames( dt_write )    <- gsub("\\.x", "", colnames( dt_write ) )
  dt_write                <- rbind( dt_write, "")
  
  
  cat_levels           <- dt_write[1,]
  colnms               <- colnames( dt_write )
  colnames( dt_write ) <- cat_levels
  dt_write[1,]         <- colnms
    
  
  write.table( dt_write,
               pth_filename, append = T, sep = ",", na = "NA", dec = ".", 
               row.names = TRUE, col.names = NA, fileEncoding = "UTF-16" )
}








   



