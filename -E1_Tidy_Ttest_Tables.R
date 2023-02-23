
pth         = "/home/elias/Data/STATISTICS/athanasoula/output/"
file        = "ttests.csv"
txtfile     <- readLines( paste0(pth, file) )
data_0      <- as.data.frame( do.call( rbind, strsplit(txtfile, ",") ) )

# In what Row, PSPP tables in csv file start and where they finish
no_tbl_index_start  <- which( gregexpr( data_0[[1]], pattern = "Table: Τεστ Ανεξαρτήτων Δειγμάτων", fixed = T ) == 1 )
no_tbl_index_fin    <- which( gregexpr( data_0[[1]], pattern = "Table: Στατιστικά Ομάδα", fixed = T ) == 1 )[-1]
no_tbl_index_F      <- which( gregexpr( data_0[[3]], pattern = "F", fixed = T ) == 1 )[-1]
no_tbl_ind_var_name <- ( which( gregexpr( data_0[[3]], pattern = "Table: Στατιστικά Ομάδας", fixed = T ) == 1 ) ) + 1
var_names           <- as.character( unlist( data_0[no_tbl_ind_var_name,][[2]] ) )
var_length          <- ( abs( no_tbl_index_start[[1]]-no_tbl_index_fin[[1]] ) / 2 ) - 2
var_names_len       <- length( var_names )
var_reps            <- unlist( lapply(1:var_names_len, function(x) c( var_names[x], rep( "", 21) ) ) )



Levene_1st_lns          <- which( gregexpr( data_0[[2]], pattern = "Υπόθεση Ίσων Διακυμάνσεων", fixed = T ) == 1 )
Levene_1st              <- data_0[ Levene_1st_lns, ]
Levene_2nd              <- data_0[ ( Levene_1st_lns + 1), ]
Levene_p                <- as.numeric( as.character( data_0[Levene_1st_lns,]["V4"][[1]] ) )
levP.105                <- which( Levene_p < .105 )

L105                    <- Levene_1st[ levP.105,]
L105                    <- Levene_2nd[ levP.105,]
L105[ c( "V3" ) ]       <- Levene_1st[ levP.105,][ c( "V3" ) ]
Levene_1st[ levP.105,]  <- L105
Levene_1st[[1]]         <-  data_0[ Levene_1st_lns, ][[1]]
colnames( Levene_1st )  <- as.character( unlist( data_0[ no_tbl_index_F,][1,] ) )

Data_Ttest              <- cbind( var_reps, Levene_1st )

pth_filename = paste0(pth, "Ttest_Tables.csv")
file.create( pth_filename )

write.table( dt_write,
             pth_filename, append = F, sep = ",", na = "NA", dec = ".", 
             row.names = TRUE, col.names = NA, fileEncoding = "UTF-16" )

