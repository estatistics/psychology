

age_f <- factor( ordered( 1:3 ), labels = c( "1 Infant", "10 Child", "100 Old" ) )

# Stackoverflow questions/54850701/

Leveling_Labels <- function( factors, split_arg = " " ) { 
  
  leveling_Labels <- list()
  
  for( i in 1:length( factors ) )  { 
    
    splits                  <- strsplit( as.character( factors[[i]] ), split_arg )
    leveling_Labels[[i]]    <- as.numeric( unlist( lapply( 1:length( splits ), function(x) splits[[x]][1] ) ) )
    levels( factors[[i]] )  <- unlist( lapply( 1:length( splits ), function(x) splits[[x]][2] ) )
    
  }
  
  results <- list( factors, leveling_Labels )
  results
  
}


Leveling_Labels( list( age_f ), " ")
