
# Found on Public Domain - Changed
cut_off_probs <- function( df_prob, rnd_no, equition = c( ">.5", "10", "<=.5", "0"  ) ){
  
  eq_length  <- length( equition ) / 2 
  seq_no_1   <- seq( from = 1, to = 20, by = 2)
  seq_no_2   <- seq( from = 2, to = 20, by = 2)
  H_values   <- round( as.data.frame( df_prob ), rnd_no )
  actual_no1 <- seq_no_1[ 1:eq_length ]
  actual_no2 <- seq_no_2[ 1:eq_length ]
  eq2_vals   <- equition[ seq_no_2[ 1:eq_length ] ]
  
  H_data <- list()
  for ( i in actual_no1 ) {
    
    H_data[[i]]  <- unlist( lapply( 1:dim( H_values )[2], function(k )
      eval( parse( text = paste( as.character( H_values, "[", H_values ), equition[i] )[k] ) ) ) )
    
  }
  
  H_freqs  <- unlist( lapply(1:max( actual_no1 ), function(k) length( H_data[[k]][H_data[[k]] == TRUE] ) )[ actual_no1 ] )
  H_prob   <- round( H_freqs / sum( H_freqs ), rnd_no )
  df_probs <- as.data.frame( cbind( as.numeric( eq2_vals ), H_prob)  )
  
  df_probs
  
}