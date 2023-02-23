df_sample <- structure(list(V1 = structure(c(7L, 6L, 1L, 2L, 3L, 4L, 5L,
                                             8L, 6L, 1L, 2L, 3L, 4L, 5L,
                                             9L, 6L, 1L, 2L, 3L, 4L, 5L,
                                             10L,6L, 1L, 2L, 3L, 4L, 5L,
                                             11L,6L, 1L, 2L, 3L, 4L, 5L,
                                             12L, 6L,1L, 2L, 3L, 4L, 5L),
                                           .Label = c("a", "b", "c", "d", "abcd", "ctgs",
                                                      "choice_A1", "choice_B1", "choice_C1",
                                                      "choice_A2", "choice_B2", "choice_C2" ),
                                           class = "factor"), V3 = structure(c(1L, 47L, 26L, 9L, 45L,
                                                                               44L, 43L, 1L, 47L, 35L, 13L, 44L, 2L, 43L, 1L, 47L, 11L, 12L,
                                                                               14L, 4L, 43L, 1L, 47L, 30L, 3L, 5L, 23L, 43L, 1L, 47L, 31L, 32L,
                                                                               7L, 32L, 43L, 1L, 47L, 17L, 5L, 10L, 8L, 43L), .Label = c("",
                                              "1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
                                              "2", "20", "21", "23", "24", "25", "26", "27", "28", "29", "3",
                                                 "31", "35", "37", "38", "39", "4", "43", "44", "5", "50", "51",
                                                  "58", "59", "6", "63", "64", "65", "66", "67", "68", "7", "8",
                                                 "9", "Frequency"), class = "factor")), row.names = c(NA, 42L), class = "data.frame")


pth  = "/home/elias/Data/STATISTICS/elena lamp/"
file = "PSPP_Tables_percentages.csv"

# Reading csv file
data_0       = read.csv( paste0(pth, file), header = F, sep = ",", quote = "" )

df_data      = data_0

# Search phrases that show the start and end points of a table. 
# start_phrase      = "choice"
# end_phrase        = "abcd"

start_phrase      = "Value Label"
end_phrase        = "Total"

# In what column, to make the respect search?
start_search_col  = 1
end_search_col    = 1

# Correcting for start line and end line, Try e.g. "1" in start or end line, 
# if your starting and end points are not the exact points 
# that your Table start or end, correspondingly. 
# This point is subtracted by the actual start or end point. 

start_line        = 1
end_line          = 0

# In What row and column are the respected labels ?
row_labels        = 1
col_labels        = 2

# How many multiple choice tables will be combined ?
combine_by        = 3

pth_filename = paste0( pth, "combine.csv" )


Multiple_Choice_Combine( df_data, start_phrase, end_phrase, start_line, end_line,
                         start_search_col, end_search_col, row_labels, col_labels, 
                         combine_by, pth_filename )





Multiple_Choice_Combine <- function( df_data, start_phrase, end_phrase,  start_line, end_line,
                                     start_search_col, end_search_col, row_labels, col_labels, 
                                     combine_by, pth_filename ) {

  lines_start   <-  which ( gregexpr( fixed = T,  pattern = start_phrase, text = df_data[ ,start_search_col ] ) == 1 )
  lines_end     <-  which ( gregexpr( fixed = T,  pattern = end_phrase,   text = df_data[ ,end_search_col   ] ) == 1 )  

  
  if ( length( lines_start ) == 0 | length( lines_end ) == 0   ) {
    
    print("start_phrase or end_phrase was not found anywhere in the data file provided")

  } else {
    
    
    lns_start    <- lines_start[ seq( 1, length( lines_start ), 1 ) ] - start_line
    lns_end      <- lines_end  [ seq( 1, length( lines_end ),   1 ) ] - end_line
    
    df_list      <- lapply( 1:length( lns_start ), function(x) as.data.frame( ( df_data[ lns_start[x]:lns_end[x], ] ) ) ) 
    only_df      <- lapply( 1:length( lns_start ), function(x) as.data.frame( ( df_data[ lns_start[x]:lns_end[x], ] ) )[ -c( row_labels, col_labels ), ] ) 
    df_vars      <- unique( unlist( lapply( 1:length( lns_start ), function(x) as.data.frame( ( df_data[ lns_start[x]:lns_end[x], ]  ) )[ row_labels, ] ) ) )
    df_vars      <- as.character( df_vars[ df_vars != "" ] )
    df_col_names <- unlist( lapply( 1:length( lns_start ), function(x) as.data.frame( ( df_data[ lns_start[x]:lns_end[x], ]  ) )[ col_labels,] ) )
    df_col_names <- as.character( unique( df_col_names ) )
    
    lng_df       <- length( df_vars )
    vars_lng     <- lng_df / combine_by
    a_lng_pos    <- seq( 1, lng_df, combine_by )
    b_lng_pos    <- seq( combine_by, lng_df, combine_by )
    
    
    df_whole_res <- list()
    for( i in 1:vars_lng ) {
      
      df_whole             <- Reduce( function( x, y ) merge(x = x, y = y, by = "V1", all.x = T, all.y = T),   only_df[ a_lng_pos[i]:b_lng_pos[i] ] )
      colnames( df_whole ) <- c( df_col_names[1],  rep( df_col_names[-1], combine_by ) ) 
      df_whole             <- cbind( as.data.frame( t( df_vars[ a_lng_pos[i]:b_lng_pos[i] ] ) ), df_whole )
      
      df_whole_res[[i]] <- df_whole
      
    }
    
    if ( pth_filename ==  0 ) {

      print("no saving")

    } else {

      file.create( pth_filename )

      for( i in 1:length( df_whole_res ) ) {

        write.table( df_whole_res[[i]],
                     pth_filename, append = T, sep = ",", na = "NA", dec = ".",
                     row.names = TRUE, col.names = NA, fileEncoding = "UTF-16" )

      }

    }
    
    df_whole_res
    
  }
  
  
}










