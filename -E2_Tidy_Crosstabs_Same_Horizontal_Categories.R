##########################################################
#### Description                                     #####
##########################################################

# This function tries to tidy up PSPP tables to be aligned vertically, based on same 
# horizontal labels 
# Crosstabulation tables produced from open source statistical program PSPP

#Limitations
# The code tries to find the Last Total column by counting Totals
# This is done by taking into consideration "100%" summaries of pspp
# that has an extra "total". This is not counted at the final results.

# WARNING 
# read.csv uses only the first 5 rows to determine if a column has data
# If not, it is skipping it. So, please add some dummy colnames in 
# order to import all your columns

##########################################################
#### Dependencies                                   #####
##########################################################

# Depends on Base R / stats only
# To write results may require(RCurl)

##########################################################
#### Example DF                                   #####
##########################################################

# SEE at the end of this file

##########################################################
####Arguments of the Function                       #####
##########################################################

# path_csv : you must define the whole path eg.  pth_file=/home/USER/data/
# In this path output results will be written
#
# file_name : you must define the whole file name eg. crosstabs.csv
#
# csv_opts: default to read.csv( paste0(path_csv, file_name), header = FALSE, sep = ",", quote = "", na.strings="." )
# 
# pth to open csv files: eg. 
# tbl_rows : you must include the row stats that you need: tbl_rows=c('Count','Row','Col', "rTotal") 
# Row, Column stats are only included if exist in csv and specified here. They refer to "Row %" and "Col %"

# chqstats : you must include the stats that you need: 
# chqstats=c('Cramersv', 'Phi',  'Contingency', 'taub', 'tauc', 'tauc', 'Gamma', 'Spearman', 'Pearson')

#

##########################################################
####  The Function                              #####
##########################################################


crosstabs_same_horizontal_labels <- function(path_csv="", file_name="", csv_opts=FALSE,
                                             tbl_rows=c('Count','Row','Col', "rTotal"), rsumTotal=0, 
                                             chqstats=c('Cramersv', 'Phi',  'Contingency', 'taub', 'tauc', 'Gamma', 'Spearman', 'Pearson') ) {

  options(max.print=999999)          
  csv_files = read.csv( paste0(path_csv, file_name), header = FALSE, strip.white = FALSE, 
                        blank.lines.skip = FALSE, sep = ",", quote = "", na.strings=".",
                        numerals = c("no.loss"), fill = T, skipNul=F )
  
  str(csv_files)
  colnames(csv_files)
  dim(csv_files)
  head(csv_files, 10)
  
  # Reading csv file
  if (csv_opts == FALSE)  csv_files=csv_files   else if (csv_opts != FALSE)     csv_files = csv_opts
  
  #### Making an operator
  # Found on stackoverflow "5831794"
  '%!in%' <- Negate(`%in%`)
  
  # For multiple crosstab csv files from PSPP in same folder to process.
  # Only when horizontal categories are same
  crosstbs <- csv_files
  crosstbs$id <- 1:(dim( crosstbs )[1])
  
  
  # What row results to include in final crosstab results
  if ('Count'     %in% tbl_rows )   tbl_Count     = 'Count'         else if ('Count'     %!in%  tbl_rows)     tbl_Count     = 'NAN' 
  if ('Row'       %in% tbl_rows )   tbl_Row       = 'Row %'         else if ('Row'       %!in%  tbl_rows)     tbl_Row       = 'NAN' 
  if ('Col'       %in% tbl_rows )   tbl_Col       = 'Col %'         else if ('Col'       %!in%  tbl_rows)     tbl_Col       = 'NAN' 
  if ('rTotal'    %in% tbl_rows )   tbl_rTotal    = 'Total %'       else if ('rTotal'    %!in%  tbl_rows)     tbl_rTotal    = 'NAN' 
  # Defining how many rows the rowsumtotal is using
  if (rsumTotal    !=         0 )   tbl_rsumTotal = rsumTotal       else if (rsumTotal    ==           0)     tbl_rsumTotal = 0 # How many positions to let grand total appear?
  
  # Dont include row results that user excluded
  include_tbl <- c(tbl_Count, tbl_Row, tbl_Col, tbl_rTotal)
  exclude_nan <- include_tbl[include_tbl %!in% "NAN" ] 
  
  # Excluding "NAN" and "1" from table(sum), finding the first and last column of data. 
  tbl_last  <- as.data.frame( sapply( 1:min(dim(crosstbs)), function(i) table(crosstbs[i])["Total"]    ) )
  tbl <- cbind( tbl_last, 1:dim(tbl_last)[1])
  colnames(tbl) <- c("a", "b")
  tbl_nn        <- tbl[!is.na(tbl["a"]),]
  tbl_nn1       <- tbl_nn[tbl_nn["a"]!=1,]
  col_min_tot   <- as.integer( tbl_nn1[min(order(tbl_nn1[,1])),]["b"][1] )
  col_max_tot   <- as.integer( tbl_nn1[max(order(tbl_nn1[,1])),]["b"][1] )

   # tbl_st tbl_fn - start/fin of numerical tables
  tbl_st          <- crosstbs[crosstbs$id[ grepl(x=crosstbs[[col_max_tot]], "Total") ], ]
  tbl_fn          <- crosstbs[crosstbs$id[ grepl(x=crosstbs[[col_min_tot]], "Total") ], ] 

  print("table start values")
  dim( tbl_st); tbl_st
  print("table end values")
  dim( tbl_fn); tbl_fn

   if ( dim( tbl_st)[1] > dim( tbl_fn)[1] ) tbl_st <- tbl_st[-1,] else if ( dim( tbl_st)[1] == dim( tbl_fn)[1] ) tbl_st=tbl_st;
  
  
  # number of tables
  no_tbls         <- length(tbl_st$id)
  # Which rows to include? 
  dims_no_st_fn   <- sapply(1:no_tbls, function(i) seq( tbl_st$id[i] + 2,  tbl_fn$id[i] -1 + tbl_rsumTotal)) 
  # The numerical tables as list
  cross_tbls      <- sapply(1:no_tbls, function(i) list(  rbind(crosstbs[ dims_no_st_fn[[i]], ], rep("", min(dim(crosstbs)) ) ) ) )
  # The numerical tables as binded
  crss_tbls_binded <- do.call("rbind", cross_tbls)
  # adding colanmes
  colnames(crss_tbls_binded) <- crosstbs[ dims_no_st_fn[[1]][1] -1, ]
  
  # Naming empty cols
  empty_colnms   <- colnames(crss_tbls_binded) %in% "" 
  nm_empty_cols  <- colnames(crosstbs)[empty_colnms]
  nm_empty_cols  <- c("Var", "Levels", "Counts", "Totals")
  colnames(crss_tbls_binded)[empty_colnms] <- nm_empty_cols
  # Naming last "ID" column
  colnames(crss_tbls_binded)[ min(dim(crosstbs)) ] <- "id"
  
  # final crosstabs data
  final_crosstbl <- crss_tbls_binded[crss_tbls_binded$Counts %in% c("", exclude_nan), ]
  
  #####################################################################################
  #####################################################################################
  
  # Extracting pearson r and sig. of it and nvalid cases
  st_pchisq          <-crosstbs[crosstbs$id[ grepl(x=crosstbs[[1]], "Pearson Chi-Square") ], ][-1][,1:3]
  st_nvalid          <-crosstbs[crosstbs$id[ grepl(x=crosstbs[[1]], "Linear-by-Linear Association") ] + 1, ][2]
  colnames(st_pchisq) <- c("chisq", "df", "p")
  colnames(st_nvalid) <- c("n.valid")
  
  # Extracing crosstab stats
  st_cramer          <-as.data.frame( crosstbs[crosstbs$id[ grepl(x=crosstbs[[2]], "Cramer") ],      ][[3]] )
  st_phi             <-as.data.frame( crosstbs[crosstbs$id[ grepl(x=crosstbs[[2]], "Phi") ],         ][[3]] )
  st_contingency     <-as.data.frame( crosstbs[crosstbs$id[ grepl(x=crosstbs[[2]], "Contingency") ], ][[3]] )
  st_taub            <-as.data.frame( crosstbs[crosstbs$id[ grepl(x=crosstbs[[2]], "tau-b")       ], ][[3]] )
  st_tauc            <-as.data.frame( crosstbs[crosstbs$id[ grepl(x=crosstbs[[2]], "tau-c") ],       ][[3]] )
  st_gamma           <-as.data.frame( crosstbs[crosstbs$id[ grepl(x=crosstbs[[2]], "Gamma") ],       ][[3]] )
  st_spearman        <-as.data.frame( crosstbs[crosstbs$id[ grepl(x=crosstbs[[2]], "Spearman") ],    ][[3]] )
  st_pearson         <-as.data.frame( crosstbs[crosstbs$id[ grepl(x=crosstbs[[2]], "Pearson") ],     ][[3]] )
  
  # colnaming crosstab stats
  colnames(st_cramer)           <- "Cramers v"
  colnames(st_phi)              <- "Phi"
  colnames(st_contingency)      <- "Contingency Coeff."
  colnames(st_taub)             <- "Kendalls tau-b"
  colnames(st_tauc)             <- "Kendalls tau-c"
  colnames(st_gamma)            <- "Gamma"
  colnames(st_spearman)         <- "Spearman corr"
  colnames(st_pearson)          <- "Pearsons r"
  
  # If else for crosstab stats
  chq_pchisq = st_pchisq 
  chq_nvalid = st_nvalid
  if ('Cramersv'    %in% chqstats )   chq_cramersv  = st_cramer       else if ('Cramersv'     %!in%  chqstats)     chq_cramersv    = 'NAN' 
  if ('Phi'         %in% chqstats )   chq_phi       = st_phi          else if ('Phi'          %!in%  chqstats)     chq_phi         = 'NAN' 
  if ('Contingency' %in% chqstats )   chq_conting   = st_contingency  else if ('Contingency'  %!in%  chqstats)     chq_conting     = 'NAN' 
  if ('taub'        %in% chqstats )   chq_taub      = st_taub         else if ('taub'         %!in%  chqstats)     chq_taub        = 'NAN' 
  if ('tauc'        %in% chqstats )   chq_tauc      = st_tauc         else if ('tauc'         %!in%  chqstats)     chq_tauc        = 'NAN' 
  if ('Gamma'       %in% chqstats )   chq_gamma     = st_gamma        else if ('Gamma'        %!in%  chqstats)     chq_gamma       = 'NAN' 
  if ('Spearman'    %in% chqstats )   chq_spearman  = st_spearman     else if ('Spearman'     %!in%  chqstats)     chq_spearman    = 'NAN' 
  if ('Pearson'     %in% chqstats )   chq_pearson   = st_pearson      else if ('Pearson'      %!in%  chqstats)     chq_pearson     = 'NAN' 
  
  # Binding all non nan crosstabs stats
  chqstats_all      <- c(chq_cramersv, chq_phi,  chq_conting, chq_taub, chq_tauc, chq_gamma, chq_spearman, chq_pearson) 
  chqstats_nan      <- chqstats_all  %!in% "NAN"
  chqstats_df       <- c(chq_cramersv, chq_phi,  chq_conting, chq_taub, chq_tauc, chq_gamma, chq_spearman, chq_pearson)
  chqstats_df_final <- cbind( chq_pchisq, chq_nvalid, chqstats_all[ chqstats_nan ] )
  
  
  #####################################################################################
  #####################################################################################
  
  # Creating an empty file for horizontal-type format table
  file.create( paste0(path_csv, "chqstats.csv") )
  
  # Writing results to the path given
  write.table( final_crosstbl,     append = TRUE, paste0(path_csv, "chqstats.csv") )
  write.table( chqstats_df_final,  append = TRUE, paste0(path_csv, "chqstats.csv") )
  
}



##########################################################
####           Running  The Example                  #####
##########################################################

#### Downloading example crosstab csv files from github of "estatistics/psychology"
#### !!! please uncomment the following lines in order to run the example !!!
#
# require(RCurl)
#
# crosstabs_01 <-read.csv(text=getURL("https://raw.githubusercontent.com/estatistics/psychology/master/crosstabs_samedata_horizontal_example.csv" ), 
#                         skip=0, header=F,  sep = "," )# 
# crosstabs_same_horizontal_labels(
#  path_csv="", file_name="crosstabs_01", csv_opts=FALSE,
#  tbl_rows=c('Count','Row', "rTotal"), rsumTotal=0, 
#  chqstats=c('Cramersv', 'Phi',  'Contingency',  'Spearman', 'Pearson')
# )
  
