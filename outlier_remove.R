
##########################################################
####             Description                         #####
##########################################################

# A function that Remove Outliers on the fly
# Some plots are drawn in orsder to show you the outliers
# Additional plots can be easily inserted into the function
# It return the dataset without the outlier
# The whole case / row is removed from the data output


# - NOTE - 
# The Cook distance points and mahalanobis points are sorted
# in order in order to be easily removed.
# The cases are removed based on cook distances.
# The rest plots are informatics only on the quality of the dataset


##########################################################
####             Dependencies                        #####
##########################################################

# Depends on Base R / stats only



##########################################################
####             Example DF                          #####
##########################################################

ex_df <- 
cbind(c(1,2,3,4,5,6,7,8,9,10,11,12,13),
      c(1,4,6,2,7,2,8,2,7,1,9,2,8), 
      c(1,2,6,4,7,5,6,7,7,2,3,6,8),
      c(3,4,5,1,3,4,8,5,7,1,9,2,1), 
      c(1,2,6,4,6,5,4,3,2,2,1,2,3)  ) 

# Making colnames and rownames
colnames(ex_df) <- letters[ 1:dim(ex_df)[2] ]
rownames(ex_df) <- 1:dim(ex_df)[1]



##########################################################
####            Arguments of the Function            #####
##########################################################

# df_data_no_id:  Must be a dataframe without ID variable
# id_name_var  :  The name of the id var 

# cook_mahal   :  "C" or 1 for Cook Distance only outlier removal
#              -  "M" or 2 for Malahanobis Distance only removal
#              -  "B" or 3 for Both Cook and Mahalanobis outlier removal

# sensitivity  :  How many outlier / cases will be removed ? 
#              -  An integer - Note: This may be relative  
#              -  e.g. 1 may mean the 1st pair of outliers 
#              -  One from "Cooks" and One from "Mahalanobis,
#              -  thus, two values will be removed. 

# cut_off      :  Both, cook and Mahalanobis distances are 
#              -  expressed in percentages based on their max value 
#              -  What percentage difference to use between the previous and the next value
#              -  in order to exclude outliers ? 
#              -  eg. 100% 91% 89% --- perc diff: 9% 2% 
#              -  Thus cut_off = 10 will exclude only one value
#              -  Thus cut_off = 1  will exclude two values

# plotit       :  To display the outlier plots or not ? 
#              -  1/2 or "TRUE" / "FALSE" or "T" / "F"



### NOTE: Because of percentaging the distances, 
#         a value can have a 10 perc distance point in the middle of the dataset
#         this value can be removed from the dataset, if the cut-off / sensitivity 
#         can catch it. 

### NOTE: By default the first two pairs of outliers are reported even 
#         cut-off / sensitivity will be set to zero

### NOTE: If cut-off is set to high, then, 0 to very few values may be reported 

### NOTE: If only one of the Cook / Mahalanobis D. values satisfy cut-off / sensitivity args, 
#         eitherwaym both pairs of values will be reported

### NOTE: Plots may change slightly even no outlier removed, if cut-off/cook_mahal args are manipulated 
#         due to the way that distances are ordered

### NOTE: Complete datasets only


##########################################################
####           The Function                          #####
##########################################################


outlier_remove <- function( df_data_wt_id, id_name_var, cook_mahal, sensitivity,  cut_off, plotit  ) {
    
      #### Test section 
       # 
       # df_data_wt_id <- ex_df
       # id_name_var = "a"
       # sensitivity= 3 
       # cut_off = 1
       # plotit = 1
      
  df_data            <- as.data.frame( df_data_wt_id[, -which(colnames( df_data_wt_id  ) %in% c(id_name_var)) ] )
  sens               <- sensitivity
   
  # Mahalanobis manipulations
  mahal_data         <- mahalanobis( df_data, colMeans(df_data), cov(df_data) )
  list_mah           <- as.list( mahal_data )
  names( list_mah )  <- 1:length(mahal_data)
  unlist_mah         <- unlist( list_mah )
  
  mahal_order        <- unlist_mah[order(unlist_mah)]
  ord_mahal          <- mahal_order[order(mahal_order, decreasing = T)]
  
  # Cook's manipulations
  cooks_order   <- round(cooks.distance(lm(df_data))[order(cooks.distance(lm(df_data)))]*100,3)
  ord_cooks     <- cooks_order[order(cooks_order, decreasing = T)]
  
  
  # Percentizing the distances
  ck_perc_0      <- round( (ord_cooks/max(ord_cooks))*100, 2)
  ck_diff_dist   <- ck_perc_0 - c(ck_perc_0[-1], 0)
  
  mh_perc_0      <- round( (ord_mahal/max(ord_mahal))*100, 2)
  mh_diff_dist   <- mh_perc_0 - c(mh_perc_0[-1], 0)

  # the ID of Data out
  ids_cook        <- names(ord_cooks[1:sens])
  ids_mahal       <- names(ord_mahal[1:sens])
  
  cook_out_vals   <- mh_diff_dist[ mh_diff_dist > cut_off ]
  mahal_out_vals  <- ck_diff_dist[ ck_diff_dist > cut_off ]
 
  
  # Merging Cook and Mahalanobis outliers in a single df, per two
  cook_mahal_out  <- unique(c( names( cook_out_vals ), names( mahal_out_vals ) ))
  fl_out          <- floor(length( cook_mahal_out ) / 2)
  col_data_out    <- cbind( cook_mahal_out[1:fl_out], cook_mahal_out[( fl_out + 1 ):( fl_out*2 ) ] )  
  list_data_out   <- unlist(as.list(( col_data_out[1:sens,] )))
  
  
  # Dataset withouτ the Cookoutliers - with id _based on sensitivity
  ckdata_out_id      <- df_data_wt_id[-which(rownames( df_data ) %in% c( ids_cook ) ),]
  
  # Dataset withouτ the Mahalanobis outliers - with id  _based on sensitivity
  mhdata_out_id      <- df_data_wt_id[-which(rownames( df_data ) %in% c( ids_mahal ) ),]
  
  # Dataset withouτ the Cook and Mahalanobis outliers - with id  _based on cut_off
  alldata_out_id     <- df_data_wt_id[-which(rownames( df_data ) %in% c( list_data_out ) ),]
  
  
  if ( cook_mahal == "C" | cook_mahal == 1 ) {
    ckdata_out_id    <-  as.data.frame( ckdata_out_id )
    ckdata_out_noid  <- ckdata_out_id[-1]
    data_out         <- ckdata_out_noid
    data_out_id      <- ckdata_out_id
    
    cooks_data_out   <- round( cooks.distance(lm( data_out ) )[order(cooks.distance( lm( data_out ) ))]*100, 3 )
    mahal_dt_out     <- mahalanobis( data_out, colMeans( data_out ), cov( data_out ))
    mahal_data_out   <- mahal_dt_out[order( mahal_dt_out )]
    
    if ( plotit == 1 | plotit == "TRUE" | plotit == "T" )  {  
      
      par(mfrow=c(2, 2))
      plot( mahal_data_out, main = "Mahal. dist" )
      plot( cooks_data_out, main = "Cooks dist.")
      
      lm_data <- lm(data_out)[[2]]
      
      qqnorm(rstandard( lm(data_out)  ) )
      plot(resid( lm(data_out) ), rstudent( lm(data_out) ),   xlab = "residuals", ylab= "rstudent" )
    }
    
    if ( plotit == 2 | plotit == "FALSE" | plotit == "F" ) {  } 
    
  }
  
  
  if ( cook_mahal == "M" | cook_mahal == 2 ) {
    mhdata_out_id    <-  as.data.frame( mhdata_out_id )
    mhdata_out_noid  <- mhdata_out_id[-1]
    data_out         <- mhdata_out_noid
    data_out_id      <- mhdata_out_id
    
    cooks_data_out   <- round( cooks.distance(lm( data_out ) )[order(cooks.distance( lm( data_out ) ))]*100, 3 )
    mahal_dt_out     <- mahalanobis( data_out, colMeans( data_out ), cov( data_out ))
    mahal_data_out   <- mahal_dt_out[order( mahal_dt_out )]
    
    if ( plotit == 1 | plotit == "TRUE" | plotit == "T" )  {  
      
      par(mfrow=c(2, 2))
      plot( mahal_data_out, main = "Mahal. dist" )
      plot( cooks_data_out, main = "Cooks dist.")
      
      lm_data <- lm(data_out)[[2]]
      
      qqnorm(rstandard( lm(data_out)  ) )
      plot(resid( lm(data_out) ), rstudent( lm(data_out) ),   xlab = "residuals", ylab= "rstudent" )
    }
    
    if ( plotit == 2 | plotit == "FALSE" | plotit == "F" ) {  } 
   
  }
  
  
  if ( cook_mahal == "B" | cook_mahal == 3 ) {
    alldata_out_id    <-  as.data.frame( alldata_out_id )
    alldata_out_noid  <- alldata_out_id[-1]
    data_out          <- alldata_out_noid
    data_out_id       <- alldata_out_id
    
    
    cooks_data_out   <- round( cooks.distance(lm( data_out ) )[order(cooks.distance( lm( data_out ) ))]*100, 3 )
    mahal_dt_out     <- mahalanobis( data_out, colMeans( data_out ), cov( data_out ))
    mahal_data_out   <- mahal_dt_out[order( mahal_dt_out )]
    
    if ( plotit == 1 | plotit == "TRUE" | plotit == "T" )  {  
      
      par(mfrow=c(2, 2))
      plot( mahal_data_out, main = "Mahal. dist" )
      plot( cooks_data_out, main = "Cooks dist.")
      
      lm_data <- lm(data_out)[[2]]
      
      qqnorm(rstandard( lm(data_out)  ) )
      plot(resid( lm(data_out) ), rstudent( lm(data_out) ),   xlab = "residuals", ylab= "rstudent" )
    }
    
    if ( plotit == 2 | plotit == "FALSE" | plotit == "F" ) {  } 

  }
  

  mh_datas           <- ck_diff_dist[1:(sens+2)] 
  ck_datas           <- mh_diff_dist[1:(sens+2)] 
  uniq_id            <- unique(names(c(mh_datas, ck_datas)))
  df_mh_ck           <- round( cbind(mh_datas, ck_datas ), 1) 
  df_mh_ck_rbind     <- rbind(names( mh_datas ), names(ck_datas) )
  unlist_df_mc       <- unlist(lapply(1:dim( df_mh_ck_rbind )[2], function(x) paste0(df_mh_ck_rbind[,x], collapse=" ")))
  rownames(df_mh_ck) <- unlist_df_mc
  
  list( data_out_id, df_mh_ck )
  
}

    

##########################################################
####           Running  The Example                  #####
##########################################################


# Please uncomment the below line/s in order to run the example 

# outlier_remove( ex_df, "a", "C", 0, 5, 1 )  # Zero points are removed, 
# outlier_remove( ex_df, "a", "C", 3, 1, 1 )  # 3 (*2) pairs of outliers are removed




