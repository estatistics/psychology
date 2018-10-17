
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
cbind(c(1,4,6,2,7,2,8,2,7,1,9,2,8), 
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
# sensitivity  :  How many (outlier) cases will be removed ? 
#              -  An integer
# plotit       :  To display the outlier plots or not ? 
#              -  1/2 or "TRUE" / "FALSE" or "T" / "F"


##########################################################
####           The Function                          #####
##########################################################


outlier_remove <- function( df_data_no_id, sensitivity, plotit ) {
  
  
  df_data       <- as.data.frame( df_data_no_id )
  sens          <- sensitivity
  
  mahal_data    <- mahalanobis(df_data, colMeans(df_data), cov(df_data))
  mahal_order   <- mahal_well[order(mahal_data)]
  cooks_order   <- round(cooks.distance(lm(df_data))[order(cooks.distance(lm(df_data)))]*100,3)
  ord_cooks     <- cooks_order[order(cooks_order, decreasing = T)]
  
  data_out         <- df_data[-which(rownames( df_data ) %in% names(ord_cooks[1:sens])),]
  
  cooks_data_out   <- round(cooks.distance(lm(data_out))[order(cooks.distance(lm(data_out)))]*100,3)
  mahal_dt_out     <- mahalanobis(data_out, colMeans(data_out), cov(data_out))
  mahal_data_out   <- mahal_dt_out[order(mahal_dt_out)]
  
  if ( plotit == 1 | plotit == "TRUE" | plotit == "T" )  {  
    
    par(mfrow=c(2, 2))
    plot( mahal_data_out, main = "Mahal. dist" )
    plot( cooks_data_out, main = "Cooks dist.")
    
    lm_data <- lm(data_out)[[2]]
    
    qqnorm(rstandard( lm(data_out)  ) )
    plot(resid( lm(data_out) ), rstudent( lm(data_out) ),   xlab = "residuals", ylab= "rstudent" )
  }
  
  if ( plotit == 2 | plotit == "FALSE" | plotit == "F" ) {  } 
  
  data_out
} 



##########################################################
####           Running  The Example                  #####
##########################################################


# Please uncomment the below line/s in order to run the example 

# outlier_remove( ex_df, 0, 1) # Zero points are removed 
# outlier_remove( ex_df, 6, 1) # 6 points are removed


