##########################################################
####        Multivariate Normality assessment       #####
##########################################################


#
# Removing so many rows with NAs in a dataset. 
# How many rows keep with NAs data in a dataset: 0 rows with NAs; 1; 2; 3; 
#
# Function found on Stackoverflow # 4862178 by "Pierre Lafortune"
#

delete.na <- function(df, n=0) {
  df[ rowSums(is.na( df )) <= n, ]
}

# Data for outliers
outdata_id <- delete.na( ndata_0 )
outdata    <- outdata_id[-1]

x_outdata = outdata[ -outdata_id$ΑΑ[62], ]


#######################################
#    General Multivariate Normality   #    
#######################################

mvn( outdata )



#################################
#     Mahalanobis Distance      #    
#################################

# MyFunction
outlier_remove( ndata_0, "ΑΑ", "C", sensitivity = 3, cut_off = 0, plotit = 1 )

out_data <- x_outdata
Mahal_dist        <- mahalanobis( out_data, colMeans( out_data ), cov( out_data ), inverted = F )
order_Mahal       <- order( Mahal_dist, decreasing = FALSE )
mahalanobis_order <- Mahal_dist[ order_Mahal ]

# How many plots in a single window ?
par( mfrow = c( 2, 2 ) )

# plot Mahalanobis Distance
plot( mahalanobis_order )
# plot( density( mahalanobis_order, bw = 0.5 ),  main="Squared Mahalanobis Distance")


qq_form <- qchisq( ppoints(100), df = ncol( out_data ) )
text( qqplot( qq_form, mahalanobis_order, 
              main = expression("QQplot of Mahal." * ~D^2 * 
                                  " vs quantiles of" * ~chi[ ncol( out_data ) ]^2 ),
              cex.main = 0.8, cex.xlab= 0.8, cex.ylab= 0.8,
              xlab = "Index", ylab= "Mahalanobis D, ordered" ), 
      labels = ifelse(   qq_form  > mean( qq_form )*1.5, names( mahalanobis_order ), ""),
      col = "red", cex=0.8 )



par( mfrow = c( 1, 2 ) )
################################
#       Cook's Distance       #    
###############################
out_data     <- ndata_ok[ -c(14, 13, 12, 10, 105, 8, 44, 80, 74, 7, 24), ]

lm_df        <- lm(out_data[-1], data = out_data)
cooksd_out   <- round( cooks.distance( lm_df )*1000, 3 )
ord_cooksd   <- cooksd_out [ order( cooksd_out ) ] 
plot( ord_cooksd , pch="*", cex=2, main="Influential Obs by Cooks distance" )
plot( cooksd_out , pch="*", cex=1, main="Influential Obs by Cooks distance" )

text( x = 1:length( cooksd_out ) + 1, y = cooksd_out, 
      labels = ifelse( cooksd_out > 4 * mean( cooksd_out, na.rm = T ),
                       names( cooksd_out ), ""), col = "red", cex=0.8 ) 


##################################
# Catching / Continue on errors  #   
##################################
# 
for (i in 1:40) {
  tryCatch({

  lavaan::cfa( model, data = ndata_ok[ -c(14, 13, 12, 10,   44, 80, 74, 7, 2), ]  )

  }, error=function(e){ cat(  paste0(i,"ERROR :"),  conditionMessage(e), "\n")} )

}
warnings()
