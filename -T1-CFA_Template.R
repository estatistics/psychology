library(lavaan)
library(semPlot)
library(semTools)
library(MVN)
library(psych)



model_cfa0 <- 
  '
Bbbbbs ~~ Cccccs
Bbbbbs =~ bzz + 1*bxx
Cccccs =~ cvv + cnn + cgg
' 
  

model_srd <- 
  ' 
Relationships   =~ closeness + conflict
Satisfaction    =~ tautonomy + tcompetence + trelatedness
Difficulties    =~ semotional + sconduct + shyper + speer
Relationships   ~~ 1*Relationships
# semotional      ~~ speer
# shyper          ~~ speer

closeness ~~      conflict
tcompetence ~~  trelatedness 

Difficulties             ~ c * Satisfaction   # direct effect
Relationships            ~ a * Satisfaction   # mediator
Difficulties             ~ b * Relationships

indirect                 := a * b             # indirect effect ( a * b )
total                    := c + ( a * b )     # total effect 
'


  # fit indices function 
  # Includes the Chisq/df index too 
  
  # Function
  fit_measures  <- function( fit_x  ) {
    
    fit_indices <- c("chisq", "df", "pvalue", "cfi","tli","ifi","nnfi", "rfi", "mfi",  "rni","gfi",
                     "rmsea", "rmr", "srmr",  "pnfi", "pgfi", "bic", "aic", "ecvi" )
    
    fit_chsq_df          <- fitMeasures( fit_x )[3] / fitMeasures( fit_x )[4]
    names( fit_chsq_df ) <- "chisq/df"
    fits                 <- round(c( fit_chsq_df, fitMeasures( fit_x, fit_indices ) ), 3 )
    
    order_fits           <- c( fit_x@SampleStats@nobs[[1]], fits[2:3], fits[1], fits[4:length(fits )] )
    
    ofits           <- as.data.frame( order_fits)
    colnames(ofits) <- c("value")
    rownames(ofits) <- c( "n_obs", rownames(ofits)[-1] )
    ofits
    
  }
  
  
  
# In order to keep Variables, excluding those that are not more presented in the cfa model
temp_model  <- summary( cfa( model_cfa0 ) )
ops_model   <- unique( temp_model$PE[temp_model$PE$op == "=~",]$rhs )
colns_model <- colnames( ndata_0 )[which( colnames(ndata_0) %in% ops_model ) ]
ndata_ok    <- ndata_0[ c( "ΑΑ", colns_model ) ]

# Estatisticseu Function - To remove outliers or at least to try!
outdata_res <- outlier_remove( ndata_ok, "ΑΑ", cook_mahal=1, sensitivity = 0, cut_off = 0, 1 )
out_df      <- outdata_res[[1]]





model <- model_cfa0
cfa_fit <- lavaan::cfa( model, std.lv = TRUE, data = data_cfa )
summary( cfa_fit, standardized = TRUE,  rsquare = TRUE )
standardizedSolution( cfa_fit, type = "std.all" )

# Modification and Fit Indices
dm <- modindices( cfa_fit )[modindices( cfa_fit )$op == "~~" & modindices( cfa_fit )$mi >= 1 ,]
head( dm[ order( dm$mi, decreasing = T ), ], 7 )


# Transforming model implied covariances to correlations
cfa_cov <- fitted( cfa_fit )$cov
cfa_cor <- cov2cor( cfa_cov )
#Residual correlations
residuals( cfa_fit, type = "cor" )$cor

# Fit indices
fit_measures( cfa_fit, fit_indices )

# Plotting the model graphically
semPaths( cfa_fit, what = "std",     layout = "tree2",   mar  =  c(2, 5, 2, 11),
          sizeMan = 8,               sizeMan2 = 2,       asize = 3,
          curvature = 9,             curveAdjacent = 1,  edge.width = 0.6, node.width = 2.0,
          edge.label.cex = 0.7,      label.cex = 1.7,    label.prop = 0.50,
          edge.label.position = 0.5, fade = F,           nCharNodes = 100, rotation = 2 )

