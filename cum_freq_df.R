


par(mfrow=c(2,4))
table(dat$M1 ); plot( table(dat$M1 ))
table(dat$M15 ); plot( table(dat$M15 ))
table(dat$M2 ); plot( table(dat$M2 ))
table(dat$PR1 ); plot( table(dat$PR1 ))
table(dat$PR15 ); plot( table(dat$PR15 ))
table(dat$PR2 ); plot( table(dat$PR2 ))
table(dat$R1 ); plot( table(dat$R1 ))
table(dat$R2 ); plot( table(dat$R2 ))

plot(table(dat$S,useNA ="ifany"))



par(mfrow=c(1,3))
scatter2D(dat$M1,  dat$M15, colvar =  dat$M2, pch = 16, bty ="n",
          type ="b", xlab="Ποσ.1", ylab="Ποσ.15",
          xlim=c(0,100), ylim=c(0,40),
          colkey = list(side = 1, length = 1),
          clab = c("Ποσ.2"),  add = F
)

scatter2D(dat$PR1*5,  dat$PR15*5, colvar =  dat$PR2*5, pch = 16, bty ="n",
          type ="b", xlab="Στχ.1", ylab="Στχ.15",
          xlim=c(0, 40), ylim=c(0,40),
          colkey = list(side = 1, length = 1),
          clab = c("Στχ.2")
)


scatter2D(dat$R1,  dat$R1==data$R2, colvar = data$R2p, pch = 11, bty ="n",
          xlab="Γκ.1", ylab="Γκ.15",
          colkey = list(side = 1, length = 1),
          clab = c("Γκ.2")
)











# Appendix
cum_freq_df <- function( df, row_id, c( ctgs_ranges )  ) {
  
  if ( is.numeric( data$M1 ) == True ) {
    
    cum_df              <- as.data.frame( table( data$M1 ) )
    cum_df$Var1         <- as.numeric( as.character( cum_df$Var1 ) ) 
    cum_multi           <- cum_df$Var1 * cum_df$Freq
    cum_multi_percent   <- ( cum_multi / sum( cum_multi ) )*100
    cum_df$perc         <- round( cumsum( cum_multi_percent ), 1 )
    
    
  } else { 
    print "Info: The provided df with the given row id is not numeric."}
  
  
  
  
  
  
  
  par(mfrow=c(1,3))
  scatter2D(data$M1,  data$M15, colvar =  data$M2, pch = 16, bty ="n",
            type ="b", xlab="Ποσ.1", ylab="Ποσ.15",
            xlim=c(0,100), ylim=c(0,40),
            colkey = list(side = 1, length = 1),
            clab = c("Ποσ.2"),  add = F
  )
  
  scatter2D(data$PR1p,  data$PR15p, colvar =  data$PR2p, pch = 16, bty ="n",
            type ="b", xlab="Στχ.1", ylab="Στχ.15",
            xlim=c(0, 70), ylim=c(0,30),
            colkey = list(side = 1, length = 1),
            clab = c("Στχ.2")
  )
  
  scatter2D(data$R1p,  data$R1p==data$R2p, colvar = data$R2p, pch = 11, bty ="n",
            xlab="Γκ.1", ylab="Γκ.15",
            colkey = list(side = 1, length = 1),
            clab = c("Γκ.2")
  )
  
  par(mfrow=c(2,2))
  plot(    data$M1, data$M15,  type="p" ,col="red", xlab = "ΠΟΣ.1", ylab = "ΠΟΣ.15")
  plot(    data$M1, data$M2,  type="p" ,col="gray", xlab = "ΠΟΣ.1", ylab = "ΠΟΣ.2")
  plot(    data$M2, data$M15,  type="p" ,col="blue", xlab = "ΠΟΣ.15", ylab = "ΠΟΣ.2")
  
  
  
  
  
  
  # All Regression Equations
  summary( fM1_15_R1N )  #xPR2
  summary( fM15_2_R1N )  #xPR2
  summary( fM1_2_R1N )   #xPR2
  
  summary( fM1_15_R2N ) #ALL
  summary( fM15_2_R2N ) #ALL
  summary( fM1_2_R2N )  #ALL
  
  summary( f1_15_S ) #M1
  summary( f15_2_S ) #M2
  summary( f1_2_S )  #Nothing
  
  
  
  
  
  
  
  
  
  
  
  Appendix
  
  
  # plots
  plot(    data$ID, data$M1,  type="p" ,col="red", xaxt="n" )
  par( new = TRUE )
  plot(   data$ID, data$M15,  type="p" ,col="blue", axes = FALSE, bty = "n", xlab = "", ylab = "")
  axis( side=4, at = pretty(range(data$M15)), col.axis="blue" )
  par( new = TRUE )
  plot(   data$ID,data$M2,  type="p" ,col="gray", axes = FALSE, bty = "n", xlab = "", ylab = "")
  axis( side=4, at = pretty(range(data$M2)), col.axis="gray" )
  
  
  scatter3D(data$M1, data$M15, data$M2,  bty = "b2", colvar = data$M15, 
            col = NULL, pch = 19, cex = 0.5,
            colkey = list(side = 1, length = 0.5),
            cex = 2, ticktype = "detailed")
  scatter3D(data$PR1p, data$PR15p, data$PR2p,  add = TRUE, colkey = list(side = 2, length = 0.5), 
            pch = 19, cex =0.5, col = NULL)
  scatter3D(data$PR1p, data$PR15p, data$PR2p,  bty = "b2", colvar = data$PR15, col = NULL, pch = 19, cex = 0.5)
  
